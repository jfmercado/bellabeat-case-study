# Install the packages
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('data.table')

# Load the packages
library(tidyverse)
library(janitor)
library(data.table)
library(tools)


# Set working directory
setwd("/Users/joshuamercado/R Working Directory/bellabeat_case_study/")

# Import the data into a list of data tables
path <- "FitBit_Fitness_Tracker_Data/mturkfitbit_export_3.12.16-4.11.16"
csv_files <- list.files(path = path,
                        pattern = "*.csv$",
                        recursive = TRUE,
                        full.names = TRUE)
unclean_data_tables1 <- map(csv_files, fread) %>% 
  set_names(file_path_sans_ext(basename(csv_files)))

path <- "FitBit_Fitness_Tracker_Data/mturkfitbit_export_4.12.16-5.12.16"
csv_files <- list.files(path = path,
                        pattern = "*.csv$",
                        recursive = TRUE,
                        full.names = TRUE)
unclean_data_tables2 <- map(csv_files, fread) %>% 
  set_names(file_path_sans_ext(basename(csv_files)))

# Explore data

# This is a lot of data in 29 total data tables, so our first step will be to simply remove any data tables aren't of use to us. The minute tables are far too granular, so we can get rid of those.

# We'll use the more efficient negative indices rather than searching for substrings
unclean_data_tables1 <- unclean_data_tables1[-c(6:10)]
unclean_data_tables2 <- unclean_data_tables2[-c(9:16)]

# Get all unique names from both lists
table_names <- unique(c(names(unclean_data_tables1), names(unclean_data_tables2)))

# Combine the lists
unclean_data_tables <- lapply(table_names, function(name) {
  if (name %in% names(unclean_data_tables1) && name %in% names(unclean_data_tables2)) {
    # If the table is in both lists, perform an inner join on Id
    rbindlist(list(unclean_data_tables1[[name]], unclean_data_tables2[[name]]), use.names = TRUE, fill = TRUE)
  } else {
    # If the table is only in one of the lists (like "sleepDay_merged"), use it as is
    unclean_data_tables2[[name]]
  }
})

# Name the elements of the combined list
names(unclean_data_tables) <- table_names

# Now that we have a single list of data tables, we can clean the individual tables and further explore.

# Now we'll clean the column names for consistency by converting to snake case
dt_list <- lapply(unclean_data_tables, clean_names)

# Remove any instance of "total" in column names as total is implied and change "step" to "steps"
dt_list <- lapply(dt_list, function(dt) {
  setnames(dt, names(dt), gsub("^total_|_total$|_total_", "", names(dt)))
  setnames(dt, names(dt), gsub("^step$", "steps", names(dt)))
})

# First we need to create consistent date column names for a merge - this is easy because every date column is the second column
lapply(dt_list, function(dt) {
  setnames(dt, 2, "date", skip_absent = TRUE)
})

# Now we can see how many unique users and days logged each has as a gauge of what we can merge
lapply(dt_list, function(dt) {
  dt[, .(unique_dates = uniqueN(date)), by = id]
})

# All of the data tables have at least 30 unique users (desired minimum sample size) besides heart rate, sleep, and weight. Sleep is close at 24, so we'll keep it in for practice.
# Heart rate and weight only are exceptionally low. Heart rate is too granular for analysis anyways. Weight only has 3 out of 13 users that recorded more than 4 weights, so we'll exclude those from our analysis.
dt_list <- dt_list[-c(2, 6)]

# It looks like our data is generally robust, at least for demonstration purposes). Now we can further clean our data.

# Check for duplicate rows
lapply(dt_list, function(dt) {
  sum(duplicated(dt) | duplicated(dt, fromLast = TRUE))
})

# Remove duplicate rows
dt_list <- lapply(dt_list, unique)

# Check for NA values
lapply(dt_list, function(dt) {
  colSums(is.na(dt))
})

# No NA values

# The daily activity table already contains information about calories, intensities, and steps. We need to check to see if they match

# Helper function
compare_tables <- function(dt_list, x_index, y_index, match_cols) {
  
  # Count the total rows in the smaller data table
  total_rows_y <- nrow(dt_list[[y_index]])
  
  # Find the matching rows and calculate the percentage of the smaller data table that matches 
  matching_rows <- dt_list[[x_index]][dt_list[[y_index]], on = match_cols, nomatch = 0L]
  num_matching_rows <- nrow(matching_rows)
  percentage <- (num_matching_rows / total_rows_y) * 100
  
  list(
    total_rows_y = total_rows_y,
    matching_rows = num_matching_rows,
    percentage = percentage
  )
}

# Compare the tables
compare_tables(dt_list, 1, 5, c("id", "date", "calories"))
compare_tables(dt_list, 1, 6, c("id", "date", c(names(dt_list$dailyIntensities_merged))))
compare_tables(dt_list, 1, 7, c("id", "date", "steps"))

# All of the data is redundant so we can remove those tables
dt_list <- dt_list[-c(5:7)]

# The last step before merging is to convert dates to consistent formats. This will be easier by splitting the data table list into daily and hourly lists.
daily_list <- dt_list[c(1, 5)]
hourly_list <- dt_list[2:4]

daily_list <- lapply(daily_list, function(dt) {
  dt[, date := parse_date_time(date, orders = c("mdy", "mdy HMS p"))]
}) # We need the more flexible parse_date_time() function to handle multiple formats

hourly_list <- lapply(hourly_list, function(dt) {
  dt[, date := mdy_hms(date)]
})

# Now we can finally merge our daily and hourly data tables based on id and date
daily_activity <- Reduce(function(x, y) merge(x, y, all.x = TRUE, by = c("id", "date")), daily_list) # We want to use a left join because we can perform analysis without sleep data, but not without activity data
hourly_activity <- Reduce(function(x, y) merge(x, y, all = TRUE, by = c("id", "date")), hourly_list) # We want an outer join because all of the data is equally important for analysis

summary(daily_activity)
summary(hourly_activity)

# We know that steps and calories should probably not be zero or even close to zero, so let's remove any rows below 100 steps or 1000 calories as those were probably days that the wearable was not worn for the majority of the day.
daily_activity <- daily_activity[!(steps < 100 | calories < 1000)]

# Make sure the new minimums are above the threshold.
summary(daily_activity[, .(steps, calories)])

# Let's create separate weekday, date, and hour columns for more granular analysis

# Preserve the datetime column
#setnames(hourly_activity, "date", "datetime")
hourly_activity[, datetime := format(date, "%Y-%m-%d %H:%M:%S")] # Convert to string for easier parsing by Excel and Tableau
hourly_activity[, hour := hour(date)]
hourly_activity[, date := date(date)]
hourly_activity[, weekday := weekdays(date)]

# Create a weekday column
daily_activity[, weekday := weekdays(date)]

# Make sure the days of the week are ordered correctly
weekday_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

hourly_activity$weekday <- factor(hourly_activity$weekday, levels = weekday_order)
daily_activity$weekday <- factor(daily_activity$weekday, levels = weekday_order)

# Create a % of day tracked by dividing the total minutes tracked by a maximum 1440 minutes in a day
daily_activity[, percent_tracked := (very_active_minutes + fairly_active_minutes + lightly_active_minutes + sedentary_minutes) / 1440]

# Create a sleep quality score which is a percentage of total time in bed that the user is asleep
daily_activity$sleep_score <- daily_activity$minutes_asleep / daily_activity$time_in_bed

# Create a daily intensity score that takes into account all active minutes. The American Heart Association recommends 150 minutes of moderate intensity or 75 minutes of vigorous intensity per week. Therefore we'll assign 1 point for lightly active minutes, 2 points for fairly active minutes, and 4 points for very active minutes.
daily_activity$intensity_score <- daily_activity$very_active_minutes * 2 + daily_activity$fairly_active_minutes + daily_activity$lightly_active_minutes * 0.5

# We know that while intensity scores can be low, they certainly shouldn't be 0.
daily_activity <- daily_activity[!(intensity_score < 1)]

# We can create a classification of the day's activity level based on steps. We'll get our category ranges from the Tudor-Locke and Bassett classification system.
steps_breaks <- c(0, 5000, 7500, 10000, 12500, max(daily_activity[,steps]))
steps_labels <- c("Sedentary", "Physically Inactive", "Moderately Active", "Physically Active", "Very Active")

daily_activity[, daily_activity_category := cut(steps,
                                       breaks = steps_breaks,
                                       labels = steps_labels,
                                       include.lowest = TRUE)]

# Make sure that the categories are ordered correctly
daily_activity$daily_activity_category <- factor(daily_activity$daily_activity_category, levels = steps_labels)

# # And finally let's create *usage* categories dividing users by how many days they logged activity i.e. used their device
# 
# # Find the frequency of each unique date, grouped by id
# usage_categories <- daily_activity[, .(frequency = uniqueN(date)), by = id]
# 
# # Create custom breaks and labels
# summary(usage_categories)
# usage_breaks <- c(0, 38, 39, 42, 62) # max possible 62 days of logging
# usage_labels <- c("Low Usage", "Medium Low Usage", "Medium High Usage", "High Usage")
# 
# # Create the categories
# usage_categories[, usage_category := cut(frequency,
#                                          breaks = usage_breaks,
#                                          labels = usage_labels,
#                                          include.lowest = TRUE)]
# 
# # Merge usage categorization data table with id_avgs
# daily_activity <- merge(daily_activity, usage_categories[, .(id, usage_category)], by = "id", all = TRUE)

# Summary Statistics

summary(daily_activity[, .(steps,
                       very_active_minutes,
                       fairly_active_minutes,
                       lightly_active_minutes,
                       sedentary_minutes,
                       calories,
                       minutes_asleep,
                       percent_tracked,
                       sleep_score,
                       intensity_score,
                       daily_activity_category)])

# Average steps is 8302, below the recommended amount of 10000
# Average intensity score is 167.6 which is far above the recommended amount of 22
# Average sedentary time is 958 minutes or 15.97 hours per day, but the max is 1440 minutes, so that must include time in bed. We unfortunately don't have enough sleep data to accurately determine how much of the sedentary minutes occurred while the user was awake.
# We can see that most users use their device for over 83% of the day which means we have fairly robust data
# Users sleep 419 minutes or just about 7 hours on average
# The National Sleep institute recommends 7-9 hours of sleep, so most users are hitting the minimum, which is better than over a third of Americans

# Find averages grouped by weekday, ID, and hour
numeric_avg_cols <- names(daily_activity)[sapply(daily_activity, is.numeric)] # Create a list of column names that we want to average
numeric_avg_cols <- numeric_avg_cols[-1] # We don't need to find the averages of the ID column

# Grouped by weekday
weekday_avgs <- daily_activity[order(weekday), lapply(.SD, mean, na.rm = TRUE), by = weekday, .SDcols = numeric_avg_cols]

# Grouped by id
id_avgs <- daily_activity[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = numeric_avg_cols]

# Grouped by hour
hourly_avg_cols <- c("calories", "intensity", "steps")
hourly_avgs <- hourly_activity[, lapply(.SD, mean, na.rm = TRUE), by = hour, .SDcols = hourly_avg_cols]

# We can apply our steps categorization to participants average scores rather than their daily scores
id_avgs[, user_category := cut(steps,
                               breaks = steps_breaks,
                               labels = steps_labels,
                               include.lowest = TRUE)]

# Make sure that the categories are ordered correctly
id_avgs$user_category <- factor(id_avgs$user_category, levels = steps_labels)

# Merge our user category with the activity tables
daily_activity <- merge(daily_activity, id_avgs[, .(id, user_category)], by = "id")
hourly_activity <- merge(hourly_activity, id_avgs[, .(id, user_category)], by = "id")

# # Merge usage categorization data table with id_avgs
# id_avgs <- merge(id_avgs, usage_categories[, .(id, usage_category)], by = "id", all = TRUE)

summary(id_avgs)

# Final Checks

# Check that we didn't create any unwanted NA values (besides sleep data)
colSums(is.na(daily_activity))
colSums(is.na(hourly_activity))

print(weekday_avgs[, .(weekday,
                       steps,
                       very_active_minutes,
                       fairly_active_minutes,
                       lightly_active_minutes,
                       sedentary_minutes,
                       calories,
                       minutes_asleep,
                       percent_tracked,
                       sleep_score,
                       intensity_score)])

# Now we can visualize this data

# General plots

# Plot categories on a pie graph
ggplot(daily_activity, aes(x = "", fill = user_category)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  theme_void()

# Plot total steps vs. calories
ggplot(daily_activity, aes(x = steps, y = calories)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title="Total Steps vs. Calories")

cor(daily_activity$steps, daily_activity$calories)
# 0.5476929 correlation - Unsurprisingly, there is a positive correlation

# Plot time asleep vs. total time in bed
ggplot(daily_activity, aes(x = minutes_asleep, y = time_in_bed)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title="Total Minutes Asleep vs. Total Time in Bed")

cor(daily_activity$minutes_asleep, daily_activity$time_in_bed, use = "complete.obs")
# 09302564 correlation - Another unsurprisingly (and extremely) positive correlation

# Plot time asleep vs. sedentary minutes
ggplot(daily_activity, aes(x = minutes_asleep, y = sedentary_minutes)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Minutes Asleep vs. Sedentary Minutes")

cor(daily_activity$minutes_asleep, daily_activity$sedentary_minutes, use = "complete.obs")
# Despite our assumption that sedentary minutes included minutes asleep, we can still see a strong negative correlation. We would expect the opposite, which means the negative correlation is probably even stronger.

# In order to control for time in bed, we check the relationship between sleep score and sedentary minutes
ggplot(daily_activity, aes(x = sleep_score, y = sedentary_minutes)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Sleep Score vs. Sedentary Minutes")

cor(daily_activity$sleep_score, daily_activity$sedentary_minutes, use = "complete.obs")
# Rather than trying to reduce sedentary minutes to improve their sleep, users would probably be better served by increasing their total time in bed, either by going to bed earlier or sleeping in later

# And then to confirm once more, we'll check minutes asleep vs. intensity score, which should have a positive correlation
ggplot(daily_activity, aes(x = minutes_asleep, y = intensity_score)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Minutes Asleep vs. Intensity Score")

cor(daily_activity$minutes_asleep, daily_activity$intensity_score, use = "complete.obs")
# This is a great example of how correlation doesn't necessarily equal causation. Lack of intensity may be correlated with less sleep, but higher intensity has little (and even) correlation with more sleep. 

# Now we can plot our average data (calories, steps, intensity, and sleep) against weekday, user, and hour

# Data vs. Weekday

# Calories burned per day
ggplot(weekday_avgs, aes(x = weekday, y = calories)) + 
  geom_histogram(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Average Calories per Weekday",
       x = "Weekday",
       y = "Average Calories")

# Steps per day
ggplot(weekday_avgs, aes(x = weekday, y = steps)) +
  geom_histogram(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 10000) + # recommended steps
  theme_minimal() +
  labs(title = "Average Steps per Weekday",
       x = "Weekday",
       y = "Average Steps")

# Hours slept per day
ggplot(weekday_avgs, aes(x = weekday, y = minutes_asleep)) + 
  geom_histogram(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 480) + # recommended sleep
  theme_minimal() +
  labs(title = "Average Sleep per Weekday",
       x = "Weekday",
       y = "Average Sleep")
# We can see that none of the averages quite reach the recommended 480 minutes of sleep

# Intensity per day
ggplot(weekday_avgs, aes(x = weekday, y = intensity_score)) + 
  geom_histogram(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 150/7) + # recommended intensity
  geom_hline(yintercept = 300/7, linetype = "dashed") + # very active
  theme_minimal() +
  labs(title = "Average Intensity Minutes per Weekday",
       x = "Weekday",
       y = "Average Intensity")

# Data vs. Hour

# Calories per hour
ggplot(hourly_avgs, aes(x = hour, y = calories)) +
  geom_histogram(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Average Calories per Hour",
       x = "Hour",
       y = "Average Calories")

# Intensity per hour
ggplot(hourly_avgs, aes(x = hour, y = intensity)) + 
  geom_histogram(stat = "identity", fill = "skyblue") +
  theme_minimal() + 
  labs(title = "Average Intensity per Hour",
       x = "Hour",
       y = "Average Intensity")
# People are most active immediately after work
# Bella Beat App can send a notification to exercise

# Steps per hour
ggplot(hourly_avgs, aes(x = hour, y = steps)) + 
  geom_histogram(stat = "identity", fill = "skyblue") +
  theme_minimal() + 
  labs(title = "Average  per Hour",
       x = "Hour",
       y = "Average Intensity")

# We can plot multiple columns against time by melting our data to long format. The variation between weekdays wasn't as pronounced as between hours, so let's try it with our hourly average data.
hourly_avgs_long <- pivot_longer(hourly_avgs,
                                 cols = c(calories, steps, intensity),
                                 names_to = "metric",
                                 values_to = "value")

ggplot(hourly_avgs_long, aes(x = hour, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ metric, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(title = "Steps and Calories by Hour", x = "Hour", y = "Value") +
  scale_fill_manual(values=c("steelblue", "orange", "red"))

cor(hourly_avgs[, .(steps, calories, intensity)])
# We can clearly see an almost perfect correlation between all 3, with values lowest in the early morning, rising steadily towards midday, with an early evening spike.
# Calories remains relatively higher overnight because we continue to burn calories while we sleep whereas steps and intensity should be minimal.

# Reshape the data from wide to long format
daily_activity_intensity_long <- pivot_longer(daily_activity,
                                     cols = c(lightly_active_minutes, fairly_active_minutes, very_active_minutes),
                                     names_to = "activity_type",
                                     values_to = "minutes")

# Make sure the intensity levels are ordered correctly
daily_activity_intensity_long$activity_type <- factor(daily_activity_intensity_long$activity_type, levels = c("lightly_active_minutes", "fairly_active_minutes", "very_active_minutes"))

# Create the plot
ggplot(daily_activity_intensity_long, aes(x = minutes, y = calories, color = activity_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ factor(activity_type, levels = c("very_active_minutes", "fairly_active_minutes", "lightly_active_minutes")), scales = "free_x", ncol = 1) +
  labs(title = "Calories vs Activity Minutes",
       x = "Minutes",
       y = "Calories",
       color = "Activity Type") +
  theme_minimal() +
  scale_color_manual(values = c("orange", "green", "blue"),
                     labels = c("Very Active", "Fairly Active", "Lightly Active"))

# Calculate correlations
cor(daily_activity[, .(calories, lightly_active_minutes, fairly_active_minutes, very_active_minutes)], use = "complete.obs")

# We can visualize multiple plots even more powerfully with Tableau, so let's write our data to csv files and transfer them to Tableau.

#Export to CSV
write.csv(daily_activity, file = 'fitbit_daily_activity_03122016_05122016.csv')
write.csv(hourly_activity, file = 'fitbit_hourly_activity_03122016_05122016.csv')
write.csv(daily_activity_intensity_long, file = 'fitbit_daily_activity_intensity_long_03122016_05122016.csv')

# Data vs. User Category

# # Plot calories vs. user category
# ggplot(daily_activity, aes(user_category, minutes_asleep, fill = user_category)) +
#   geom_boxplot() +
#   theme(legend.position="none") +
#   geom_hline(yintercept = 480) +
#   labs(title="Calories by User type") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# 
# ggplot(id_avgs, aes(usage_category, steps, fill = usage_category)) +
#   geom_boxplot() +
#   theme(legend.position="none") +
#   labs(title="Calories by User type") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))