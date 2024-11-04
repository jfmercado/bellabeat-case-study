# Environment Set-Up

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
setwd("/Users/joshuamercado/projects/bellabeat-case-study")

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

# Data Cleaning

# There is a lot of data in 29 total data tables, so our first step will be to simply remove any data tables aren't of use to us. The minute tables are far too granular, so we can get rid of those.

# We'll use the more efficient negative indices rather than searching for substrings
unclean_data_tables1 <- unclean_data_tables1[-c(6:10)]
unclean_data_tables2 <- unclean_data_tables2[-c(9:16)]

# Now that we have 2 lists of the same data (except for sleep data) from different time periods, we can combine based on their shared column names.

# Get all unique names from both lists
table_names <- unique(c(names(unclean_data_tables1), names(unclean_data_tables2)))

# Combine the lists
unclean_data_tables <- lapply(table_names, function(name) {
  if (name %in% names(unclean_data_tables1) && name %in% names(unclean_data_tables2)) {
    # If the table is in both lists, join on matching column names
    rbindlist(list(unclean_data_tables1[[name]], unclean_data_tables2[[name]]), use.names = TRUE, fill = TRUE)
  } else {
    # If the table is only in one of the lists (like "sleepDay_merged"), use it as is
    unclean_data_tables2[[name]]
  }
})

# Name the elements of the combined list
names(unclean_data_tables) <- table_names


# Now that we have a single list of data tables, we can clean the individual tables and further explore.

# Clean the column names for consistency by converting to snake case
dt_list <- lapply(unclean_data_tables, clean_names)

# Remove any instance of "total" in column names as total is implied and change "step" to "steps"
dt_list <- lapply(dt_list, function(dt) {
  setnames(dt, names(dt), gsub("^total_|_total$|_total_", "", names(dt)))
  setnames(dt, names(dt), gsub("^step$", "steps", names(dt)))
})

# We need to create consistent date column names for a merge - this is easy because every date column is the second column
lapply(dt_list, function(dt) {
  setnames(dt, 2, "date", skip_absent = TRUE)
})

# Now we can see how many unique users and days logged each data table has to ensure we have a large enough sample size
lapply(dt_list, function(dt) {
  dt[, .(unique_dates = uniqueN(date)), by = id]
})

# All of the data tables have at least 30 unique users (desired minimum sample size) besides heart rate, sleep, and weight. Sleep is close at 24, so we'll keep it in for practice. Heart rate and weight are exceptionally low, so we'll exclude those from our analysis.

dt_list <- dt_list[-c(2, 6)]

# Next, we'll perform some checks.

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


# The daily activity table already contains information about calories, intensities, and steps. We need to check to see if it'those individual tables are redundant.

# Helper function
compare_tables <- function(dt_list, x_index, y_index, match_cols) {
  
  # Count the total rows in the smaller data table
  total_rows_y <- nrow(dt_list[[y_index]])
  
  # Find the matching rows and calculate the percentage of the smaller data table that matches 
  matching_rows <- dt_list[[x_index]][dt_list[[y_index]], on = match_cols, nomatch = 0L]
  num_matching_rows <- nrow(matching_rows)
  percentage <- (num_matching_rows / total_rows_y)# 100
  
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

# Now we can finally extract our daily and hourly data tables from the lists based on id and date
daily_activity <- Reduce(function(x, y) merge(x, y, all.x = TRUE, by = c("id", "date")), daily_list) # We want to use a left join here because we want the sleep data
hourly_activity <- Reduce(function(x, y) merge(x, y, all = TRUE, by = c("id", "date")), hourly_list) # We want an outer join here because all of the data is equally important for analysis

# Check our work
summary(daily_activity)
summary(hourly_activity)

# We know that steps and calories should probably not be zero or even close to zero, so let's remove any rows below 100 steps or 1000 calories as those were probably days that the wearable was not worn for the majority of the day.
daily_activity <- daily_activity[!(steps < 100 | calories < 1000)]

# Data Transformation

# We'll create new columns for more granular analysis.

# Weekday, date, and hour columns
hourly_activity[, datetime := format(date, "%Y-%m-%d %H:%M:%S")] # Convert to string for easier parsing by Excel and Tableau
hourly_activity[, hour := hour(date)]
hourly_activity[, date := date(date)]
hourly_activity[, weekday := weekdays(date)]

daily_activity[, weekday := weekdays(date)]

# Make sure the days of the week are ordered correctly
weekday_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

hourly_activity$weekday <- factor(hourly_activity$weekday, levels = weekday_order)
daily_activity$weekday <- factor(daily_activity$weekday, levels = weekday_order)

# Percent of day tracked by dividing the total minutes tracked by a maximum 1440 minutes in a day
daily_activity[, percent_tracked := (very_active_minutes + fairly_active_minutes + lightly_active_minutes + sedentary_minutes) / 1440]

# Sleep quality score which is a percentage of total time in bed that the user is asleep
daily_activity$sleep_score <- daily_activity$minutes_asleep / daily_activity$time_in_bed

# Daily intensity score that takes into account all active minutes. The American Heart Association recommends 150 minutes of moderate intensity or 75 minutes of vigorous intensity per week. Therefore we'll assign .5 points for lightly active minutes, 1 points for fairly active minutes, and 2 points for very active minutes.
daily_activity$intensity_score <- daily_activity$very_active_minutes# 2 + daily_activity$fairly_active_minutes + daily_activity$lightly_active_minutes# 0.5

# We can create a classification of the day's activity level based on steps. We'll get our category ranges from the Tudor-Locke and Bassett classification system.

steps_breaks <- c(0, 5000, 7500, 10000, 12500, max(daily_activity[,steps]))
steps_labels <- c("Sedentary", "Physically Inactive", "Moderately Active", "Physically Active", "Very Active")

daily_activity[, daily_activity_category := cut(steps,
                                                breaks = steps_breaks,
                                                labels = steps_labels,
                                                include.lowest = TRUE)]

# Make sure that the categories are ordered correctly
daily_activity$daily_activity_category <- factor(daily_activity$daily_activity_category, levels = steps_labels)

# Next, we'll create new data tables with the averages of the metrics grouped by weekday, ID, and hour

# Create a list of column names that we want to average
numeric_avg_cols <- names(daily_activity)[sapply(daily_activity, is.numeric)]
numeric_avg_cols <- numeric_avg_cols[-1] # We don't need to find the averages of the ID column

# Grouped by weekday
weekday_avgs <- daily_activity[order(weekday), lapply(.SD, mean, na.rm = TRUE), by = weekday, .SDcols = numeric_avg_cols]

# Grouped by id
id_avgs <- daily_activity[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = numeric_avg_cols]

# Grouped by hour
hourly_avg_cols <- c("calories", "intensity", "steps")
hourly_avgs <- hourly_activity[, lapply(.SD, mean, na.rm = TRUE), by = hour, .SDcols = hourly_avg_cols]

# We can apply our steps categorization to users' average scores rather than their daily scores
id_avgs[, user_category := cut(steps,
                               breaks = steps_breaks,
                               labels = steps_labels,
                               include.lowest = TRUE)]

# Make sure that the categories are ordered correctly
id_avgs$user_category <- factor(id_avgs$user_category, levels = steps_labels)

# Merge our user category with the activity tables
daily_activity <- merge(daily_activity, id_avgs[, .(id, user_category)], by = "id")
hourly_activity <- merge(hourly_activity, id_avgs[, .(id, user_category)], by = "id")

# Check that we didn't create any unwanted NA values (besides sleep data)
colSums(is.na(daily_activity))
colSums(is.na(hourly_activity))

# Data Analysis

# Summary Statistics
summary(daily_activity[, .(steps,
                           very_active_minutes,
                           fairly_active_minutes,
                           lightly_active_minutes,
                           sedentary_minutes,
                           calories,
                           minutes_asleep,
                           time_in_bed,
                           percent_tracked,
                           sleep_score,
                           intensity_score,
                           user_category)])

# Average steps is 8302, below the recommended amount of 10000, but well above the American average of 3000 - 4000 (according to the Mayo Clinic).
# Average intensity score is 167.6 which is far above the recommended amount of 22.
# Average sedentary time is 958 minutes or 15.97 hours per day, but the max is 1440 minutes, so we assume that sedentary time includes time in bed.
# We unfortunately don't have enough sleep data to accurately determine how much of the sedentary minutes occurred while the user was awake.
# We can see that most users use their device for over 83%. 100% would be better, but we can still glean insight.
# Users sleep 419 minutes or just about 7 hours on average.
# The National Sleep institute recommends 7-9 hours of sleep, so most users are hitting the minimum, which is better than over a third of Americans.
# A WebMD survey (one of the few that distuingishes time asleep from time in bed) found that on average, Americans spend only 5.7 hours asleep while spending 7.67 hours in bed (our users' averages are 6.98 hours alsleep and 7.63 hours in bed).

summary(hourly_activity)

# The range between the minimum and 1st quartile of calorie burn is 20, while the range between the 3rd quartile and the maxiumum is 842.
# There is a similar result in intensity minutes and steps.
# There are relatively very few hours of high calorie burn, lots of intensity minutes, and high step count, which makes sense thinking about the average person's day. One can only exercise for so many hours in the day.
# (Hourly) intensity is measured in minutes, but we don't have data on the level of intensity for those minutes.

# Next, we'll try and find any relationships between metrics.

paste("Steps vs. Calories R-Value:", cor(daily_activity$steps, daily_activity$calories))
paste("Minutes Asleep vs. Time in Bed R-Value:", cor(daily_activity$minutes_asleep, daily_activity$time_in_bed, use = "complete.obs"))
paste("Minutes Asleep vs. Sedentary Minutes R-Value:", cor(daily_activity$minutes_asleep, daily_activity$sedentary_minutes, use = "complete.obs"))

# Our assumption that sedentary minutes includes minutes asleep means that we'd expect a strong positive correlation and, in fact, we see a strong negative correlation. This data tells us that the more sedentary minutes a user has in the day, the more minutes they spend sleeping.

# In order to control for time in bed, we check the relationship between sleep score and sedentary minutes
paste("Sleep Score vs. Sedentary Minutes R-Value:",cor(daily_activity$sleep_score, daily_activity$sedentary_minutes, use = "complete.obs"))

# And then to confirm once more, we'll check minutes asleep vs. intensity score, which is the inverse of sedentary minutes
paste("Minutes Asleep vs. Intensity Score R-Value:",cor(daily_activity$minutes_asleep, daily_activity$intensity_score, use = "complete.obs"))


# This is a great example of how correlation doesn't necessarily equal causation. Sedentary minutes may be correlated with less sleep, but sedentary minutes is not correlated with *how well# one sleeps. Inversely, higher intensity throughout the day has little correlation with less sleep. Rather than trying to reduce sedentary minutes to improve their sleep, users would probably be better served by increasing their total time in bed, either by going to bed earlier or sleeping in later.

# Saturday has the highest average steps, fairly active minutes, lightly active minutes, calories burned, and intensity score.
# This makes sense considering Saturdays involve lots of free time without work, but also without the pressure to prepare for the week/rest on Sunday.
# Tuesday has the highest average very active minutes and is a close second behind Saturday in all of the metrics that Saturday leads in.
# Tuesdays might seem like an odd day for lots of exercise, but in the context of Monday being consistently the third highest average day for steps, activity, and calorie burn, I'd guess that people try and start their week off on a positive note with some exercise, similar to the gym sign-up phenomenon after New Years.
# Sunday consistently has the lowest average metrics of activity, which makes sense given the concept of Sunday being a day of rest.
# Sunday also has the highest time in bed and time asleep.
# We can infer that people want to get a good night's sleep before the week ahead and there usually aren't many activities to do on Sunday nights.
# Sunday also has the lowest average sleep score meaning that users may increase their time in bed, but there is not an equivalent increase in time asleep.
# This means that there is an inflection point where beyond it, users shouldn't try and spend more time in bed and expect continual gains in their time asleep.

# We can perform a similar analysis on hourly averages, but with 24 hours in the day, it might be easier to glean insight from vizualizations.

# Data Visualization

# General plots
# These plots include the correlation tests we conducted in the Analyze phase.

# Plot categories on a pie graph
ggplot(daily_activity, aes(x = "", fill = user_category)) + 
    geom_bar(width = 1) +
    coord_polar("y", start = 0) +
    theme_void()

# Plot total steps vs. calories
ggplot(daily_activity, aes(x = steps, y = calories)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title="Daily Steps vs. Calories")

# Plot time asleep vs. total time in bed
ggplot(daily_activity, aes(x = minutes_asleep, y = time_in_bed)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title="Minutes Asleep vs. Time in Bed")

# Plot time asleep vs. sedentary minutes
ggplot(daily_activity, aes(x = minutes_asleep, y = sedentary_minutes)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Minutes Asleep vs. Sedentary Minutes")

# In order to control for time in bed, we check the relationship between sleep score and sedentary minutes
ggplot(daily_activity, aes(x = sleep_score, y = sedentary_minutes)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Sleep Score vs. Sedentary Minutes")

# And then to confirm once more, we'll check minutes asleep vs. intensity score
ggplot(daily_activity, aes(x = minutes_asleep, y = intensity_score)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Minutes Asleep vs. Intensity Score")

# Average Data vs. Weekday

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

# Intensity per day
ggplot(weekday_avgs, aes(x = weekday, y = intensity_score)) + 
  geom_histogram(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 150/7) + # recommended intensity
  geom_hline(yintercept = 300/7, linetype = "dashed") + # very active
  theme_minimal() +
  labs(title = "Average Sleep per Weekday",
       x = "Weekday",
       y = "Average Intensity")

# We can see that none of the averages quite reach the recommended 10000 steps or 480 minutes of sleep per day, yet the daily recommendation of ~22 minutes of intense minutes is easily cleared on each weekday. Even the above-and-beyond recommendation of ~44 minutes per day, is about a third of what each user averages per day. This tells us that either the recommendation is too low or FitBit defines active minutes differently than the American Heart Association.

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

# Steps per hour
ggplot(hourly_avgs, aes(x = hour, y = steps)) + 
  geom_histogram(stat = "identity", fill = "skyblue") +
  theme_minimal() + 
  labs(title = "Average Steps per Hour",
       x = "Hour",
       y = "Average Steps")

# All three metrics reach their minimums overnight, but while intensity and steps bottom out near zero, calories still remains at a moderate level.
# Our bodies obviously don't take steps or perform activity while we sleep overnight, but they do maintain a basal metabolic rate of calorie burn even when we're sleeping.
# Users are most active immediately around 6pm and 7pm (after work).
# The second most active time of day is the early afternoon (lunch time).
# We can infer that most of the users work full-time jobs.

# We can plot multiple columns against time by pivoting our data to long format. The variation between weekdays wasn't as pronounced as between hours, so let's try it with our hourly average data.

# Pivot to long data
hourly_avgs_long <- pivot_longer(hourly_avgs,
                                 cols = c(calories, steps, intensity),
                                 names_to = "metric",
                                 values_to = "value")

# Plot 
ggplot(hourly_avgs_long, aes(x = hour, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ metric, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(title = "Calories, Intensity Minutes, and Steps by Hour", x = "Hour", y = "Value")

# Confirm correlations
cor(hourly_avgs[, .(steps, calories, intensity)])


# We can clearly see an almost perfect correlation between all 3, with values lowest in the early morning, rising steadily towards midday, with an early evening spike. Calories remain relatively higher overnight because we continue to burn calories while we sleep whereas steps and intensity should be minimal.

# Next we'll plot the different activity intensity levels against calories using the same pivoting method.

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
  theme_minimal()

# Calculate correlations
cor(daily_activity[, .(calories, lightly_active_minutes, fairly_active_minutes, very_active_minutes)], use = "complete.obs")

# We can that all active minutes are positively correlated with calories, with correlations strengthening as the intensity of thoe minutes increases.

# Export Data
# We can visualize multiple plots even more powerfully with Tableau, so let's write our data to csv files and transfer them to Tableau.

#Export to CSV
write.csv(daily_activity, file = 'fitbit_daily_activity_03122016_05122016.csv')
write.csv(hourly_activity, file = 'fitbit_hourly_activity_03122016_05122016.csv')
write.csv(daily_activity_intensity_long, file = 'fitbit_daily_activity_intensity_long_03122016_05122016.csv')