# Introduction

This sample case study was provided through the Google Data Analytics Professional Certificate course.

### Valuable things I learned while working with this data:
* Data tables vs. data frames - more efficient functions and data table notation resembles SQL Selects.
* Data permaneance - learning to make sure that any transformations made didn't cause any unintended consequences.
* Date formats - learning to work with POSIX functions as well as transforming dates into strings to be read more easily by Excel and Tableau.

### Data Limitations:
* Unless the FitBit was worn all day (usually not the case), the daily data will be at least slightly skewed.
* Our sleep data only had 24 users, most of whom recorded their sleep on less than 30 days (Kept it in the analysis for practice/demonstration purposes).
* We had to completely scrap our weight data which only had 13 users, only 3 of which logged their weight more than 4 times.
* We did not have age, gender, height, or weight data, all of which are extremely deterministic when it comes to health and activity.
* One of the metrics we used the most was active minute intensity (lightly, fairly, and very). When measured against the American Heart Association's recommendation of 150 weekly active minutes, every user went way beyond, leading us to question FitBit's definition of active minute intensity.

# 1. Ask

### Scenario
You are a junior data analyst working on the marketing analyst team at Bellabeat, a high-tech manufacturer of health-focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opporunities for the company. You have been asked to focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. The insights you discover will then help guide marketing strategy for the company. You will present your analysis to the Bellabeat executive team along with your high-level recommendations for Bellabeat’s marketing strategy.

Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that manufactures health-focused smart products. Sršen used her background as an artist to develop beautifully designed technology that informs and inspires women around the world. Collecting data on activity, sleep, stress, and reproductive health has allowed Bellabeat to empower women with knowledge about their own health and habits. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women.

By 2016, Bellabeat had opened offices around the world and launched multiple products. Bellabeat products became available through a growing number of online retailers in addition to their own e-commerce channel on their website. The company has invested in traditional advertising media, such as radio, out-of-home billboards, print, and television, but focuses on digital marketing extensively. Bellabeat invests year-round in Google Search, maintaining active Facebook and Instagram pages, and consistently engages consumers on Twitter. Additionally, Bellabeat runs video ads on Youtube and display ads on the Google Display Network to support campaigns around key marketing dates.

Sršen asks you to analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smart devices.

### These questions will guide your analysis:
1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?

# 2. Prepare
## Data Collection

We will be using the following data: [FitBit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit) (CC0: Public Domain, dataset made available through [Mobius](https://www.kaggle.com/arashnic)): This Kaggle data set contains personal fitness tracker from from 3/12/2016 - 5/12/2016. Fitbit users consented to the submission of personal tracker data. It includes information about daily activity that can be used to explore users’ habits.

### Environment Set-Up


```python
# Load the packages
library(tidyverse)
library(janitor)
library(data.table)
library(tools)
```


```python
# Import the data into a list of data tables
path <- "/kaggle/input/fitbit/mturkfitbit_export_3.12.16-4.11.16"
csv_files <- list.files(path = path,
                        pattern = "*.csv$",
                        recursive = TRUE,
                        full.names = TRUE)
unclean_data_tables1 <- map(csv_files, fread) %>% 
  set_names(file_path_sans_ext(basename(csv_files)))

path <- "/kaggle/input/fitbit/mturkfitbit_export_4.12.16-5.12.16"
csv_files <- list.files(path = path,
                        pattern = "*.csv$",
                        recursive = TRUE,
                        full.names = TRUE)
unclean_data_tables2 <- map(csv_files, fread) %>% 
  set_names(file_path_sans_ext(basename(csv_files)))
```

# 3. Process
## Data Cleaning
#### There is a lot of data in 29 total data tables, so our first step will be to simply remove any data tables aren't of use to us. The minute tables are far too granular, so we can get rid of those.


```python
# We'll use the more efficient negative indices rather than searching for substrings
unclean_data_tables1 <- unclean_data_tables1[-c(6:10)]
unclean_data_tables2 <- unclean_data_tables2[-c(9:16)]
```

#### Now that we have 2 lists of the same data (except for sleep data) from different time periods, we can combine based on their shared column names.


```python
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
```

#### Now that we have a single list of data tables, we can clean the individual tables and further explore.


```python
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
```


<dl>
	<dt>$dailyActivity_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 1397 × 15</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>steps</th><th scope=col>distance</th><th scope=col>tracker_distance</th><th scope=col>logged_activities_distance</th><th scope=col>very_active_distance</th><th scope=col>moderately_active_distance</th><th scope=col>light_active_distance</th><th scope=col>sedentary_active_distance</th><th scope=col>very_active_minutes</th><th scope=col>fairly_active_minutes</th><th scope=col>lightly_active_minutes</th><th scope=col>sedentary_minutes</th><th scope=col>calories</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>3/25/2016</td><td>11004</td><td> 7.11</td><td> 7.11</td><td>0</td><td>2.57</td><td>0.46</td><td> 4.07</td><td>0.00</td><td>33</td><td>12</td><td>205</td><td> 804</td><td>1819</td></tr>
	<tr><td>1503960366</td><td>3/26/2016</td><td>17609</td><td>11.55</td><td>11.55</td><td>0</td><td>6.92</td><td>0.73</td><td> 3.91</td><td>0.00</td><td>89</td><td>17</td><td>274</td><td> 588</td><td>2154</td></tr>
	<tr><td>1503960366</td><td>3/27/2016</td><td>12736</td><td> 8.53</td><td> 8.53</td><td>0</td><td>4.66</td><td>0.16</td><td> 3.71</td><td>0.00</td><td>56</td><td> 5</td><td>268</td><td> 605</td><td>1944</td></tr>
	<tr><td>1503960366</td><td>3/28/2016</td><td>13231</td><td> 8.93</td><td> 8.93</td><td>0</td><td>3.19</td><td>0.79</td><td> 4.95</td><td>0.00</td><td>39</td><td>20</td><td>224</td><td>1080</td><td>1932</td></tr>
	<tr><td>1503960366</td><td>3/29/2016</td><td>12041</td><td> 7.85</td><td> 7.85</td><td>0</td><td>2.16</td><td>1.09</td><td> 4.61</td><td>0.00</td><td>28</td><td>28</td><td>243</td><td> 763</td><td>1886</td></tr>
	<tr><td>1503960366</td><td>3/30/2016</td><td>10970</td><td> 7.16</td><td> 7.16</td><td>0</td><td>2.36</td><td>0.51</td><td> 4.29</td><td>0.00</td><td>30</td><td>13</td><td>223</td><td>1174</td><td>1820</td></tr>
	<tr><td>1503960366</td><td>3/31/2016</td><td>12256</td><td> 7.86</td><td> 7.86</td><td>0</td><td>2.29</td><td>0.49</td><td> 5.04</td><td>0.00</td><td>33</td><td>12</td><td>239</td><td> 820</td><td>1889</td></tr>
	<tr><td>1503960366</td><td>4/1/2016 </td><td>12262</td><td> 7.87</td><td> 7.87</td><td>0</td><td>3.32</td><td>0.83</td><td> 3.64</td><td>0.00</td><td>47</td><td>21</td><td>200</td><td> 866</td><td>1868</td></tr>
	<tr><td>1503960366</td><td>4/2/2016 </td><td>11248</td><td> 7.25</td><td> 7.25</td><td>0</td><td>3.00</td><td>0.45</td><td> 3.74</td><td>0.00</td><td>40</td><td>11</td><td>244</td><td> 636</td><td>1843</td></tr>
	<tr><td>1503960366</td><td>4/3/2016 </td><td>10016</td><td> 6.37</td><td> 6.37</td><td>0</td><td>0.91</td><td>1.28</td><td> 4.18</td><td>0.00</td><td>15</td><td>30</td><td>314</td><td> 655</td><td>1850</td></tr>
	<tr><td>1503960366</td><td>4/4/2016 </td><td>14557</td><td> 9.80</td><td> 9.80</td><td>0</td><td>3.39</td><td>0.70</td><td> 5.69</td><td>0.00</td><td>43</td><td>18</td><td>285</td><td> 757</td><td>2030</td></tr>
	<tr><td>1503960366</td><td>4/5/2016 </td><td>14844</td><td> 9.73</td><td> 9.73</td><td>0</td><td>2.94</td><td>0.76</td><td> 6.04</td><td>0.00</td><td>36</td><td>18</td><td>341</td><td> 736</td><td>2083</td></tr>
	<tr><td>1503960366</td><td>4/6/2016 </td><td>11974</td><td> 7.67</td><td> 7.67</td><td>0</td><td>2.04</td><td>0.48</td><td> 5.15</td><td>0.00</td><td>27</td><td>12</td><td>228</td><td>1173</td><td>1861</td></tr>
	<tr><td>1503960366</td><td>4/7/2016 </td><td>10198</td><td> 6.44</td><td> 6.44</td><td>0</td><td>1.20</td><td>0.79</td><td> 4.45</td><td>0.00</td><td>17</td><td>20</td><td>195</td><td>1208</td><td>1755</td></tr>
	<tr><td>1503960366</td><td>4/8/2016 </td><td>12521</td><td> 7.94</td><td> 7.94</td><td>0</td><td>3.31</td><td>0.90</td><td> 3.74</td><td>0.00</td><td>46</td><td>22</td><td>212</td><td>1160</td><td>1895</td></tr>
	<tr><td>1503960366</td><td>4/9/2016 </td><td>12432</td><td> 8.10</td><td> 8.10</td><td>0</td><td>2.59</td><td>0.59</td><td> 4.92</td><td>0.00</td><td>32</td><td>15</td><td>248</td><td> 738</td><td>1883</td></tr>
	<tr><td>1503960366</td><td>4/10/2016</td><td>10057</td><td> 6.98</td><td> 6.98</td><td>0</td><td>4.00</td><td>0.49</td><td> 2.48</td><td>0.00</td><td>44</td><td>13</td><td>168</td><td> 737</td><td>1755</td></tr>
	<tr><td>1503960366</td><td>4/11/2016</td><td>10990</td><td> 7.26</td><td> 7.26</td><td>0</td><td>2.04</td><td>0.57</td><td> 4.65</td><td>0.00</td><td>26</td><td>14</td><td>216</td><td> 855</td><td>1811</td></tr>
	<tr><td>1503960366</td><td>4/12/2016</td><td>  224</td><td> 0.14</td><td> 0.14</td><td>0</td><td>0.00</td><td>0.00</td><td> 0.13</td><td>0.00</td><td> 0</td><td> 0</td><td>  9</td><td>  32</td><td>  50</td></tr>
	<tr><td>1624580081</td><td>3/25/2016</td><td> 1810</td><td> 1.18</td><td> 1.18</td><td>0</td><td>0.00</td><td>0.00</td><td> 1.13</td><td>0.01</td><td> 0</td><td> 0</td><td>121</td><td>1319</td><td>1373</td></tr>
	<tr><td>1624580081</td><td>3/26/2016</td><td>  815</td><td> 0.53</td><td> 0.53</td><td>0</td><td>0.00</td><td>0.00</td><td> 0.53</td><td>0.00</td><td> 0</td><td> 0</td><td> 47</td><td>1393</td><td>1264</td></tr>
	<tr><td>1624580081</td><td>3/27/2016</td><td> 1985</td><td> 1.29</td><td> 1.29</td><td>0</td><td>0.00</td><td>0.00</td><td> 1.28</td><td>0.01</td><td> 0</td><td> 0</td><td>112</td><td>1328</td><td>1350</td></tr>
	<tr><td>1624580081</td><td>3/28/2016</td><td> 1905</td><td> 1.24</td><td> 1.24</td><td>0</td><td>0.00</td><td>0.00</td><td> 1.24</td><td>0.00</td><td> 0</td><td> 0</td><td> 95</td><td>1345</td><td>1342</td></tr>
	<tr><td>1624580081</td><td>3/29/2016</td><td> 1552</td><td> 1.01</td><td> 1.01</td><td>0</td><td>0.00</td><td>0.00</td><td> 1.00</td><td>0.00</td><td> 0</td><td> 0</td><td> 66</td><td>1374</td><td>1300</td></tr>
	<tr><td>1624580081</td><td>3/30/2016</td><td> 1675</td><td> 1.09</td><td> 1.09</td><td>0</td><td>0.00</td><td>0.00</td><td> 1.09</td><td>0.00</td><td> 0</td><td> 0</td><td> 84</td><td>1356</td><td>1313</td></tr>
	<tr><td>1624580081</td><td>3/31/2016</td><td> 4506</td><td> 2.93</td><td> 2.93</td><td>0</td><td>0.51</td><td>0.27</td><td> 2.15</td><td>0.01</td><td> 7</td><td> 4</td><td>144</td><td>1285</td><td>1498</td></tr>
	<tr><td>1624580081</td><td>4/1/2016 </td><td> 9218</td><td> 5.99</td><td> 5.99</td><td>0</td><td>0.00</td><td>0.00</td><td> 5.97</td><td>0.01</td><td> 0</td><td> 0</td><td>221</td><td>1219</td><td>1541</td></tr>
	<tr><td>1624580081</td><td>4/2/2016 </td><td> 1556</td><td> 1.01</td><td> 1.01</td><td>0</td><td>0.00</td><td>0.00</td><td> 1.01</td><td>0.01</td><td> 0</td><td> 0</td><td> 88</td><td>1352</td><td>1327</td></tr>
	<tr><td>1624580081</td><td>4/3/2016 </td><td> 2910</td><td> 1.89</td><td> 1.89</td><td>0</td><td>0.00</td><td>0.00</td><td> 1.88</td><td>0.01</td><td> 0</td><td> 0</td><td>157</td><td>1283</td><td>1452</td></tr>
	<tr><td>1624580081</td><td>4/4/2016 </td><td>18464</td><td>12.00</td><td>12.00</td><td>0</td><td>0.00</td><td>0.00</td><td>12.00</td><td>0.00</td><td> 0</td><td> 0</td><td>270</td><td>1170</td><td>1574</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8877689391</td><td>4/13/2016</td><td>15337</td><td> 9.58</td><td> 9.58</td><td>0</td><td> 3.55</td><td>0.38</td><td> 5.64</td><td>0.00</td><td>108</td><td>18</td><td>216</td><td>1098</td><td>3566</td></tr>
	<tr><td>8877689391</td><td>4/14/2016</td><td>21129</td><td>18.98</td><td>18.98</td><td>0</td><td>10.55</td><td>0.59</td><td> 7.75</td><td>0.02</td><td> 68</td><td>13</td><td>298</td><td>1061</td><td>3793</td></tr>
	<tr><td>8877689391</td><td>4/15/2016</td><td>13422</td><td> 7.17</td><td> 7.17</td><td>0</td><td> 0.05</td><td>0.05</td><td> 7.01</td><td>0.01</td><td>106</td><td> 1</td><td>281</td><td>1052</td><td>3934</td></tr>
	<tr><td>8877689391</td><td>4/16/2016</td><td>29326</td><td>25.29</td><td>25.29</td><td>0</td><td>13.24</td><td>1.21</td><td>10.71</td><td>0.00</td><td> 94</td><td>29</td><td>429</td><td> 888</td><td>4547</td></tr>
	<tr><td>8877689391</td><td>4/17/2016</td><td>15118</td><td> 8.87</td><td> 8.87</td><td>0</td><td> 0.00</td><td>0.07</td><td> 8.79</td><td>0.00</td><td> 58</td><td>15</td><td>307</td><td>1060</td><td>3545</td></tr>
	<tr><td>8877689391</td><td>4/18/2016</td><td>11423</td><td> 8.67</td><td> 8.67</td><td>0</td><td> 2.44</td><td>0.27</td><td> 5.94</td><td>0.00</td><td> 29</td><td> 5</td><td>191</td><td>1215</td><td>2761</td></tr>
	<tr><td>8877689391</td><td>4/19/2016</td><td>18785</td><td>17.40</td><td>17.40</td><td>0</td><td>12.15</td><td>0.18</td><td> 5.03</td><td>0.00</td><td> 82</td><td>13</td><td>214</td><td>1131</td><td>3676</td></tr>
	<tr><td>8877689391</td><td>4/20/2016</td><td>19948</td><td>18.11</td><td>18.11</td><td>0</td><td>11.02</td><td>0.69</td><td> 6.34</td><td>0.00</td><td> 73</td><td>19</td><td>225</td><td>1123</td><td>3679</td></tr>
	<tr><td>8877689391</td><td>4/21/2016</td><td>19377</td><td>17.62</td><td>17.62</td><td>0</td><td>12.29</td><td>0.42</td><td> 4.89</td><td>0.00</td><td> 82</td><td>13</td><td>226</td><td>1119</td><td>3659</td></tr>
	<tr><td>8877689391</td><td>4/22/2016</td><td>18258</td><td>16.31</td><td>16.31</td><td>0</td><td>10.23</td><td>0.03</td><td> 5.97</td><td>0.05</td><td> 61</td><td> 2</td><td>236</td><td>1141</td><td>3427</td></tr>
	<tr><td>8877689391</td><td>4/23/2016</td><td>11200</td><td> 7.43</td><td> 7.43</td><td>0</td><td> 0.00</td><td>0.00</td><td> 7.40</td><td>0.01</td><td>102</td><td> 6</td><td>300</td><td>1032</td><td>3891</td></tr>
	<tr><td>8877689391</td><td>4/24/2016</td><td>16674</td><td>15.74</td><td>15.74</td><td>0</td><td>11.01</td><td>0.01</td><td> 4.69</td><td>0.00</td><td> 64</td><td> 1</td><td>227</td><td>1148</td><td>3455</td></tr>
	<tr><td>8877689391</td><td>4/25/2016</td><td>12986</td><td> 8.74</td><td> 8.74</td><td>0</td><td> 2.37</td><td>0.07</td><td> 6.27</td><td>0.01</td><td>113</td><td> 8</td><td>218</td><td>1101</td><td>3802</td></tr>
	<tr><td>8877689391</td><td>4/26/2016</td><td>11101</td><td> 8.43</td><td> 8.43</td><td>0</td><td> 1.76</td><td>0.13</td><td> 6.50</td><td>0.00</td><td> 22</td><td> 3</td><td>258</td><td>1157</td><td>2860</td></tr>
	<tr><td>8877689391</td><td>4/27/2016</td><td>23629</td><td>20.65</td><td>20.65</td><td>0</td><td>13.07</td><td>0.44</td><td> 7.10</td><td>0.00</td><td> 93</td><td> 8</td><td>235</td><td>1104</td><td>3808</td></tr>
	<tr><td>8877689391</td><td>4/28/2016</td><td>14890</td><td>11.30</td><td>11.30</td><td>0</td><td> 4.93</td><td>0.38</td><td> 5.97</td><td>0.00</td><td> 58</td><td> 8</td><td>231</td><td>1143</td><td>3060</td></tr>
	<tr><td>8877689391</td><td>4/29/2016</td><td> 9733</td><td> 7.39</td><td> 7.39</td><td>0</td><td> 1.38</td><td>0.17</td><td> 5.79</td><td>0.00</td><td> 18</td><td> 5</td><td>210</td><td>1207</td><td>2698</td></tr>
	<tr><td>8877689391</td><td>4/30/2016</td><td>27745</td><td>26.72</td><td>26.72</td><td>0</td><td>21.66</td><td>0.08</td><td> 4.93</td><td>0.00</td><td>124</td><td> 4</td><td>223</td><td>1089</td><td>4398</td></tr>
	<tr><td>8877689391</td><td>5/1/2016 </td><td>10930</td><td> 8.32</td><td> 8.32</td><td>0</td><td> 3.13</td><td>0.57</td><td> 4.57</td><td>0.00</td><td> 36</td><td>12</td><td>166</td><td>1226</td><td>2786</td></tr>
	<tr><td>8877689391</td><td>5/2/2016 </td><td> 4790</td><td> 3.64</td><td> 3.64</td><td>0</td><td> 0.00</td><td>0.00</td><td> 3.56</td><td>0.00</td><td>  0</td><td> 0</td><td>105</td><td>1335</td><td>2189</td></tr>
	<tr><td>8877689391</td><td>5/3/2016 </td><td>10818</td><td> 8.21</td><td> 8.21</td><td>0</td><td> 1.39</td><td>0.10</td><td> 6.67</td><td>0.01</td><td> 19</td><td> 3</td><td>229</td><td>1189</td><td>2817</td></tr>
	<tr><td>8877689391</td><td>5/4/2016 </td><td>18193</td><td>16.30</td><td>16.30</td><td>0</td><td>10.42</td><td>0.31</td><td> 5.53</td><td>0.00</td><td> 66</td><td> 8</td><td>212</td><td>1154</td><td>3477</td></tr>
	<tr><td>8877689391</td><td>5/5/2016 </td><td>14055</td><td>10.67</td><td>10.67</td><td>0</td><td> 5.46</td><td>0.82</td><td> 4.37</td><td>0.00</td><td> 67</td><td>15</td><td>188</td><td>1170</td><td>3052</td></tr>
	<tr><td>8877689391</td><td>5/6/2016 </td><td>21727</td><td>19.34</td><td>19.34</td><td>0</td><td>12.79</td><td>0.29</td><td> 6.16</td><td>0.00</td><td> 96</td><td>17</td><td>232</td><td>1095</td><td>4015</td></tr>
	<tr><td>8877689391</td><td>5/7/2016 </td><td>12332</td><td> 8.13</td><td> 8.13</td><td>0</td><td> 0.08</td><td>0.96</td><td> 6.99</td><td>0.00</td><td>105</td><td>28</td><td>271</td><td>1036</td><td>4142</td></tr>
	<tr><td>8877689391</td><td>5/8/2016 </td><td>10686</td><td> 8.11</td><td> 8.11</td><td>0</td><td> 1.08</td><td>0.20</td><td> 6.80</td><td>0.00</td><td> 17</td><td> 4</td><td>245</td><td>1174</td><td>2847</td></tr>
	<tr><td>8877689391</td><td>5/9/2016 </td><td>20226</td><td>18.25</td><td>18.25</td><td>0</td><td>11.10</td><td>0.80</td><td> 6.24</td><td>0.05</td><td> 73</td><td>19</td><td>217</td><td>1131</td><td>3710</td></tr>
	<tr><td>8877689391</td><td>5/10/2016</td><td>10733</td><td> 8.15</td><td> 8.15</td><td>0</td><td> 1.35</td><td>0.46</td><td> 6.28</td><td>0.00</td><td> 18</td><td>11</td><td>224</td><td>1187</td><td>2832</td></tr>
	<tr><td>8877689391</td><td>5/11/2016</td><td>21420</td><td>19.56</td><td>19.56</td><td>0</td><td>13.22</td><td>0.41</td><td> 5.89</td><td>0.00</td><td> 88</td><td>12</td><td>213</td><td>1127</td><td>3832</td></tr>
	<tr><td>8877689391</td><td>5/12/2016</td><td> 8064</td><td> 6.12</td><td> 6.12</td><td>0</td><td> 1.82</td><td>0.04</td><td> 4.25</td><td>0.00</td><td> 23</td><td> 1</td><td>137</td><td> 770</td><td>1849</td></tr>
</tbody>
</table>
</dd>
	<dt>$heartrate_seconds_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 3638339 × 3</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>value</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>2022484408</td><td>4/1/2016 7:54:00 AM</td><td> 93</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:05 AM</td><td> 91</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:10 AM</td><td> 96</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:15 AM</td><td> 98</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:20 AM</td><td>100</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:25 AM</td><td>101</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:30 AM</td><td>104</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:35 AM</td><td>105</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:45 AM</td><td>102</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:54:55 AM</td><td>106</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:05 AM</td><td>109</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:10 AM</td><td>112</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:15 AM</td><td>111</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:25 AM</td><td>109</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:35 AM</td><td>110</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:40 AM</td><td>111</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:45 AM</td><td>112</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:50 AM</td><td>111</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:55:55 AM</td><td>106</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:00 AM</td><td>104</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:05 AM</td><td> 98</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:10 AM</td><td> 96</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:15 AM</td><td> 95</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:25 AM</td><td> 90</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:30 AM</td><td> 87</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:35 AM</td><td> 87</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:40 AM</td><td> 88</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:56:55 AM</td><td> 88</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:57:00 AM</td><td> 91</td></tr>
	<tr><td>2022484408</td><td>4/1/2016 7:57:10 AM</td><td> 90</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:40:23 PM</td><td>56</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:40:33 PM</td><td>57</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:40:38 PM</td><td>56</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:40:43 PM</td><td>58</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:40:48 PM</td><td>56</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:40:53 PM</td><td>55</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:40:58 PM</td><td>55</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:41:13 PM</td><td>55</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:41:18 PM</td><td>56</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:41:28 PM</td><td>62</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:41:38 PM</td><td>60</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:41:43 PM</td><td>59</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:41:48 PM</td><td>57</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:41:58 PM</td><td>56</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:42:03 PM</td><td>57</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:42:08 PM</td><td>55</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:42:13 PM</td><td>55</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:42:28 PM</td><td>55</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:42:33 PM</td><td>56</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:42:43 PM</td><td>57</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:42:58 PM</td><td>57</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:43:13 PM</td><td>57</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:43:18 PM</td><td>58</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:43:23 PM</td><td>59</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:43:38 PM</td><td>58</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:43:53 PM</td><td>57</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:43:58 PM</td><td>56</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:44:03 PM</td><td>55</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:44:18 PM</td><td>55</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:44:28 PM</td><td>56</td></tr>
</tbody>
</table>
</dd>
	<dt>$hourlyCalories_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 46183 × 3</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>calories</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>3/12/2016 12:00:00 AM</td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 1:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 2:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 3:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 4:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 5:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 6:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 7:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 8:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 9:00:00 AM </td><td> 49</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 10:00:00 AM</td><td> 89</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 11:00:00 AM</td><td>134</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 12:00:00 PM</td><td>130</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 1:00:00 PM </td><td> 81</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 2:00:00 PM </td><td>254</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 3:00:00 PM </td><td>271</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 4:00:00 PM </td><td>141</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 5:00:00 PM </td><td>104</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 6:00:00 PM </td><td> 54</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 7:00:00 PM </td><td> 97</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 8:00:00 PM </td><td> 58</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 9:00:00 PM </td><td>100</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 10:00:00 PM</td><td>123</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 11:00:00 PM</td><td>111</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 12:00:00 AM</td><td> 55</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 1:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 2:00:00 AM </td><td> 50</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 3:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 4:00:00 AM </td><td> 48</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 5:00:00 AM </td><td> 48</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 9:00:00 AM </td><td>113</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 10:00:00 AM</td><td>107</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 11:00:00 AM</td><td>168</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 12:00:00 PM</td><td>402</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 1:00:00 PM </td><td>120</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 2:00:00 PM </td><td> 82</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 3:00:00 PM </td><td>108</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 4:00:00 PM </td><td>310</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 5:00:00 PM </td><td>835</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 6:00:00 PM </td><td>197</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 7:00:00 PM </td><td>144</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 8:00:00 PM </td><td>174</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 9:00:00 PM </td><td>123</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 10:00:00 PM</td><td> 93</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 11:00:00 PM</td><td> 73</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 12:00:00 AM</td><td> 73</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 1:00:00 AM </td><td> 73</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:00:00 AM </td><td> 73</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 3:00:00 AM </td><td> 73</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 4:00:00 AM </td><td> 73</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 5:00:00 AM </td><td> 73</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 6:00:00 AM </td><td>107</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 7:00:00 AM </td><td>148</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 8:00:00 AM </td><td>137</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 9:00:00 AM </td><td> 88</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 10:00:00 AM</td><td>126</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 11:00:00 AM</td><td>192</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 12:00:00 PM</td><td>321</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 1:00:00 PM </td><td>101</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:00:00 PM </td><td>113</td></tr>
</tbody>
</table>
</dd>
	<dt>$hourlyIntensities_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 46183 × 4</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>intensity</th><th scope=col>average_intensity</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>3/12/2016 12:00:00 AM</td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 1:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 2:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 3:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 4:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 5:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 6:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 7:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 8:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 9:00:00 AM </td><td>  1</td><td>0.016667</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 10:00:00 AM</td><td> 26</td><td>0.433333</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 11:00:00 AM</td><td> 49</td><td>0.816667</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 12:00:00 PM</td><td> 47</td><td>0.783333</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 1:00:00 PM </td><td> 20</td><td>0.333333</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 2:00:00 PM </td><td>121</td><td>2.016667</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 3:00:00 PM </td><td>129</td><td>2.150000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 4:00:00 PM </td><td> 42</td><td>0.700000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 5:00:00 PM </td><td> 20</td><td>0.333333</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 6:00:00 PM </td><td>  2</td><td>0.033333</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 7:00:00 PM </td><td> 30</td><td>0.500000</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 8:00:00 PM </td><td>  4</td><td>0.066667</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 9:00:00 PM </td><td> 29</td><td>0.483333</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 10:00:00 PM</td><td> 41</td><td>0.683333</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 11:00:00 PM</td><td> 35</td><td>0.583333</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 12:00:00 AM</td><td>  3</td><td>0.050000</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 1:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 2:00:00 AM </td><td>  1</td><td>0.016667</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 3:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 4:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 5:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 9:00:00 AM </td><td> 11</td><td>0.183333</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 10:00:00 AM</td><td> 11</td><td>0.183333</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 11:00:00 AM</td><td> 23</td><td>0.383333</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 12:00:00 PM</td><td> 96</td><td>1.600000</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 1:00:00 PM </td><td> 12</td><td>0.200000</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 2:00:00 PM </td><td>  3</td><td>0.050000</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 3:00:00 PM </td><td>  8</td><td>0.133333</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 4:00:00 PM </td><td> 58</td><td>0.966667</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 5:00:00 PM </td><td>163</td><td>2.716667</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 6:00:00 PM </td><td> 29</td><td>0.483333</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 7:00:00 PM </td><td> 15</td><td>0.250000</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 8:00:00 PM </td><td> 26</td><td>0.433333</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 9:00:00 PM </td><td> 13</td><td>0.216667</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 10:00:00 PM</td><td>  4</td><td>0.066667</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 11:00:00 PM</td><td>  0</td><td>0.000000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 12:00:00 AM</td><td>  0</td><td>0.000000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 1:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 3:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 4:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 5:00:00 AM </td><td>  0</td><td>0.000000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 6:00:00 AM </td><td>  9</td><td>0.150000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 7:00:00 AM </td><td> 19</td><td>0.316667</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 8:00:00 AM </td><td> 16</td><td>0.266667</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 9:00:00 AM </td><td>  4</td><td>0.066667</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 10:00:00 AM</td><td> 12</td><td>0.200000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 11:00:00 AM</td><td> 29</td><td>0.483333</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 12:00:00 PM</td><td> 93</td><td>1.550000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 1:00:00 PM </td><td>  6</td><td>0.100000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:00:00 PM </td><td>  9</td><td>0.150000</td></tr>
</tbody>
</table>
</dd>
	<dt>$hourlySteps_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 46183 × 3</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>steps</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>3/12/2016 12:00:00 AM</td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 1:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 2:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 3:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 4:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 5:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 6:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 7:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 8:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 9:00:00 AM </td><td>   8</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 10:00:00 AM</td><td> 551</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 11:00:00 AM</td><td>1764</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 12:00:00 PM</td><td>1259</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 1:00:00 PM </td><td> 253</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 2:00:00 PM </td><td>4470</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 3:00:00 PM </td><td>4919</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 4:00:00 PM </td><td>1905</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 5:00:00 PM </td><td>1038</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 6:00:00 PM </td><td>  14</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 7:00:00 PM </td><td> 606</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 8:00:00 PM </td><td>  56</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 9:00:00 PM </td><td> 782</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 10:00:00 PM</td><td>1132</td></tr>
	<tr><td>1503960366</td><td>3/12/2016 11:00:00 PM</td><td> 918</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 12:00:00 AM</td><td>  39</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 1:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 2:00:00 AM </td><td>  27</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 3:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 4:00:00 AM </td><td>   0</td></tr>
	<tr><td>1503960366</td><td>3/13/2016 5:00:00 AM </td><td>   0</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 9:00:00 AM </td><td> 338</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 10:00:00 AM</td><td> 295</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 11:00:00 AM</td><td> 974</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 12:00:00 PM</td><td>3929</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 1:00:00 PM </td><td> 481</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 2:00:00 PM </td><td>  37</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 3:00:00 PM </td><td> 416</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 4:00:00 PM </td><td>2869</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 5:00:00 PM </td><td>8357</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 6:00:00 PM </td><td> 920</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 7:00:00 PM </td><td> 408</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 8:00:00 PM </td><td> 633</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 9:00:00 PM </td><td> 228</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 10:00:00 PM</td><td> 109</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 11:00:00 PM</td><td>   0</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 12:00:00 AM</td><td>   0</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 1:00:00 AM </td><td>   0</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:00:00 AM </td><td>   0</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 3:00:00 AM </td><td>   0</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 4:00:00 AM </td><td>   0</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 5:00:00 AM </td><td>   0</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 6:00:00 AM </td><td> 254</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 7:00:00 AM </td><td> 574</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 8:00:00 AM </td><td> 765</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 9:00:00 AM </td><td> 164</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 10:00:00 AM</td><td> 514</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 11:00:00 AM</td><td>1407</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 12:00:00 PM</td><td>3135</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 1:00:00 PM </td><td> 307</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 2:00:00 PM </td><td> 457</td></tr>
</tbody>
</table>
</dd>
	<dt>$weightLogInfo_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 100 × 8</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>weight_kg</th><th scope=col>weight_pounds</th><th scope=col>fat</th><th scope=col>bmi</th><th scope=col>is_manual_report</th><th scope=col>log_id</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;lgl&gt;</th><th scope=col>&lt;int64&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>4/5/2016 11:59:59 PM </td><td> 53.3</td><td>117.5064</td><td>22</td><td>22.97</td><td> TRUE</td><td>1459900799000</td></tr>
	<tr><td>1927972279</td><td>4/10/2016 6:33:26 PM </td><td>129.6</td><td>285.7191</td><td>NA</td><td>46.17</td><td>FALSE</td><td>1460313206000</td></tr>
	<tr><td>2347167796</td><td>4/3/2016 11:59:59 PM </td><td> 63.4</td><td>139.7731</td><td>10</td><td>24.77</td><td> TRUE</td><td>1459727999000</td></tr>
	<tr><td>2873212765</td><td>4/6/2016 11:59:59 PM </td><td> 56.7</td><td>125.0021</td><td>NA</td><td>21.45</td><td> TRUE</td><td>1459987199000</td></tr>
	<tr><td>2873212765</td><td>4/7/2016 11:59:59 PM </td><td> 57.2</td><td>126.1044</td><td>NA</td><td>21.65</td><td> TRUE</td><td>1460073599000</td></tr>
	<tr><td>2891001357</td><td>4/5/2016 11:59:59 PM </td><td> 88.4</td><td>194.8886</td><td>NA</td><td>25.03</td><td> TRUE</td><td>1459900799000</td></tr>
	<tr><td>4445114986</td><td>3/30/2016 11:59:59 PM</td><td> 92.4</td><td>203.7071</td><td>NA</td><td>35.01</td><td> TRUE</td><td>1459382399000</td></tr>
	<tr><td>4558609924</td><td>4/8/2016 11:59:59 PM </td><td> 69.4</td><td>153.0008</td><td>NA</td><td>27.14</td><td> TRUE</td><td>1460159999000</td></tr>
	<tr><td>4702921684</td><td>4/4/2016 11:59:59 PM </td><td> 99.7</td><td>219.8009</td><td>NA</td><td>26.11</td><td> TRUE</td><td>1459814399000</td></tr>
	<tr><td>6962181067</td><td>3/30/2016 11:59:59 PM</td><td> 61.5</td><td>135.5843</td><td>NA</td><td>24.03</td><td> TRUE</td><td>1459382399000</td></tr>
	<tr><td>6962181067</td><td>3/31/2016 11:59:59 PM</td><td> 61.5</td><td>135.5843</td><td>NA</td><td>24.03</td><td> TRUE</td><td>1459468799000</td></tr>
	<tr><td>6962181067</td><td>4/1/2016 11:59:59 PM </td><td> 60.9</td><td>134.2615</td><td>NA</td><td>23.78</td><td> TRUE</td><td>1459555199000</td></tr>
	<tr><td>6962181067</td><td>4/2/2016 11:59:59 PM </td><td> 61.2</td><td>134.9229</td><td>NA</td><td>23.89</td><td> TRUE</td><td>1459641599000</td></tr>
	<tr><td>6962181067</td><td>4/3/2016 11:59:59 PM </td><td> 61.5</td><td>135.5843</td><td>NA</td><td>24.03</td><td> TRUE</td><td>1459727999000</td></tr>
	<tr><td>6962181067</td><td>4/4/2016 11:59:59 PM </td><td> 62.4</td><td>137.5685</td><td>NA</td><td>24.35</td><td> TRUE</td><td>1459814399000</td></tr>
	<tr><td>6962181067</td><td>4/5/2016 11:59:59 PM </td><td> 61.7</td><td>136.0252</td><td>NA</td><td>24.10</td><td> TRUE</td><td>1459900799000</td></tr>
	<tr><td>6962181067</td><td>4/6/2016 11:59:59 PM </td><td> 62.2</td><td>137.1275</td><td>NA</td><td>24.28</td><td> TRUE</td><td>1459987199000</td></tr>
	<tr><td>6962181067</td><td>4/7/2016 11:59:59 PM </td><td> 62.2</td><td>137.1275</td><td>NA</td><td>24.28</td><td> TRUE</td><td>1460073599000</td></tr>
	<tr><td>6962181067</td><td>4/8/2016 11:59:59 PM </td><td> 61.7</td><td>136.0252</td><td>NA</td><td>24.10</td><td> TRUE</td><td>1460159999000</td></tr>
	<tr><td>6962181067</td><td>4/9/2016 11:59:59 PM </td><td> 62.1</td><td>136.9071</td><td>NA</td><td>24.24</td><td> TRUE</td><td>1460246399000</td></tr>
	<tr><td>6962181067</td><td>4/10/2016 11:59:59 PM</td><td> 62.5</td><td>137.7889</td><td>NA</td><td>24.39</td><td> TRUE</td><td>1460332799000</td></tr>
	<tr><td>6962181067</td><td>4/11/2016 11:59:59 PM</td><td> 62.2</td><td>137.1275</td><td>NA</td><td>24.28</td><td> TRUE</td><td>1460419199000</td></tr>
	<tr><td>6962181067</td><td>4/12/2016 11:59:59 PM</td><td> 62.5</td><td>137.7889</td><td>NA</td><td>24.39</td><td> TRUE</td><td>1460505599000</td></tr>
	<tr><td>8253242879</td><td>4/7/2016 11:59:59 PM </td><td> 75.6</td><td>166.6695</td><td>NA</td><td>29.55</td><td> TRUE</td><td>1460073599000</td></tr>
	<tr><td>8877689391</td><td>4/1/2016 6:49:40 AM  </td><td> 85.5</td><td>188.4952</td><td>NA</td><td>25.61</td><td>FALSE</td><td>1459493380000</td></tr>
	<tr><td>8877689391</td><td>4/4/2016 6:53:43 AM  </td><td> 86.6</td><td>190.9203</td><td>NA</td><td>25.94</td><td>FALSE</td><td>1459752823000</td></tr>
	<tr><td>8877689391</td><td>4/5/2016 6:40:15 AM  </td><td> 86.0</td><td>189.5975</td><td>NA</td><td>25.76</td><td>FALSE</td><td>1459838415000</td></tr>
	<tr><td>8877689391</td><td>4/6/2016 6:49:31 AM  </td><td> 86.3</td><td>190.2589</td><td>NA</td><td>25.83</td><td>FALSE</td><td>1459925371000</td></tr>
	<tr><td>8877689391</td><td>4/7/2016 6:15:08 AM  </td><td> 85.1</td><td>187.6134</td><td>NA</td><td>25.49</td><td>FALSE</td><td>1460009708000</td></tr>
	<tr><td>8877689391</td><td>4/8/2016 6:39:44 AM  </td><td> 85.0</td><td>187.3929</td><td>NA</td><td>25.44</td><td>FALSE</td><td>1460097584000</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>6962181067</td><td>5/7/2016 11:59:59 PM </td><td>61.2</td><td>134.9229</td><td>NA</td><td>23.89</td><td> TRUE</td><td>1462665599000</td></tr>
	<tr><td>6962181067</td><td>5/8/2016 11:59:59 PM </td><td>61.2</td><td>134.9229</td><td>NA</td><td>23.89</td><td> TRUE</td><td>1462751999000</td></tr>
	<tr><td>6962181067</td><td>5/9/2016 11:59:59 PM </td><td>62.4</td><td>137.5685</td><td>NA</td><td>24.35</td><td> TRUE</td><td>1462838399000</td></tr>
	<tr><td>6962181067</td><td>5/10/2016 11:59:59 PM</td><td>62.1</td><td>136.9071</td><td>NA</td><td>24.24</td><td> TRUE</td><td>1462924799000</td></tr>
	<tr><td>6962181067</td><td>5/11/2016 11:59:59 PM</td><td>61.9</td><td>136.4661</td><td>NA</td><td>24.17</td><td> TRUE</td><td>1463011199000</td></tr>
	<tr><td>6962181067</td><td>5/12/2016 11:59:59 PM</td><td>61.9</td><td>136.4661</td><td>NA</td><td>24.17</td><td> TRUE</td><td>1463097599000</td></tr>
	<tr><td>8877689391</td><td>4/12/2016 6:47:11 AM </td><td>85.8</td><td>189.1566</td><td>NA</td><td>25.68</td><td>FALSE</td><td>1460443631000</td></tr>
	<tr><td>8877689391</td><td>4/13/2016 6:55:00 AM </td><td>84.9</td><td>187.1725</td><td>NA</td><td>25.41</td><td>FALSE</td><td>1460530500000</td></tr>
	<tr><td>8877689391</td><td>4/14/2016 6:48:43 AM </td><td>84.5</td><td>186.2906</td><td>NA</td><td>25.31</td><td>FALSE</td><td>1460616523000</td></tr>
	<tr><td>8877689391</td><td>4/16/2016 1:39:25 PM </td><td>85.5</td><td>188.4952</td><td>NA</td><td>25.59</td><td>FALSE</td><td>1460813965000</td></tr>
	<tr><td>8877689391</td><td>4/18/2016 6:51:14 AM </td><td>85.8</td><td>189.1566</td><td>NA</td><td>25.68</td><td>FALSE</td><td>1460962274000</td></tr>
	<tr><td>8877689391</td><td>4/19/2016 6:39:31 AM </td><td>85.3</td><td>188.0543</td><td>NA</td><td>25.53</td><td>FALSE</td><td>1461047971000</td></tr>
	<tr><td>8877689391</td><td>4/20/2016 6:44:54 AM </td><td>84.9</td><td>187.1725</td><td>NA</td><td>25.41</td><td>FALSE</td><td>1461134694000</td></tr>
	<tr><td>8877689391</td><td>4/21/2016 6:50:27 AM </td><td>84.5</td><td>186.2906</td><td>NA</td><td>25.29</td><td>FALSE</td><td>1461221427000</td></tr>
	<tr><td>8877689391</td><td>4/23/2016 7:22:28 AM </td><td>85.5</td><td>188.4952</td><td>NA</td><td>25.59</td><td>FALSE</td><td>1461396148000</td></tr>
	<tr><td>8877689391</td><td>4/24/2016 7:38:05 AM </td><td>85.5</td><td>188.4952</td><td>NA</td><td>25.59</td><td>FALSE</td><td>1461483485000</td></tr>
	<tr><td>8877689391</td><td>4/25/2016 6:40:16 AM </td><td>85.4</td><td>188.2748</td><td>NA</td><td>25.56</td><td>FALSE</td><td>1461566416000</td></tr>
	<tr><td>8877689391</td><td>4/26/2016 6:50:27 AM </td><td>85.1</td><td>187.6134</td><td>NA</td><td>25.49</td><td>FALSE</td><td>1461653427000</td></tr>
	<tr><td>8877689391</td><td>4/27/2016 6:51:05 AM </td><td>85.4</td><td>188.2748</td><td>NA</td><td>25.56</td><td>FALSE</td><td>1461739865000</td></tr>
	<tr><td>8877689391</td><td>4/28/2016 6:50:03 AM </td><td>85.1</td><td>187.6134</td><td>NA</td><td>25.49</td><td>FALSE</td><td>1461826203000</td></tr>
	<tr><td>8877689391</td><td>4/29/2016 6:49:55 AM </td><td>84.9</td><td>187.1725</td><td>NA</td><td>25.41</td><td>FALSE</td><td>1461912595000</td></tr>
	<tr><td>8877689391</td><td>4/30/2016 7:49:03 AM </td><td>85.5</td><td>188.4952</td><td>NA</td><td>25.59</td><td>FALSE</td><td>1462002543000</td></tr>
	<tr><td>8877689391</td><td>5/1/2016 8:47:49 AM  </td><td>85.3</td><td>188.0543</td><td>NA</td><td>25.53</td><td>FALSE</td><td>1462092469000</td></tr>
	<tr><td>8877689391</td><td>5/3/2016 6:49:41 AM  </td><td>84.9</td><td>187.1725</td><td>NA</td><td>25.41</td><td>FALSE</td><td>1462258181000</td></tr>
	<tr><td>8877689391</td><td>5/4/2016 6:48:22 AM  </td><td>84.4</td><td>186.0702</td><td>NA</td><td>25.26</td><td>FALSE</td><td>1462344502000</td></tr>
	<tr><td>8877689391</td><td>5/6/2016 6:43:35 AM  </td><td>85.0</td><td>187.3929</td><td>NA</td><td>25.44</td><td>FALSE</td><td>1462517015000</td></tr>
	<tr><td>8877689391</td><td>5/8/2016 7:35:53 AM  </td><td>85.4</td><td>188.2748</td><td>NA</td><td>25.56</td><td>FALSE</td><td>1462692953000</td></tr>
	<tr><td>8877689391</td><td>5/9/2016 6:39:44 AM  </td><td>85.5</td><td>188.4952</td><td>NA</td><td>25.61</td><td>FALSE</td><td>1462775984000</td></tr>
	<tr><td>8877689391</td><td>5/11/2016 6:51:47 AM </td><td>85.4</td><td>188.2748</td><td>NA</td><td>25.56</td><td>FALSE</td><td>1462949507000</td></tr>
	<tr><td>8877689391</td><td>5/12/2016 6:42:53 AM </td><td>84.0</td><td>185.1883</td><td>NA</td><td>25.14</td><td>FALSE</td><td>1463035373000</td></tr>
</tbody>
</table>
</dd>
	<dt>$dailyCalories_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 940 × 3</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>calories</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>4/12/2016</td><td>1985</td></tr>
	<tr><td>1503960366</td><td>4/13/2016</td><td>1797</td></tr>
	<tr><td>1503960366</td><td>4/14/2016</td><td>1776</td></tr>
	<tr><td>1503960366</td><td>4/15/2016</td><td>1745</td></tr>
	<tr><td>1503960366</td><td>4/16/2016</td><td>1863</td></tr>
	<tr><td>1503960366</td><td>4/17/2016</td><td>1728</td></tr>
	<tr><td>1503960366</td><td>4/18/2016</td><td>1921</td></tr>
	<tr><td>1503960366</td><td>4/19/2016</td><td>2035</td></tr>
	<tr><td>1503960366</td><td>4/20/2016</td><td>1786</td></tr>
	<tr><td>1503960366</td><td>4/21/2016</td><td>1775</td></tr>
	<tr><td>1503960366</td><td>4/22/2016</td><td>1827</td></tr>
	<tr><td>1503960366</td><td>4/23/2016</td><td>1949</td></tr>
	<tr><td>1503960366</td><td>4/24/2016</td><td>1788</td></tr>
	<tr><td>1503960366</td><td>4/25/2016</td><td>2013</td></tr>
	<tr><td>1503960366</td><td>4/26/2016</td><td>1970</td></tr>
	<tr><td>1503960366</td><td>4/27/2016</td><td>2159</td></tr>
	<tr><td>1503960366</td><td>4/28/2016</td><td>1898</td></tr>
	<tr><td>1503960366</td><td>4/29/2016</td><td>1837</td></tr>
	<tr><td>1503960366</td><td>4/30/2016</td><td>1947</td></tr>
	<tr><td>1503960366</td><td>5/1/2016 </td><td>1820</td></tr>
	<tr><td>1503960366</td><td>5/2/2016 </td><td>2004</td></tr>
	<tr><td>1503960366</td><td>5/3/2016 </td><td>1990</td></tr>
	<tr><td>1503960366</td><td>5/4/2016 </td><td>1819</td></tr>
	<tr><td>1503960366</td><td>5/5/2016 </td><td>1959</td></tr>
	<tr><td>1503960366</td><td>5/6/2016 </td><td>1896</td></tr>
	<tr><td>1503960366</td><td>5/7/2016 </td><td>1821</td></tr>
	<tr><td>1503960366</td><td>5/8/2016 </td><td>1740</td></tr>
	<tr><td>1503960366</td><td>5/9/2016 </td><td>1819</td></tr>
	<tr><td>1503960366</td><td>5/10/2016</td><td>1859</td></tr>
	<tr><td>1503960366</td><td>5/11/2016</td><td>1783</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8877689391</td><td>4/13/2016</td><td>3566</td></tr>
	<tr><td>8877689391</td><td>4/14/2016</td><td>3793</td></tr>
	<tr><td>8877689391</td><td>4/15/2016</td><td>3934</td></tr>
	<tr><td>8877689391</td><td>4/16/2016</td><td>4547</td></tr>
	<tr><td>8877689391</td><td>4/17/2016</td><td>3545</td></tr>
	<tr><td>8877689391</td><td>4/18/2016</td><td>2761</td></tr>
	<tr><td>8877689391</td><td>4/19/2016</td><td>3676</td></tr>
	<tr><td>8877689391</td><td>4/20/2016</td><td>3679</td></tr>
	<tr><td>8877689391</td><td>4/21/2016</td><td>3659</td></tr>
	<tr><td>8877689391</td><td>4/22/2016</td><td>3427</td></tr>
	<tr><td>8877689391</td><td>4/23/2016</td><td>3891</td></tr>
	<tr><td>8877689391</td><td>4/24/2016</td><td>3455</td></tr>
	<tr><td>8877689391</td><td>4/25/2016</td><td>3802</td></tr>
	<tr><td>8877689391</td><td>4/26/2016</td><td>2860</td></tr>
	<tr><td>8877689391</td><td>4/27/2016</td><td>3808</td></tr>
	<tr><td>8877689391</td><td>4/28/2016</td><td>3060</td></tr>
	<tr><td>8877689391</td><td>4/29/2016</td><td>2698</td></tr>
	<tr><td>8877689391</td><td>4/30/2016</td><td>4398</td></tr>
	<tr><td>8877689391</td><td>5/1/2016 </td><td>2786</td></tr>
	<tr><td>8877689391</td><td>5/2/2016 </td><td>2189</td></tr>
	<tr><td>8877689391</td><td>5/3/2016 </td><td>2817</td></tr>
	<tr><td>8877689391</td><td>5/4/2016 </td><td>3477</td></tr>
	<tr><td>8877689391</td><td>5/5/2016 </td><td>3052</td></tr>
	<tr><td>8877689391</td><td>5/6/2016 </td><td>4015</td></tr>
	<tr><td>8877689391</td><td>5/7/2016 </td><td>4142</td></tr>
	<tr><td>8877689391</td><td>5/8/2016 </td><td>2847</td></tr>
	<tr><td>8877689391</td><td>5/9/2016 </td><td>3710</td></tr>
	<tr><td>8877689391</td><td>5/10/2016</td><td>2832</td></tr>
	<tr><td>8877689391</td><td>5/11/2016</td><td>3832</td></tr>
	<tr><td>8877689391</td><td>5/12/2016</td><td>1849</td></tr>
</tbody>
</table>
</dd>
	<dt>$dailyIntensities_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 940 × 10</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>sedentary_minutes</th><th scope=col>lightly_active_minutes</th><th scope=col>fairly_active_minutes</th><th scope=col>very_active_minutes</th><th scope=col>sedentary_active_distance</th><th scope=col>light_active_distance</th><th scope=col>moderately_active_distance</th><th scope=col>very_active_distance</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>4/12/2016</td><td> 728</td><td>328</td><td>13</td><td>25</td><td>0</td><td>6.06</td><td>0.55</td><td>1.88</td></tr>
	<tr><td>1503960366</td><td>4/13/2016</td><td> 776</td><td>217</td><td>19</td><td>21</td><td>0</td><td>4.71</td><td>0.69</td><td>1.57</td></tr>
	<tr><td>1503960366</td><td>4/14/2016</td><td>1218</td><td>181</td><td>11</td><td>30</td><td>0</td><td>3.91</td><td>0.40</td><td>2.44</td></tr>
	<tr><td>1503960366</td><td>4/15/2016</td><td> 726</td><td>209</td><td>34</td><td>29</td><td>0</td><td>2.83</td><td>1.26</td><td>2.14</td></tr>
	<tr><td>1503960366</td><td>4/16/2016</td><td> 773</td><td>221</td><td>10</td><td>36</td><td>0</td><td>5.04</td><td>0.41</td><td>2.71</td></tr>
	<tr><td>1503960366</td><td>4/17/2016</td><td> 539</td><td>164</td><td>20</td><td>38</td><td>0</td><td>2.51</td><td>0.78</td><td>3.19</td></tr>
	<tr><td>1503960366</td><td>4/18/2016</td><td>1149</td><td>233</td><td>16</td><td>42</td><td>0</td><td>4.71</td><td>0.64</td><td>3.25</td></tr>
	<tr><td>1503960366</td><td>4/19/2016</td><td> 775</td><td>264</td><td>31</td><td>50</td><td>0</td><td>5.03</td><td>1.32</td><td>3.53</td></tr>
	<tr><td>1503960366</td><td>4/20/2016</td><td> 818</td><td>205</td><td>12</td><td>28</td><td>0</td><td>4.24</td><td>0.48</td><td>1.96</td></tr>
	<tr><td>1503960366</td><td>4/21/2016</td><td> 838</td><td>211</td><td> 8</td><td>19</td><td>0</td><td>4.65</td><td>0.35</td><td>1.34</td></tr>
	<tr><td>1503960366</td><td>4/22/2016</td><td>1217</td><td>130</td><td>27</td><td>66</td><td>0</td><td>2.24</td><td>1.12</td><td>4.76</td></tr>
	<tr><td>1503960366</td><td>4/23/2016</td><td> 732</td><td>262</td><td>21</td><td>41</td><td>0</td><td>5.36</td><td>0.87</td><td>2.81</td></tr>
	<tr><td>1503960366</td><td>4/24/2016</td><td> 709</td><td>238</td><td> 5</td><td>39</td><td>0</td><td>3.28</td><td>0.21</td><td>2.92</td></tr>
	<tr><td>1503960366</td><td>4/25/2016</td><td> 814</td><td>216</td><td>14</td><td>73</td><td>0</td><td>3.94</td><td>0.57</td><td>5.29</td></tr>
	<tr><td>1503960366</td><td>4/26/2016</td><td> 833</td><td>279</td><td>23</td><td>31</td><td>0</td><td>5.54</td><td>0.92</td><td>2.33</td></tr>
	<tr><td>1503960366</td><td>4/27/2016</td><td>1108</td><td>243</td><td>11</td><td>78</td><td>0</td><td>5.41</td><td>0.41</td><td>6.40</td></tr>
	<tr><td>1503960366</td><td>4/28/2016</td><td> 782</td><td>189</td><td>28</td><td>48</td><td>0</td><td>3.79</td><td>1.16</td><td>3.54</td></tr>
	<tr><td>1503960366</td><td>4/29/2016</td><td> 815</td><td>243</td><td>12</td><td>16</td><td>0</td><td>5.58</td><td>0.50</td><td>1.06</td></tr>
	<tr><td>1503960366</td><td>4/30/2016</td><td> 712</td><td>217</td><td>34</td><td>52</td><td>0</td><td>4.27</td><td>1.42</td><td>3.56</td></tr>
	<tr><td>1503960366</td><td>5/1/2016 </td><td> 730</td><td>246</td><td>35</td><td>33</td><td>0</td><td>2.92</td><td>1.60</td><td>2.29</td></tr>
	<tr><td>1503960366</td><td>5/2/2016 </td><td> 798</td><td>277</td><td>15</td><td>41</td><td>0</td><td>5.92</td><td>0.57</td><td>3.21</td></tr>
	<tr><td>1503960366</td><td>5/3/2016 </td><td> 816</td><td>254</td><td>24</td><td>50</td><td>0</td><td>4.88</td><td>1.05</td><td>3.73</td></tr>
	<tr><td>1503960366</td><td>5/4/2016 </td><td>1179</td><td>203</td><td>22</td><td>36</td><td>0</td><td>3.82</td><td>0.87</td><td>2.46</td></tr>
	<tr><td>1503960366</td><td>5/5/2016 </td><td> 857</td><td>250</td><td>24</td><td>45</td><td>0</td><td>4.88</td><td>1.08</td><td>2.92</td></tr>
	<tr><td>1503960366</td><td>5/6/2016 </td><td> 754</td><td>289</td><td> 6</td><td>24</td><td>0</td><td>5.81</td><td>0.25</td><td>1.97</td></tr>
	<tr><td>1503960366</td><td>5/7/2016 </td><td> 833</td><td>175</td><td>46</td><td>37</td><td>0</td><td>3.13</td><td>2.12</td><td>2.46</td></tr>
	<tr><td>1503960366</td><td>5/8/2016 </td><td> 574</td><td>203</td><td> 8</td><td>44</td><td>0</td><td>2.73</td><td>0.32</td><td>3.53</td></tr>
	<tr><td>1503960366</td><td>5/9/2016 </td><td> 835</td><td>206</td><td>11</td><td>46</td><td>0</td><td>3.74</td><td>0.53</td><td>3.45</td></tr>
	<tr><td>1503960366</td><td>5/10/2016</td><td> 746</td><td>214</td><td>31</td><td>46</td><td>0</td><td>3.26</td><td>1.16</td><td>3.35</td></tr>
	<tr><td>1503960366</td><td>5/11/2016</td><td> 669</td><td>251</td><td>23</td><td>36</td><td>0</td><td>4.55</td><td>1.01</td><td>2.56</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8877689391</td><td>4/13/2016</td><td>1098</td><td>216</td><td>18</td><td>108</td><td>0.00</td><td> 5.64</td><td>0.38</td><td> 3.55</td></tr>
	<tr><td>8877689391</td><td>4/14/2016</td><td>1061</td><td>298</td><td>13</td><td> 68</td><td>0.02</td><td> 7.75</td><td>0.59</td><td>10.55</td></tr>
	<tr><td>8877689391</td><td>4/15/2016</td><td>1052</td><td>281</td><td> 1</td><td>106</td><td>0.01</td><td> 7.01</td><td>0.05</td><td> 0.05</td></tr>
	<tr><td>8877689391</td><td>4/16/2016</td><td> 888</td><td>429</td><td>29</td><td> 94</td><td>0.00</td><td>10.71</td><td>1.21</td><td>13.24</td></tr>
	<tr><td>8877689391</td><td>4/17/2016</td><td>1060</td><td>307</td><td>15</td><td> 58</td><td>0.00</td><td> 8.79</td><td>0.07</td><td> 0.00</td></tr>
	<tr><td>8877689391</td><td>4/18/2016</td><td>1215</td><td>191</td><td> 5</td><td> 29</td><td>0.00</td><td> 5.94</td><td>0.27</td><td> 2.44</td></tr>
	<tr><td>8877689391</td><td>4/19/2016</td><td>1131</td><td>214</td><td>13</td><td> 82</td><td>0.00</td><td> 5.03</td><td>0.18</td><td>12.15</td></tr>
	<tr><td>8877689391</td><td>4/20/2016</td><td>1123</td><td>225</td><td>19</td><td> 73</td><td>0.00</td><td> 6.34</td><td>0.69</td><td>11.02</td></tr>
	<tr><td>8877689391</td><td>4/21/2016</td><td>1119</td><td>226</td><td>13</td><td> 82</td><td>0.00</td><td> 4.89</td><td>0.42</td><td>12.29</td></tr>
	<tr><td>8877689391</td><td>4/22/2016</td><td>1141</td><td>236</td><td> 2</td><td> 61</td><td>0.05</td><td> 5.97</td><td>0.03</td><td>10.23</td></tr>
	<tr><td>8877689391</td><td>4/23/2016</td><td>1032</td><td>300</td><td> 6</td><td>102</td><td>0.01</td><td> 7.40</td><td>0.00</td><td> 0.00</td></tr>
	<tr><td>8877689391</td><td>4/24/2016</td><td>1148</td><td>227</td><td> 1</td><td> 64</td><td>0.00</td><td> 4.69</td><td>0.01</td><td>11.01</td></tr>
	<tr><td>8877689391</td><td>4/25/2016</td><td>1101</td><td>218</td><td> 8</td><td>113</td><td>0.01</td><td> 6.27</td><td>0.07</td><td> 2.37</td></tr>
	<tr><td>8877689391</td><td>4/26/2016</td><td>1157</td><td>258</td><td> 3</td><td> 22</td><td>0.00</td><td> 6.50</td><td>0.13</td><td> 1.76</td></tr>
	<tr><td>8877689391</td><td>4/27/2016</td><td>1104</td><td>235</td><td> 8</td><td> 93</td><td>0.00</td><td> 7.10</td><td>0.44</td><td>13.07</td></tr>
	<tr><td>8877689391</td><td>4/28/2016</td><td>1143</td><td>231</td><td> 8</td><td> 58</td><td>0.00</td><td> 5.97</td><td>0.38</td><td> 4.93</td></tr>
	<tr><td>8877689391</td><td>4/29/2016</td><td>1207</td><td>210</td><td> 5</td><td> 18</td><td>0.00</td><td> 5.79</td><td>0.17</td><td> 1.38</td></tr>
	<tr><td>8877689391</td><td>4/30/2016</td><td>1089</td><td>223</td><td> 4</td><td>124</td><td>0.00</td><td> 4.93</td><td>0.08</td><td>21.66</td></tr>
	<tr><td>8877689391</td><td>5/1/2016 </td><td>1226</td><td>166</td><td>12</td><td> 36</td><td>0.00</td><td> 4.57</td><td>0.57</td><td> 3.13</td></tr>
	<tr><td>8877689391</td><td>5/2/2016 </td><td>1335</td><td>105</td><td> 0</td><td>  0</td><td>0.00</td><td> 3.56</td><td>0.00</td><td> 0.00</td></tr>
	<tr><td>8877689391</td><td>5/3/2016 </td><td>1189</td><td>229</td><td> 3</td><td> 19</td><td>0.01</td><td> 6.67</td><td>0.10</td><td> 1.39</td></tr>
	<tr><td>8877689391</td><td>5/4/2016 </td><td>1154</td><td>212</td><td> 8</td><td> 66</td><td>0.00</td><td> 5.53</td><td>0.31</td><td>10.42</td></tr>
	<tr><td>8877689391</td><td>5/5/2016 </td><td>1170</td><td>188</td><td>15</td><td> 67</td><td>0.00</td><td> 4.37</td><td>0.82</td><td> 5.46</td></tr>
	<tr><td>8877689391</td><td>5/6/2016 </td><td>1095</td><td>232</td><td>17</td><td> 96</td><td>0.00</td><td> 6.16</td><td>0.29</td><td>12.79</td></tr>
	<tr><td>8877689391</td><td>5/7/2016 </td><td>1036</td><td>271</td><td>28</td><td>105</td><td>0.00</td><td> 6.99</td><td>0.96</td><td> 0.08</td></tr>
	<tr><td>8877689391</td><td>5/8/2016 </td><td>1174</td><td>245</td><td> 4</td><td> 17</td><td>0.00</td><td> 6.80</td><td>0.20</td><td> 1.08</td></tr>
	<tr><td>8877689391</td><td>5/9/2016 </td><td>1131</td><td>217</td><td>19</td><td> 73</td><td>0.05</td><td> 6.24</td><td>0.80</td><td>11.10</td></tr>
	<tr><td>8877689391</td><td>5/10/2016</td><td>1187</td><td>224</td><td>11</td><td> 18</td><td>0.00</td><td> 6.28</td><td>0.46</td><td> 1.35</td></tr>
	<tr><td>8877689391</td><td>5/11/2016</td><td>1127</td><td>213</td><td>12</td><td> 88</td><td>0.00</td><td> 5.89</td><td>0.41</td><td>13.22</td></tr>
	<tr><td>8877689391</td><td>5/12/2016</td><td> 770</td><td>137</td><td> 1</td><td> 23</td><td>0.00</td><td> 4.25</td><td>0.04</td><td> 1.82</td></tr>
</tbody>
</table>
</dd>
	<dt>$dailySteps_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 940 × 3</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>steps</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>4/12/2016</td><td>13162</td></tr>
	<tr><td>1503960366</td><td>4/13/2016</td><td>10735</td></tr>
	<tr><td>1503960366</td><td>4/14/2016</td><td>10460</td></tr>
	<tr><td>1503960366</td><td>4/15/2016</td><td> 9762</td></tr>
	<tr><td>1503960366</td><td>4/16/2016</td><td>12669</td></tr>
	<tr><td>1503960366</td><td>4/17/2016</td><td> 9705</td></tr>
	<tr><td>1503960366</td><td>4/18/2016</td><td>13019</td></tr>
	<tr><td>1503960366</td><td>4/19/2016</td><td>15506</td></tr>
	<tr><td>1503960366</td><td>4/20/2016</td><td>10544</td></tr>
	<tr><td>1503960366</td><td>4/21/2016</td><td> 9819</td></tr>
	<tr><td>1503960366</td><td>4/22/2016</td><td>12764</td></tr>
	<tr><td>1503960366</td><td>4/23/2016</td><td>14371</td></tr>
	<tr><td>1503960366</td><td>4/24/2016</td><td>10039</td></tr>
	<tr><td>1503960366</td><td>4/25/2016</td><td>15355</td></tr>
	<tr><td>1503960366</td><td>4/26/2016</td><td>13755</td></tr>
	<tr><td>1503960366</td><td>4/27/2016</td><td>18134</td></tr>
	<tr><td>1503960366</td><td>4/28/2016</td><td>13154</td></tr>
	<tr><td>1503960366</td><td>4/29/2016</td><td>11181</td></tr>
	<tr><td>1503960366</td><td>4/30/2016</td><td>14673</td></tr>
	<tr><td>1503960366</td><td>5/1/2016 </td><td>10602</td></tr>
	<tr><td>1503960366</td><td>5/2/2016 </td><td>14727</td></tr>
	<tr><td>1503960366</td><td>5/3/2016 </td><td>15103</td></tr>
	<tr><td>1503960366</td><td>5/4/2016 </td><td>11100</td></tr>
	<tr><td>1503960366</td><td>5/5/2016 </td><td>14070</td></tr>
	<tr><td>1503960366</td><td>5/6/2016 </td><td>12159</td></tr>
	<tr><td>1503960366</td><td>5/7/2016 </td><td>11992</td></tr>
	<tr><td>1503960366</td><td>5/8/2016 </td><td>10060</td></tr>
	<tr><td>1503960366</td><td>5/9/2016 </td><td>12022</td></tr>
	<tr><td>1503960366</td><td>5/10/2016</td><td>12207</td></tr>
	<tr><td>1503960366</td><td>5/11/2016</td><td>12770</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8877689391</td><td>4/13/2016</td><td>15337</td></tr>
	<tr><td>8877689391</td><td>4/14/2016</td><td>21129</td></tr>
	<tr><td>8877689391</td><td>4/15/2016</td><td>13422</td></tr>
	<tr><td>8877689391</td><td>4/16/2016</td><td>29326</td></tr>
	<tr><td>8877689391</td><td>4/17/2016</td><td>15118</td></tr>
	<tr><td>8877689391</td><td>4/18/2016</td><td>11423</td></tr>
	<tr><td>8877689391</td><td>4/19/2016</td><td>18785</td></tr>
	<tr><td>8877689391</td><td>4/20/2016</td><td>19948</td></tr>
	<tr><td>8877689391</td><td>4/21/2016</td><td>19377</td></tr>
	<tr><td>8877689391</td><td>4/22/2016</td><td>18258</td></tr>
	<tr><td>8877689391</td><td>4/23/2016</td><td>11200</td></tr>
	<tr><td>8877689391</td><td>4/24/2016</td><td>16674</td></tr>
	<tr><td>8877689391</td><td>4/25/2016</td><td>12986</td></tr>
	<tr><td>8877689391</td><td>4/26/2016</td><td>11101</td></tr>
	<tr><td>8877689391</td><td>4/27/2016</td><td>23629</td></tr>
	<tr><td>8877689391</td><td>4/28/2016</td><td>14890</td></tr>
	<tr><td>8877689391</td><td>4/29/2016</td><td> 9733</td></tr>
	<tr><td>8877689391</td><td>4/30/2016</td><td>27745</td></tr>
	<tr><td>8877689391</td><td>5/1/2016 </td><td>10930</td></tr>
	<tr><td>8877689391</td><td>5/2/2016 </td><td> 4790</td></tr>
	<tr><td>8877689391</td><td>5/3/2016 </td><td>10818</td></tr>
	<tr><td>8877689391</td><td>5/4/2016 </td><td>18193</td></tr>
	<tr><td>8877689391</td><td>5/5/2016 </td><td>14055</td></tr>
	<tr><td>8877689391</td><td>5/6/2016 </td><td>21727</td></tr>
	<tr><td>8877689391</td><td>5/7/2016 </td><td>12332</td></tr>
	<tr><td>8877689391</td><td>5/8/2016 </td><td>10686</td></tr>
	<tr><td>8877689391</td><td>5/9/2016 </td><td>20226</td></tr>
	<tr><td>8877689391</td><td>5/10/2016</td><td>10733</td></tr>
	<tr><td>8877689391</td><td>5/11/2016</td><td>21420</td></tr>
	<tr><td>8877689391</td><td>5/12/2016</td><td> 8064</td></tr>
</tbody>
</table>
</dd>
	<dt>$sleepDay_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 413 × 5</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>date</th><th scope=col>sleep_records</th><th scope=col>minutes_asleep</th><th scope=col>time_in_bed</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>4/12/2016 12:00:00 AM</td><td>1</td><td>327</td><td>346</td></tr>
	<tr><td>1503960366</td><td>4/13/2016 12:00:00 AM</td><td>2</td><td>384</td><td>407</td></tr>
	<tr><td>1503960366</td><td>4/15/2016 12:00:00 AM</td><td>1</td><td>412</td><td>442</td></tr>
	<tr><td>1503960366</td><td>4/16/2016 12:00:00 AM</td><td>2</td><td>340</td><td>367</td></tr>
	<tr><td>1503960366</td><td>4/17/2016 12:00:00 AM</td><td>1</td><td>700</td><td>712</td></tr>
	<tr><td>1503960366</td><td>4/19/2016 12:00:00 AM</td><td>1</td><td>304</td><td>320</td></tr>
	<tr><td>1503960366</td><td>4/20/2016 12:00:00 AM</td><td>1</td><td>360</td><td>377</td></tr>
	<tr><td>1503960366</td><td>4/21/2016 12:00:00 AM</td><td>1</td><td>325</td><td>364</td></tr>
	<tr><td>1503960366</td><td>4/23/2016 12:00:00 AM</td><td>1</td><td>361</td><td>384</td></tr>
	<tr><td>1503960366</td><td>4/24/2016 12:00:00 AM</td><td>1</td><td>430</td><td>449</td></tr>
	<tr><td>1503960366</td><td>4/25/2016 12:00:00 AM</td><td>1</td><td>277</td><td>323</td></tr>
	<tr><td>1503960366</td><td>4/26/2016 12:00:00 AM</td><td>1</td><td>245</td><td>274</td></tr>
	<tr><td>1503960366</td><td>4/28/2016 12:00:00 AM</td><td>1</td><td>366</td><td>393</td></tr>
	<tr><td>1503960366</td><td>4/29/2016 12:00:00 AM</td><td>1</td><td>341</td><td>354</td></tr>
	<tr><td>1503960366</td><td>4/30/2016 12:00:00 AM</td><td>1</td><td>404</td><td>425</td></tr>
	<tr><td>1503960366</td><td>5/1/2016 12:00:00 AM </td><td>1</td><td>369</td><td>396</td></tr>
	<tr><td>1503960366</td><td>5/2/2016 12:00:00 AM </td><td>1</td><td>277</td><td>309</td></tr>
	<tr><td>1503960366</td><td>5/3/2016 12:00:00 AM </td><td>1</td><td>273</td><td>296</td></tr>
	<tr><td>1503960366</td><td>5/5/2016 12:00:00 AM </td><td>1</td><td>247</td><td>264</td></tr>
	<tr><td>1503960366</td><td>5/6/2016 12:00:00 AM </td><td>1</td><td>334</td><td>367</td></tr>
	<tr><td>1503960366</td><td>5/7/2016 12:00:00 AM </td><td>1</td><td>331</td><td>349</td></tr>
	<tr><td>1503960366</td><td>5/8/2016 12:00:00 AM </td><td>1</td><td>594</td><td>611</td></tr>
	<tr><td>1503960366</td><td>5/9/2016 12:00:00 AM </td><td>1</td><td>338</td><td>342</td></tr>
	<tr><td>1503960366</td><td>5/10/2016 12:00:00 AM</td><td>1</td><td>383</td><td>403</td></tr>
	<tr><td>1503960366</td><td>5/11/2016 12:00:00 AM</td><td>1</td><td>285</td><td>306</td></tr>
	<tr><td>1644430081</td><td>4/29/2016 12:00:00 AM</td><td>1</td><td>119</td><td>127</td></tr>
	<tr><td>1644430081</td><td>4/30/2016 12:00:00 AM</td><td>1</td><td>124</td><td>142</td></tr>
	<tr><td>1644430081</td><td>5/2/2016 12:00:00 AM </td><td>1</td><td>796</td><td>961</td></tr>
	<tr><td>1644430081</td><td>5/8/2016 12:00:00 AM </td><td>1</td><td>137</td><td>154</td></tr>
	<tr><td>1844505072</td><td>4/15/2016 12:00:00 AM</td><td>1</td><td>644</td><td>961</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>8378563200</td><td>4/28/2016 12:00:00 AM</td><td>1</td><td>506</td><td>556</td></tr>
	<tr><td>8378563200</td><td>4/29/2016 12:00:00 AM</td><td>1</td><td>527</td><td>562</td></tr>
	<tr><td>8378563200</td><td>4/30/2016 12:00:00 AM</td><td>1</td><td>468</td><td>555</td></tr>
	<tr><td>8378563200</td><td>5/1/2016 12:00:00 AM </td><td>1</td><td>475</td><td>539</td></tr>
	<tr><td>8378563200</td><td>5/2/2016 12:00:00 AM </td><td>1</td><td>351</td><td>385</td></tr>
	<tr><td>8378563200</td><td>5/3/2016 12:00:00 AM </td><td>1</td><td>405</td><td>429</td></tr>
	<tr><td>8378563200</td><td>5/4/2016 12:00:00 AM </td><td>1</td><td>441</td><td>477</td></tr>
	<tr><td>8378563200</td><td>5/5/2016 12:00:00 AM </td><td>1</td><td>381</td><td>417</td></tr>
	<tr><td>8378563200</td><td>5/6/2016 12:00:00 AM </td><td>1</td><td>323</td><td>355</td></tr>
	<tr><td>8378563200</td><td>5/7/2016 12:00:00 AM </td><td>2</td><td>459</td><td>513</td></tr>
	<tr><td>8378563200</td><td>5/8/2016 12:00:00 AM </td><td>1</td><td>545</td><td>606</td></tr>
	<tr><td>8378563200</td><td>5/9/2016 12:00:00 AM </td><td>1</td><td>359</td><td>399</td></tr>
	<tr><td>8378563200</td><td>5/10/2016 12:00:00 AM</td><td>1</td><td>342</td><td>391</td></tr>
	<tr><td>8378563200</td><td>5/11/2016 12:00:00 AM</td><td>1</td><td>368</td><td>387</td></tr>
	<tr><td>8378563200</td><td>5/12/2016 12:00:00 AM</td><td>1</td><td>496</td><td>546</td></tr>
	<tr><td>8792009665</td><td>4/12/2016 12:00:00 AM</td><td>1</td><td>458</td><td>493</td></tr>
	<tr><td>8792009665</td><td>4/13/2016 12:00:00 AM</td><td>1</td><td>531</td><td>552</td></tr>
	<tr><td>8792009665</td><td>4/14/2016 12:00:00 AM</td><td>1</td><td>486</td><td>503</td></tr>
	<tr><td>8792009665</td><td>4/15/2016 12:00:00 AM</td><td>1</td><td>363</td><td>377</td></tr>
	<tr><td>8792009665</td><td>4/20/2016 12:00:00 AM</td><td>1</td><td>528</td><td>547</td></tr>
	<tr><td>8792009665</td><td>4/22/2016 12:00:00 AM</td><td>1</td><td>391</td><td>407</td></tr>
	<tr><td>8792009665</td><td>4/23/2016 12:00:00 AM</td><td>1</td><td>339</td><td>360</td></tr>
	<tr><td>8792009665</td><td>4/27/2016 12:00:00 AM</td><td>1</td><td>423</td><td>428</td></tr>
	<tr><td>8792009665</td><td>4/28/2016 12:00:00 AM</td><td>1</td><td>402</td><td>416</td></tr>
	<tr><td>8792009665</td><td>4/29/2016 12:00:00 AM</td><td>1</td><td>398</td><td>406</td></tr>
	<tr><td>8792009665</td><td>4/30/2016 12:00:00 AM</td><td>1</td><td>343</td><td>360</td></tr>
	<tr><td>8792009665</td><td>5/1/2016 12:00:00 AM </td><td>1</td><td>503</td><td>527</td></tr>
	<tr><td>8792009665</td><td>5/2/2016 12:00:00 AM </td><td>1</td><td>415</td><td>423</td></tr>
	<tr><td>8792009665</td><td>5/3/2016 12:00:00 AM </td><td>1</td><td>516</td><td>545</td></tr>
	<tr><td>8792009665</td><td>5/4/2016 12:00:00 AM </td><td>1</td><td>439</td><td>463</td></tr>
</tbody>
</table>
</dd>
</dl>




```python
# Now we can see how many unique users and days logged each data table has to ensure we have a large enough sample size
lapply(dt_list, function(dt) {
  dt[, .(unique_dates = uniqueN(date)), by = id]
})
```


<dl>
	<dt>$dailyActivity_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 35 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>49</td></tr>
	<tr><td>1624580081</td><td>49</td></tr>
	<tr><td>1644430081</td><td>40</td></tr>
	<tr><td>1844505072</td><td>42</td></tr>
	<tr><td>1927972279</td><td>42</td></tr>
	<tr><td>2022484408</td><td>42</td></tr>
	<tr><td>2026352035</td><td>42</td></tr>
	<tr><td>2320127002</td><td>42</td></tr>
	<tr><td>2347167796</td><td>32</td></tr>
	<tr><td>2873212765</td><td>42</td></tr>
	<tr><td>2891001357</td><td> 8</td></tr>
	<tr><td>3372868164</td><td>30</td></tr>
	<tr><td>3977333714</td><td>41</td></tr>
	<tr><td>4020332650</td><td>62</td></tr>
	<tr><td>4057192912</td><td>35</td></tr>
	<tr><td>4319703577</td><td>43</td></tr>
	<tr><td>4388161847</td><td>39</td></tr>
	<tr><td>4445114986</td><td>45</td></tr>
	<tr><td>4558609924</td><td>42</td></tr>
	<tr><td>4702921684</td><td>45</td></tr>
	<tr><td>5553957443</td><td>42</td></tr>
	<tr><td>5577150313</td><td>41</td></tr>
	<tr><td>6117666160</td><td>38</td></tr>
	<tr><td>6290855005</td><td>39</td></tr>
	<tr><td>6391747486</td><td> 9</td></tr>
	<tr><td>6775888955</td><td>35</td></tr>
	<tr><td>6962181067</td><td>44</td></tr>
	<tr><td>7007744171</td><td>37</td></tr>
	<tr><td>7086361926</td><td>42</td></tr>
	<tr><td>8053475328</td><td>41</td></tr>
	<tr><td>8253242879</td><td>30</td></tr>
	<tr><td>8378563200</td><td>42</td></tr>
	<tr><td>8583815059</td><td>39</td></tr>
	<tr><td>8792009665</td><td>40</td></tr>
	<tr><td>8877689391</td><td>42</td></tr>
</tbody>
</table>
</dd>
	<dt>$heartrate_seconds_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 15 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>2022484408</td><td>209056</td></tr>
	<tr><td>2026352035</td><td>  2929</td></tr>
	<tr><td>2347167796</td><td>273487</td></tr>
	<tr><td>4020332650</td><td>561120</td></tr>
	<tr><td>4558609924</td><td>259941</td></tr>
	<tr><td>5553957443</td><td>350549</td></tr>
	<tr><td>5577150313</td><td>336209</td></tr>
	<tr><td>6117666160</td><td>212565</td></tr>
	<tr><td>6391747486</td><td>  3747</td></tr>
	<tr><td>6775888955</td><td> 67871</td></tr>
	<tr><td>6962181067</td><td>387284</td></tr>
	<tr><td>7007744171</td><td>197393</td></tr>
	<tr><td>8792009665</td><td>190383</td></tr>
	<tr><td>8877689391</td><td>312633</td></tr>
	<tr><td>4388161847</td><td>249748</td></tr>
</tbody>
</table>
</dd>
	<dt>$hourlyCalories_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 35 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>1461</td></tr>
	<tr><td>1624580081</td><td>1480</td></tr>
	<tr><td>1644430081</td><td>1408</td></tr>
	<tr><td>1844505072</td><td>1475</td></tr>
	<tr><td>1927972279</td><td>1480</td></tr>
	<tr><td>2022484408</td><td>1480</td></tr>
	<tr><td>2026352035</td><td>1480</td></tr>
	<tr><td>2320127002</td><td>1479</td></tr>
	<tr><td>2347167796</td><td>1158</td></tr>
	<tr><td>2873212765</td><td>1476</td></tr>
	<tr><td>2891001357</td><td>  12</td></tr>
	<tr><td>3372868164</td><td>1183</td></tr>
	<tr><td>3977333714</td><td>1440</td></tr>
	<tr><td>4020332650</td><td>1476</td></tr>
	<tr><td>4057192912</td><td> 832</td></tr>
	<tr><td>4319703577</td><td>1395</td></tr>
	<tr><td>4445114986</td><td>1479</td></tr>
	<tr><td>4558609924</td><td>1480</td></tr>
	<tr><td>4702921684</td><td>1473</td></tr>
	<tr><td>5553957443</td><td>1474</td></tr>
	<tr><td>5577150313</td><td>1438</td></tr>
	<tr><td>6117666160</td><td>1355</td></tr>
	<tr><td>6290855005</td><td>1186</td></tr>
	<tr><td>6391747486</td><td> 675</td></tr>
	<tr><td>6775888955</td><td>1292</td></tr>
	<tr><td>6962181067</td><td>1476</td></tr>
	<tr><td>7007744171</td><td>1345</td></tr>
	<tr><td>7086361926</td><td>1477</td></tr>
	<tr><td>8053475328</td><td>1479</td></tr>
	<tr><td>8253242879</td><td>1171</td></tr>
	<tr><td>8378563200</td><td>1479</td></tr>
	<tr><td>8583815059</td><td>1384</td></tr>
	<tr><td>8792009665</td><td>1416</td></tr>
	<tr><td>8877689391</td><td>1479</td></tr>
	<tr><td>4388161847</td><td> 735</td></tr>
</tbody>
</table>
</dd>
	<dt>$hourlyIntensities_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 35 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>1461</td></tr>
	<tr><td>1624580081</td><td>1480</td></tr>
	<tr><td>1644430081</td><td>1408</td></tr>
	<tr><td>1844505072</td><td>1475</td></tr>
	<tr><td>1927972279</td><td>1480</td></tr>
	<tr><td>2022484408</td><td>1480</td></tr>
	<tr><td>2026352035</td><td>1480</td></tr>
	<tr><td>2320127002</td><td>1479</td></tr>
	<tr><td>2347167796</td><td>1158</td></tr>
	<tr><td>2873212765</td><td>1476</td></tr>
	<tr><td>2891001357</td><td>  12</td></tr>
	<tr><td>3372868164</td><td>1183</td></tr>
	<tr><td>3977333714</td><td>1440</td></tr>
	<tr><td>4020332650</td><td>1476</td></tr>
	<tr><td>4057192912</td><td> 832</td></tr>
	<tr><td>4319703577</td><td>1395</td></tr>
	<tr><td>4445114986</td><td>1479</td></tr>
	<tr><td>4558609924</td><td>1480</td></tr>
	<tr><td>4702921684</td><td>1473</td></tr>
	<tr><td>5553957443</td><td>1474</td></tr>
	<tr><td>5577150313</td><td>1438</td></tr>
	<tr><td>6117666160</td><td>1355</td></tr>
	<tr><td>6290855005</td><td>1186</td></tr>
	<tr><td>6391747486</td><td> 675</td></tr>
	<tr><td>6775888955</td><td>1292</td></tr>
	<tr><td>6962181067</td><td>1476</td></tr>
	<tr><td>7007744171</td><td>1345</td></tr>
	<tr><td>7086361926</td><td>1477</td></tr>
	<tr><td>8053475328</td><td>1479</td></tr>
	<tr><td>8253242879</td><td>1171</td></tr>
	<tr><td>8378563200</td><td>1479</td></tr>
	<tr><td>8583815059</td><td>1384</td></tr>
	<tr><td>8792009665</td><td>1416</td></tr>
	<tr><td>8877689391</td><td>1479</td></tr>
	<tr><td>4388161847</td><td> 735</td></tr>
</tbody>
</table>
</dd>
	<dt>$hourlySteps_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 35 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>1461</td></tr>
	<tr><td>1624580081</td><td>1480</td></tr>
	<tr><td>1644430081</td><td>1408</td></tr>
	<tr><td>1844505072</td><td>1475</td></tr>
	<tr><td>1927972279</td><td>1480</td></tr>
	<tr><td>2022484408</td><td>1480</td></tr>
	<tr><td>2026352035</td><td>1480</td></tr>
	<tr><td>2320127002</td><td>1479</td></tr>
	<tr><td>2347167796</td><td>1158</td></tr>
	<tr><td>2873212765</td><td>1476</td></tr>
	<tr><td>2891001357</td><td>  12</td></tr>
	<tr><td>3372868164</td><td>1183</td></tr>
	<tr><td>3977333714</td><td>1440</td></tr>
	<tr><td>4020332650</td><td>1476</td></tr>
	<tr><td>4057192912</td><td> 832</td></tr>
	<tr><td>4319703577</td><td>1395</td></tr>
	<tr><td>4445114986</td><td>1479</td></tr>
	<tr><td>4558609924</td><td>1480</td></tr>
	<tr><td>4702921684</td><td>1473</td></tr>
	<tr><td>5553957443</td><td>1474</td></tr>
	<tr><td>5577150313</td><td>1438</td></tr>
	<tr><td>6117666160</td><td>1355</td></tr>
	<tr><td>6290855005</td><td>1186</td></tr>
	<tr><td>6391747486</td><td> 675</td></tr>
	<tr><td>6775888955</td><td>1292</td></tr>
	<tr><td>6962181067</td><td>1476</td></tr>
	<tr><td>7007744171</td><td>1345</td></tr>
	<tr><td>7086361926</td><td>1477</td></tr>
	<tr><td>8053475328</td><td>1479</td></tr>
	<tr><td>8253242879</td><td>1171</td></tr>
	<tr><td>8378563200</td><td>1479</td></tr>
	<tr><td>8583815059</td><td>1384</td></tr>
	<tr><td>8792009665</td><td>1416</td></tr>
	<tr><td>8877689391</td><td>1479</td></tr>
	<tr><td>4388161847</td><td> 735</td></tr>
</tbody>
</table>
</dd>
	<dt>$weightLogInfo_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 13 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td> 3</td></tr>
	<tr><td>1927972279</td><td> 2</td></tr>
	<tr><td>2347167796</td><td> 1</td></tr>
	<tr><td>2873212765</td><td> 4</td></tr>
	<tr><td>2891001357</td><td> 1</td></tr>
	<tr><td>4445114986</td><td> 1</td></tr>
	<tr><td>4558609924</td><td> 6</td></tr>
	<tr><td>4702921684</td><td> 1</td></tr>
	<tr><td>6962181067</td><td>43</td></tr>
	<tr><td>8253242879</td><td> 1</td></tr>
	<tr><td>8877689391</td><td>32</td></tr>
	<tr><td>4319703577</td><td> 2</td></tr>
	<tr><td>5577150313</td><td> 1</td></tr>
</tbody>
</table>
</dd>
	<dt>$dailyCalories_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 33 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>31</td></tr>
	<tr><td>1624580081</td><td>31</td></tr>
	<tr><td>1644430081</td><td>30</td></tr>
	<tr><td>1844505072</td><td>31</td></tr>
	<tr><td>1927972279</td><td>31</td></tr>
	<tr><td>2022484408</td><td>31</td></tr>
	<tr><td>2026352035</td><td>31</td></tr>
	<tr><td>2320127002</td><td>31</td></tr>
	<tr><td>2347167796</td><td>18</td></tr>
	<tr><td>2873212765</td><td>31</td></tr>
	<tr><td>3372868164</td><td>20</td></tr>
	<tr><td>3977333714</td><td>30</td></tr>
	<tr><td>4020332650</td><td>31</td></tr>
	<tr><td>4057192912</td><td> 4</td></tr>
	<tr><td>4319703577</td><td>31</td></tr>
	<tr><td>4388161847</td><td>31</td></tr>
	<tr><td>4445114986</td><td>31</td></tr>
	<tr><td>4558609924</td><td>31</td></tr>
	<tr><td>4702921684</td><td>31</td></tr>
	<tr><td>5553957443</td><td>31</td></tr>
	<tr><td>5577150313</td><td>30</td></tr>
	<tr><td>6117666160</td><td>28</td></tr>
	<tr><td>6290855005</td><td>29</td></tr>
	<tr><td>6775888955</td><td>26</td></tr>
	<tr><td>6962181067</td><td>31</td></tr>
	<tr><td>7007744171</td><td>26</td></tr>
	<tr><td>7086361926</td><td>31</td></tr>
	<tr><td>8053475328</td><td>31</td></tr>
	<tr><td>8253242879</td><td>19</td></tr>
	<tr><td>8378563200</td><td>31</td></tr>
	<tr><td>8583815059</td><td>31</td></tr>
	<tr><td>8792009665</td><td>29</td></tr>
	<tr><td>8877689391</td><td>31</td></tr>
</tbody>
</table>
</dd>
	<dt>$dailyIntensities_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 33 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>31</td></tr>
	<tr><td>1624580081</td><td>31</td></tr>
	<tr><td>1644430081</td><td>30</td></tr>
	<tr><td>1844505072</td><td>31</td></tr>
	<tr><td>1927972279</td><td>31</td></tr>
	<tr><td>2022484408</td><td>31</td></tr>
	<tr><td>2026352035</td><td>31</td></tr>
	<tr><td>2320127002</td><td>31</td></tr>
	<tr><td>2347167796</td><td>18</td></tr>
	<tr><td>2873212765</td><td>31</td></tr>
	<tr><td>3372868164</td><td>20</td></tr>
	<tr><td>3977333714</td><td>30</td></tr>
	<tr><td>4020332650</td><td>31</td></tr>
	<tr><td>4057192912</td><td> 4</td></tr>
	<tr><td>4319703577</td><td>31</td></tr>
	<tr><td>4388161847</td><td>31</td></tr>
	<tr><td>4445114986</td><td>31</td></tr>
	<tr><td>4558609924</td><td>31</td></tr>
	<tr><td>4702921684</td><td>31</td></tr>
	<tr><td>5553957443</td><td>31</td></tr>
	<tr><td>5577150313</td><td>30</td></tr>
	<tr><td>6117666160</td><td>28</td></tr>
	<tr><td>6290855005</td><td>29</td></tr>
	<tr><td>6775888955</td><td>26</td></tr>
	<tr><td>6962181067</td><td>31</td></tr>
	<tr><td>7007744171</td><td>26</td></tr>
	<tr><td>7086361926</td><td>31</td></tr>
	<tr><td>8053475328</td><td>31</td></tr>
	<tr><td>8253242879</td><td>19</td></tr>
	<tr><td>8378563200</td><td>31</td></tr>
	<tr><td>8583815059</td><td>31</td></tr>
	<tr><td>8792009665</td><td>29</td></tr>
	<tr><td>8877689391</td><td>31</td></tr>
</tbody>
</table>
</dd>
	<dt>$dailySteps_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 33 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>31</td></tr>
	<tr><td>1624580081</td><td>31</td></tr>
	<tr><td>1644430081</td><td>30</td></tr>
	<tr><td>1844505072</td><td>31</td></tr>
	<tr><td>1927972279</td><td>31</td></tr>
	<tr><td>2022484408</td><td>31</td></tr>
	<tr><td>2026352035</td><td>31</td></tr>
	<tr><td>2320127002</td><td>31</td></tr>
	<tr><td>2347167796</td><td>18</td></tr>
	<tr><td>2873212765</td><td>31</td></tr>
	<tr><td>3372868164</td><td>20</td></tr>
	<tr><td>3977333714</td><td>30</td></tr>
	<tr><td>4020332650</td><td>31</td></tr>
	<tr><td>4057192912</td><td> 4</td></tr>
	<tr><td>4319703577</td><td>31</td></tr>
	<tr><td>4388161847</td><td>31</td></tr>
	<tr><td>4445114986</td><td>31</td></tr>
	<tr><td>4558609924</td><td>31</td></tr>
	<tr><td>4702921684</td><td>31</td></tr>
	<tr><td>5553957443</td><td>31</td></tr>
	<tr><td>5577150313</td><td>30</td></tr>
	<tr><td>6117666160</td><td>28</td></tr>
	<tr><td>6290855005</td><td>29</td></tr>
	<tr><td>6775888955</td><td>26</td></tr>
	<tr><td>6962181067</td><td>31</td></tr>
	<tr><td>7007744171</td><td>26</td></tr>
	<tr><td>7086361926</td><td>31</td></tr>
	<tr><td>8053475328</td><td>31</td></tr>
	<tr><td>8253242879</td><td>19</td></tr>
	<tr><td>8378563200</td><td>31</td></tr>
	<tr><td>8583815059</td><td>31</td></tr>
	<tr><td>8792009665</td><td>29</td></tr>
	<tr><td>8877689391</td><td>31</td></tr>
</tbody>
</table>
</dd>
	<dt>$sleepDay_merged</dt>
		<dd><table class="dataframe">
<caption>A data.table: 24 × 2</caption>
<thead>
	<tr><th scope=col>id</th><th scope=col>unique_dates</th></tr>
	<tr><th scope=col>&lt;int64&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1503960366</td><td>25</td></tr>
	<tr><td>1644430081</td><td> 4</td></tr>
	<tr><td>1844505072</td><td> 3</td></tr>
	<tr><td>1927972279</td><td> 5</td></tr>
	<tr><td>2026352035</td><td>28</td></tr>
	<tr><td>2320127002</td><td> 1</td></tr>
	<tr><td>2347167796</td><td>15</td></tr>
	<tr><td>3977333714</td><td>28</td></tr>
	<tr><td>4020332650</td><td> 8</td></tr>
	<tr><td>4319703577</td><td>26</td></tr>
	<tr><td>4388161847</td><td>23</td></tr>
	<tr><td>4445114986</td><td>28</td></tr>
	<tr><td>4558609924</td><td> 5</td></tr>
	<tr><td>4702921684</td><td>27</td></tr>
	<tr><td>5553957443</td><td>31</td></tr>
	<tr><td>5577150313</td><td>26</td></tr>
	<tr><td>6117666160</td><td>18</td></tr>
	<tr><td>6775888955</td><td> 3</td></tr>
	<tr><td>6962181067</td><td>31</td></tr>
	<tr><td>7007744171</td><td> 2</td></tr>
	<tr><td>7086361926</td><td>24</td></tr>
	<tr><td>8053475328</td><td> 3</td></tr>
	<tr><td>8378563200</td><td>31</td></tr>
	<tr><td>8792009665</td><td>15</td></tr>
</tbody>
</table>
</dd>
</dl>



#### All of the data tables have at least 30 unique users (desired minimum sample size) besides heart rate, sleep, and weight. Sleep is close at 24, so we'll keep it in for practice. Heart rate and weight are exceptionally low, so we'll exclude those from our analysis.


```python
dt_list <- dt_list[-c(2, 6)]
```

#### Next, we'll perform some checks.


```python
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
```


<dl>
	<dt>$dailyActivity_merged</dt>
		<dd>0</dd>
	<dt>$hourlyCalories_merged</dt>
		<dd>350</dd>
	<dt>$hourlyIntensities_merged</dt>
		<dd>350</dd>
	<dt>$hourlySteps_merged</dt>
		<dd>350</dd>
	<dt>$dailyCalories_merged</dt>
		<dd>0</dd>
	<dt>$dailyIntensities_merged</dt>
		<dd>0</dd>
	<dt>$dailySteps_merged</dt>
		<dd>0</dd>
	<dt>$sleepDay_merged</dt>
		<dd>6</dd>
</dl>




<dl>
	<dt>$dailyActivity_merged</dt>
		<dd><style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>steps</dt><dd>0</dd><dt>distance</dt><dd>0</dd><dt>tracker_distance</dt><dd>0</dd><dt>logged_activities_distance</dt><dd>0</dd><dt>very_active_distance</dt><dd>0</dd><dt>moderately_active_distance</dt><dd>0</dd><dt>light_active_distance</dt><dd>0</dd><dt>sedentary_active_distance</dt><dd>0</dd><dt>very_active_minutes</dt><dd>0</dd><dt>fairly_active_minutes</dt><dd>0</dd><dt>lightly_active_minutes</dt><dd>0</dd><dt>sedentary_minutes</dt><dd>0</dd><dt>calories</dt><dd>0</dd></dl>
</dd>
	<dt>$hourlyCalories_merged</dt>
		<dd><style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>calories</dt><dd>0</dd></dl>
</dd>
	<dt>$hourlyIntensities_merged</dt>
		<dd><style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>intensity</dt><dd>0</dd><dt>average_intensity</dt><dd>0</dd></dl>
</dd>
	<dt>$hourlySteps_merged</dt>
		<dd><style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>steps</dt><dd>0</dd></dl>
</dd>
	<dt>$dailyCalories_merged</dt>
		<dd><style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>calories</dt><dd>0</dd></dl>
</dd>
	<dt>$dailyIntensities_merged</dt>
		<dd><style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>sedentary_minutes</dt><dd>0</dd><dt>lightly_active_minutes</dt><dd>0</dd><dt>fairly_active_minutes</dt><dd>0</dd><dt>very_active_minutes</dt><dd>0</dd><dt>sedentary_active_distance</dt><dd>0</dd><dt>light_active_distance</dt><dd>0</dd><dt>moderately_active_distance</dt><dd>0</dd><dt>very_active_distance</dt><dd>0</dd></dl>
</dd>
	<dt>$dailySteps_merged</dt>
		<dd><style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>steps</dt><dd>0</dd></dl>
</dd>
	<dt>$sleepDay_merged</dt>
		<dd><style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>sleep_records</dt><dd>0</dd><dt>minutes_asleep</dt><dd>0</dd><dt>time_in_bed</dt><dd>0</dd></dl>
</dd>
</dl>



#### The daily activity table already contains information about calories, intensities, and steps. We need to check to see if it'those individual tables are redundant.


```python
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
```


<dl>
	<dt>$total_rows_y</dt>
		<dd>940</dd>
	<dt>$matching_rows</dt>
		<dd>940</dd>
	<dt>$percentage</dt>
		<dd>100</dd>
</dl>




<dl>
	<dt>$total_rows_y</dt>
		<dd>940</dd>
	<dt>$matching_rows</dt>
		<dd>940</dd>
	<dt>$percentage</dt>
		<dd>100</dd>
</dl>




<dl>
	<dt>$total_rows_y</dt>
		<dd>940</dd>
	<dt>$matching_rows</dt>
		<dd>940</dd>
	<dt>$percentage</dt>
		<dd>100</dd>
</dl>




```python
# All of the data is redundant so we can remove those tables
dt_list <- dt_list[-c(5:7)]
```


```python
# The last step before merging is to convert dates to consistent formats. This will be easier by splitting the data table list into daily and hourly lists.
daily_list <- dt_list[c(1, 5)]
hourly_list <- dt_list[2:4]

daily_list <- lapply(daily_list, function(dt) {
  dt[, date := parse_date_time(date, orders = c("mdy", "mdy HMS p"))]
}) # We need the more flexible parse_date_time() function to handle multiple formats

hourly_list <- lapply(hourly_list, function(dt) {
  dt[, date := mdy_hms(date)]
})
```


```python
# Now we can finally extract our daily and hourly data tables from the lists based on id and date
daily_activity <- Reduce(function(x, y) merge(x, y, all.x = TRUE, by = c("id", "date")), daily_list) # We want to use a left join here because we want the sleep data
hourly_activity <- Reduce(function(x, y) merge(x, y, all = TRUE, by = c("id", "date")), hourly_list) # We want an outer join here because all of the data is equally important for analysis
```


```python
# Check our work
summary(daily_activity)
summary(hourly_activity)
```


           id                  date                            steps      
     Min.   :1503960366   Min.   :2016-03-12 00:00:00.00   Min.   :    0  
     1st Qu.:2320127002   1st Qu.:2016-04-09 00:00:00.00   1st Qu.: 3146  
     Median :4445114986   Median :2016-04-19 00:00:00.00   Median : 6999  
     Mean   :4781210221   Mean   :2016-04-19 01:26:35.13   Mean   : 7281  
     3rd Qu.:6962181067   3rd Qu.:2016-04-30 00:00:00.00   3rd Qu.:10544  
     Max.   :8877689391   Max.   :2016-05-12 00:00:00.00   Max.   :36019  
                                                                          
        distance      tracker_distance logged_activities_distance
     Min.   : 0.000   Min.   : 0.000   Min.   :0.0000            
     1st Qu.: 2.170   1st Qu.: 2.160   1st Qu.:0.0000            
     Median : 4.950   Median : 4.950   Median :0.0000            
     Mean   : 5.219   Mean   : 5.192   Mean   :0.1315            
     3rd Qu.: 7.500   3rd Qu.: 7.480   3rd Qu.:0.0000            
     Max.   :28.030   Max.   :28.030   Max.   :6.7271            
                                                                 
     very_active_distance moderately_active_distance light_active_distance
     Min.   : 0.000       Min.   :0.0000             Min.   : 0.000       
     1st Qu.: 0.000       1st Qu.:0.0000             1st Qu.: 1.610       
     Median : 0.100       Median :0.2000             Median : 3.240       
     Mean   : 1.397       Mean   :0.5385             Mean   : 3.193       
     3rd Qu.: 1.830       3rd Qu.:0.7700             3rd Qu.: 4.690       
     Max.   :21.920       Max.   :6.4800             Max.   :12.510       
                                                                          
     sedentary_active_distance very_active_minutes fairly_active_minutes
     Min.   :0.000000          Min.   :  0.00      Min.   :  0.0        
     1st Qu.:0.000000          1st Qu.:  0.00      1st Qu.:  0.0        
     Median :0.000000          Median :  2.00      Median :  6.0        
     Mean   :0.001704          Mean   : 19.68      Mean   : 13.4        
     3rd Qu.:0.000000          3rd Qu.: 30.00      3rd Qu.: 18.0        
     Max.   :0.110000          Max.   :210.00      Max.   :660.0        
                                                                        
     lightly_active_minutes sedentary_minutes    calories    sleep_records  
     Min.   :  0.0          Min.   :   0.0    Min.   :   0   Min.   :1.000  
     1st Qu.:111.0          1st Qu.: 729.0    1st Qu.:1799   1st Qu.:1.000  
     Median :195.0          Median :1057.0    Median :2114   Median :1.000  
     Mean   :185.4          Mean   : 992.5    Mean   :2266   Mean   :1.123  
     3rd Qu.:262.0          3rd Qu.:1244.0    3rd Qu.:2770   3rd Qu.:1.000  
     Max.   :720.0          Max.   :1440.0    Max.   :4900   Max.   :3.000  
                                                             NA's   :975    
     minutes_asleep   time_in_bed   
     Min.   : 58.0   Min.   : 61.0  
     1st Qu.:361.0   1st Qu.:403.8  
     Median :432.5   Median :463.5  
     Mean   :419.9   Mean   :459.2  
     3rd Qu.:491.5   3rd Qu.:526.0  
     Max.   :796.0   Max.   :961.0  
     NA's   :975     NA's   :975    



           id                  date                           calories     
     Min.   :1503960366   Min.   :2016-03-12 00:00:00.00   Min.   : 42.00  
     1st Qu.:2320127002   1st Qu.:2016-03-26 12:00:00.00   1st Qu.: 62.00  
     Median :4558609924   Median :2016-04-10 10:30:00.00   Median : 80.00  
     Mean   :4869940441   Mean   :2016-04-10 17:02:57.23   Mean   : 95.82  
     3rd Qu.:6962181067   3rd Qu.:2016-04-25 16:00:00.00   3rd Qu.:106.00  
     Max.   :8877689391   Max.   :2016-05-12 15:00:00.00   Max.   :948.00  
       intensity      average_intensity     steps        
     Min.   :  0.00   Min.   :0.00000   Min.   :    0.0  
     1st Qu.:  0.00   1st Qu.:0.00000   1st Qu.:    0.0  
     Median :  2.00   Median :0.03333   Median :   21.0  
     Mean   : 11.42   Mean   :0.19037   Mean   :  302.9  
     3rd Qu.: 15.00   3rd Qu.:0.25000   3rd Qu.:  323.0  
     Max.   :180.00   Max.   :3.00000   Max.   :10565.0  



```python
# We know that steps and calories should probably not be zero or even close to zero, so let's remove any rows below 100 steps or 1000 calories as those were probably days that the wearable was not worn for the majority of the day.
daily_activity <- daily_activity[!(steps < 100 | calories < 1000)]
```

# 4. Analyze
## Data Transformation

### We'll create new columns for more granular analysis.


```python
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
daily_activity$intensity_score <- daily_activity$very_active_minutes * 2 + daily_activity$fairly_active_minutes + daily_activity$lightly_active_minutes * 0.5
```

### We can create a classification of the day's activity level based on steps. We'll get our category ranges from the Tudor-Locke and Bassett classification system.


```python
steps_breaks <- c(0, 5000, 7500, 10000, 12500, max(daily_activity[,steps]))
steps_labels <- c("Sedentary", "Physically Inactive", "Moderately Active", "Physically Active", "Very Active")

daily_activity[, daily_activity_category := cut(steps,
                                       breaks = steps_breaks,
                                       labels = steps_labels,
                                       include.lowest = TRUE)]

# Make sure that the categories are ordered correctly
daily_activity$daily_activity_category <- factor(daily_activity$daily_activity_category, levels = steps_labels)
```

### Next, we'll create new data tables with the averages of the metrics grouped by weekday, ID, and hour


```python
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
```


```python
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
```


```python
# Check that we didn't create any unwanted NA values (besides sleep data)
colSums(is.na(daily_activity))
colSums(is.na(hourly_activity))
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>steps</dt><dd>0</dd><dt>distance</dt><dd>0</dd><dt>tracker_distance</dt><dd>0</dd><dt>logged_activities_distance</dt><dd>0</dd><dt>very_active_distance</dt><dd>0</dd><dt>moderately_active_distance</dt><dd>0</dd><dt>light_active_distance</dt><dd>0</dd><dt>sedentary_active_distance</dt><dd>0</dd><dt>very_active_minutes</dt><dd>0</dd><dt>fairly_active_minutes</dt><dd>0</dd><dt>lightly_active_minutes</dt><dd>0</dd><dt>sedentary_minutes</dt><dd>0</dd><dt>calories</dt><dd>0</dd><dt>sleep_records</dt><dd>814</dd><dt>minutes_asleep</dt><dd>814</dd><dt>time_in_bed</dt><dd>814</dd><dt>weekday</dt><dd>0</dd><dt>percent_tracked</dt><dd>0</dd><dt>sleep_score</dt><dd>814</dd><dt>intensity_score</dt><dd>0</dd><dt>daily_activity_category</dt><dd>0</dd><dt>user_category</dt><dd>0</dd></dl>




<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>id</dt><dd>0</dd><dt>date</dt><dd>0</dd><dt>calories</dt><dd>0</dd><dt>intensity</dt><dd>0</dd><dt>average_intensity</dt><dd>0</dd><dt>steps</dt><dd>0</dd><dt>datetime</dt><dd>0</dd><dt>hour</dt><dd>0</dd><dt>weekday</dt><dd>0</dd><dt>user_category</dt><dd>0</dd></dl>



## Data Analysis


```python
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
```


         steps       very_active_minutes fairly_active_minutes
     Min.   :  108   Min.   :  0.00      Min.   :  0.00       
     1st Qu.: 4790   1st Qu.:  0.00      1st Qu.:  0.00       
     Median : 7891   Median :  6.00      Median :  8.00       
     Mean   : 8302   Mean   : 22.39      Mean   : 14.77       
     3rd Qu.:11100   3rd Qu.: 34.00      3rd Qu.: 21.00       
     Max.   :36019   Max.   :210.00      Max.   :143.00       
                                                              
     lightly_active_minutes sedentary_minutes    calories    minutes_asleep
     Min.   :  0.0          Min.   :  99.0    Min.   :1002   Min.   : 58   
     1st Qu.:147.0          1st Qu.: 724.0    1st Qu.:1878   1st Qu.:361   
     Median :212.0          Median :1018.0    Median :2225   Median :432   
     Mean   :211.5          Mean   : 958.6    Mean   :2378   Mean   :419   
     3rd Qu.:272.0          3rd Qu.:1188.0    3rd Qu.:2839   3rd Qu.:490   
     Max.   :720.0          Max.   :1440.0    Max.   :4900   Max.   :796   
                                                             NA's   :814   
      time_in_bed    percent_tracked   sleep_score     intensity_score
     Min.   : 61.0   Min.   :0.2146   Min.   :0.4984   Min.   :  0.0  
     1st Qu.:402.5   1st Qu.:0.6840   1st Qu.:0.9119   1st Qu.:102.0  
     Median :462.0   Median :0.9396   Median :0.9431   Median :163.0  
     Mean   :458.3   Mean   :0.8384   Mean   :0.9165   Mean   :165.3  
     3rd Qu.:526.0   3rd Qu.:1.0000   3rd Qu.:0.9606   3rd Qu.:218.5  
     Max.   :961.0   Max.   :1.0000   Max.   :1.0000   Max.   :555.5  
     NA's   :814                      NA's   :814                     
                 user_category
     Sedentary          :188  
     Physically Inactive:362  
     Moderately Active  :316  
     Physically Active  :272  
     Very Active        : 83  
                              
                              


* Average steps is 8302, below the recommended amount of 10000, but well above the American average of 3000 - 4000 (according to the Mayo Clinic).
* Average intensity score is 167.6 which is far above the recommended amount of 22.
* Average sedentary time is 958 minutes or 15.97 hours per day, but the max is 1440 minutes, so we assume that sedentary time includes time in bed.
 * We unfortunately don't have enough sleep data to accurately determine how much of the sedentary minutes occurred while the user was awake.
* We can see that most users use their device for over 83%. 100% would be better, but we can still glean insight.
* Users sleep 419 minutes or just about 7 hours on average.
 * The National Sleep institute recommends 7-9 hours of sleep, so most users are hitting the minimum, which is better than over a third of Americans.
 * A WebMD survey (one of the few that distuingishes time asleep from time in bed) found that on average, Americans spend only 5.7 hours asleep while spending 7.67 hours in bed (our users' averages are 6.98 hours alsleep and 7.63 hours in bed).


```python
summary(hourly_activity)
```


           id                  date               calories        intensity     
     Min.   :1503960366   Min.   :2016-03-12   Min.   : 42.00   Min.   :  0.00  
     1st Qu.:2320127002   1st Qu.:2016-03-26   1st Qu.: 62.00   1st Qu.:  0.00  
     Median :4558609924   Median :2016-04-10   Median : 80.00   Median :  2.00  
     Mean   :4869940441   Mean   :2016-04-10   Mean   : 95.82   Mean   : 11.42  
     3rd Qu.:6962181067   3rd Qu.:2016-04-25   3rd Qu.:106.00   3rd Qu.: 15.00  
     Max.   :8877689391   Max.   :2016-05-12   Max.   :948.00   Max.   :180.00  
                                                                                
     average_intensity     steps           datetime              hour      
     Min.   :0.00000   Min.   :    0.0   Length:46008       Min.   : 0.00  
     1st Qu.:0.00000   1st Qu.:    0.0   Class :character   1st Qu.: 5.00  
     Median :0.03333   Median :   21.0   Mode  :character   Median :11.00  
     Mean   :0.19037   Mean   :  302.9                      Mean   :11.45  
     3rd Qu.:0.25000   3rd Qu.:  323.0                      3rd Qu.:17.00  
     Max.   :3.00000   Max.   :10565.0                      Max.   :23.00  
                                                                           
          weekday                 user_category  
     Sunday   :6659   Sedentary          : 8848  
     Monday   :6581   Physically Inactive:13608  
     Tuesday  :6756   Moderately Active  :11499  
     Wednesday:6691   Physically Active  : 9095  
     Thursday :6427   Very Active        : 2958  
     Friday   :6134                              
     Saturday :6760                              


* The range between the minimum and 1st quartile of calorie burn is 20, while the range between the 3rd quartile and the maxiumum is 842.
 * There is a similar result in intensity minutes and steps.
 * There are relatively very few hours of high calorie burn, lots of intensity minutes, and high step count, which makes sense thinking about the average person's day. One can only exercise for so many hours in the day.
* (Hourly) intensity is measured in minutes, but we don't have data on the level of intensity for those minutes.

#### Next, we'll try and find any relationships between metrics.


```python
paste("Steps vs. Calories R-Value:", cor(daily_activity$steps, daily_activity$calories))
paste("Minutes Asleep vs. Time in Bed R-Value:", cor(daily_activity$minutes_asleep, daily_activity$time_in_bed, use = "complete.obs"))
paste("Minutes Asleep vs. Sedentary Minutes R-Value:", cor(daily_activity$minutes_asleep, daily_activity$sedentary_minutes, use = "complete.obs"))
```


'Steps vs. Calories R-Value: 0.531874541288099'



'Minutes Asleep vs. Time in Bed R-Value: 0.930333945828331'



'Minutes Asleep vs. Sedentary Minutes R-Value: -0.640616673746269'


Our assumption that sedentary minutes includes minutes asleep means that we'd expect a strong positive correlation and, in fact, we see a strong negative correlation. This data tells us that the more sedentary minutes a user has in the day, the more minutes they spend sleeping.


```python
# In order to control for time in bed, we check the relationship between sleep score and sedentary minutes
paste("Sleep Score vs. Sedentary Minutes R-Value:",cor(daily_activity$sleep_score, daily_activity$sedentary_minutes, use = "complete.obs"))

# And then to confirm once more, we'll check minutes asleep vs. intensity score, which is the inverse of sedentary minutes
paste("Minutes Asleep vs. Intensity Score R-Value:",cor(daily_activity$minutes_asleep, daily_activity$intensity_score, use = "complete.obs"))
```


'Sleep Score vs. Sedentary Minutes R-Value: 0.020916194471464'



'Minutes Asleep vs. Intensity Score R-Value: -0.129673010454482'


This is a great example of how correlation doesn't necessarily equal causation. Sedentary minutes may be correlated with less sleep, but sedentary minutes is not correlated with *how well* one sleeps. Inversely, higher intensity throughout the day has little correlation with less sleep. Rather than trying to reduce sedentary minutes to improve their sleep, users would probably be better served by increasing their total time in bed, either by going to bed earlier or sleeping in later.


```python
print(weekday_avgs)
```

         weekday    steps distance tracker_distance logged_activities_distance
          <fctr>    <num>    <num>            <num>                      <num>
    1:    Sunday 7626.826 5.500000         5.500000                 0.00000000
    2:    Monday 8489.210 6.047964         6.017964                 0.23363385
    3:   Tuesday 8667.834 6.213149         6.140442                 0.26654663
    4: Wednesday 8344.341 6.036648         6.012458                 0.17818268
    5:  Thursday 8297.541 5.937000         5.904176                 0.16505856
    6:    Friday 7858.423 5.622582         5.596813                 0.14498931
    7:  Saturday 8814.686 6.300057         6.270057                 0.02997978
       very_active_distance moderately_active_distance light_active_distance
                      <num>                      <num>                 <num>
    1:             1.579581                  0.6262275              3.196647
    2:             1.627246                  0.5892814              3.735928
    3:             1.655912                  0.6188398              3.777680
    4:             1.702737                  0.6111732              3.635419
    5:             1.551765                  0.5931765              3.632529
    6:             1.416978                  0.5636813              3.595604
    7:             1.634971                  0.7001143              3.888571
       sedentary_active_distance very_active_minutes fairly_active_minutes
                           <num>               <num>                 <num>
    1:               0.001017964            20.77844              14.49701
    2:               0.002335329            24.32335              14.29341
    3:               0.001657459            24.30387              15.49171
    4:               0.001508380            21.94413              14.87151
    5:               0.003235294            21.65882              14.34706
    6:               0.002032967            20.30220              13.67033
    7:               0.001885714            23.47429              16.16571
       lightly_active_minutes sedentary_minutes calories sleep_records
                        <num>             <num>    <num>         <num>
    1:               196.3234          944.9162 2321.707      1.181818
    2:               207.8802          982.7605 2373.922      1.108696
    3:               217.1271          957.8564 2417.547      1.106061
    4:               210.0000          955.4469 2388.620      1.151515
    5:               206.6412          948.4235 2329.835      1.032787
    6:               212.8187          992.5989 2367.489      1.071429
    7:               228.1829          927.2686 2440.897      1.192982
       minutes_asleep time_in_bed percent_tracked sleep_score intensity_score
                <num>       <num>           <num>       <num>           <num>
    1:       452.7455    503.5091       0.8170243   0.9050898        154.2156
    2:       419.5000    457.3478       0.8536510   0.9196862        166.8802
    3:       403.5303    441.9697       0.8435965   0.9115313        172.6630
    4:       434.6818    470.0303       0.8349046   0.9212852        163.7598
    5:       400.4426    434.4426       0.8271324   0.9217072        160.9853
    6:       405.3214    444.5536       0.8606876   0.9209848        160.6841
    7:       419.0702    459.8421       0.8299246   0.9149787        177.2057


* Saturday has the highest average steps, fairly active minutes, lightly active minutes, calories burned, and intensity score.
 * This makes sense considering Saturdays involve lots of free time without work, but also without the pressure to prepare for the week/rest on Sunday.
* Tuesday has the highest average very active minutes and is a close second behind Saturday in all of the metrics that Saturday leads in.
 * Tuesdays might seem like an odd day for lots of exercise, but in the context of Monday being consistently the third highest average day for steps, activity, and calorie burn, I'd guess that people try and start their week off on a positive note with some exercise, similar to the gym sign-up phenomenon after New Years.
* Sunday consistently has the lowest average metrics of activity, which makes sense given the concept of Sunday being a day of rest.
* Sunday also has the highest time in bed and time asleep.
 * We can infer that people want to get a good night's sleep before the week ahead and there usually aren't many activities to do on Sunday nights.
* Sunday also has the lowest average sleep score meaning that users may increase their time in bed, but there is not an equivalent increase in time asleep.
 * This means that there is an inflection point where beyond it, users shouldn't try and spend more time in bed and expect continual gains in their time asleep.

### We can perform a similar analysis on hourly averages, but with 24 hours in the day, it might be easier to glean insight from vizualizations.

# 5. Share
## Data Visualization

### General plots
These plots include the correlation tests we conducted in the Analyze phase.


```python
# Plot categories on a pie graph
ggplot(daily_activity, aes(x = "", fill = user_category)) + 
    geom_bar(width = 1) +
    coord_polar("y", start = 0) +
    theme_void()
```


    
![png](output_46_0.png)
    



```python
# Plot total steps vs. calories
ggplot(daily_activity, aes(x = steps, y = calories)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title="Daily Steps vs. Calories")
```

    [1m[22m`geom_smooth()` using formula = 'y ~ x'



    
![png](output_47_1.png)
    



```python
# Plot time asleep vs. total time in bed
ggplot(daily_activity, aes(x = minutes_asleep, y = time_in_bed)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title="Minutes Asleep vs. Time in Bed")
```

    [1m[22m`geom_smooth()` using formula = 'y ~ x'
    Warning message:
    “[1m[22mRemoved 814 rows containing non-finite outside the scale range
    (`stat_smooth()`).”
    Warning message:
    “[1m[22mRemoved 814 rows containing missing values or values outside the scale range
    (`geom_point()`).”



    
![png](output_48_1.png)
    



```python
# Plot time asleep vs. sedentary minutes
ggplot(daily_activity, aes(x = minutes_asleep, y = sedentary_minutes)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Minutes Asleep vs. Sedentary Minutes")
```

    [1m[22m`geom_smooth()` using formula = 'y ~ x'
    Warning message:
    “[1m[22mRemoved 814 rows containing non-finite outside the scale range
    (`stat_smooth()`).”
    Warning message:
    “[1m[22mRemoved 814 rows containing missing values or values outside the scale range
    (`geom_point()`).”



    
![png](output_49_1.png)
    



```python
# In order to control for time in bed, we check the relationship between sleep score and sedentary minutes
ggplot(daily_activity, aes(x = sleep_score, y = sedentary_minutes)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Sleep Score vs. Sedentary Minutes")
```

    [1m[22m`geom_smooth()` using formula = 'y ~ x'
    Warning message:
    “[1m[22mRemoved 814 rows containing non-finite outside the scale range
    (`stat_smooth()`).”
    Warning message:
    “[1m[22mRemoved 814 rows containing missing values or values outside the scale range
    (`geom_point()`).”



    
![png](output_50_1.png)
    



```python
# And then to confirm once more, we'll check minutes asleep vs. intensity score
ggplot(daily_activity, aes(x = minutes_asleep, y = intensity_score)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Minutes Asleep vs. Intensity Score")
```

    [1m[22m`geom_smooth()` using formula = 'y ~ x'
    Warning message:
    “[1m[22mRemoved 814 rows containing non-finite outside the scale range
    (`stat_smooth()`).”
    Warning message:
    “[1m[22mRemoved 814 rows containing missing values or values outside the scale range
    (`geom_point()`).”



    
![png](output_51_1.png)
    


### Average Data vs. Weekday


```python
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
```

    Warning message in geom_histogram(stat = "identity", fill = "skyblue"):
    “[1m[22mIgnoring unknown parameters: `binwidth`, `bins`, and `pad`”
    Warning message in geom_histogram(stat = "identity", fill = "skyblue"):
    “[1m[22mIgnoring unknown parameters: `binwidth`, `bins`, and `pad`”



    
![png](output_53_1.png)
    


    Warning message in geom_histogram(stat = "identity", fill = "skyblue"):
    “[1m[22mIgnoring unknown parameters: `binwidth`, `bins`, and `pad`”



    
![png](output_53_3.png)
    


    Warning message in geom_histogram(stat = "identity", fill = "skyblue"):
    “[1m[22mIgnoring unknown parameters: `binwidth`, `bins`, and `pad`”



    
![png](output_53_5.png)
    



    
![png](output_53_6.png)
    


We can see that none of the averages quite reach the recommended 10000 steps or 480 minutes of sleep per day, yet the daily recommendation of ~22 minutes of intense minutes is easily cleared on each weekday. Even the above-and-beyond recommendation of ~44 minutes per day, is about a third of what each user averages per day. This tells us that either the recommendation is too low or FitBit defines active minutes differently than the American Heart Association.

### Data vs. Hour


```python
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
```

    Warning message in geom_histogram(stat = "identity", fill = "skyblue"):
    “[1m[22mIgnoring unknown parameters: `binwidth`, `bins`, and `pad`”
    Warning message in geom_histogram(stat = "identity", fill = "skyblue"):
    “[1m[22mIgnoring unknown parameters: `binwidth`, `bins`, and `pad`”



    
![png](output_56_1.png)
    


    Warning message in geom_histogram(stat = "identity", fill = "skyblue"):
    “[1m[22mIgnoring unknown parameters: `binwidth`, `bins`, and `pad`”



    
![png](output_56_3.png)
    



    
![png](output_56_4.png)
    


* All three metrics reach their minimums overnight, but while intensity and steps bottom out near zero, calories still remains at a moderate level.
 * Our bodies obviously don't take steps or perform activity while we sleep overnight, but they do maintain a basal metabolic rate of calorie burn even when we're sleeping.
* Users are most active immediately around 6pm and 7pm (after work).
* The second most active time of day is the early afternoon (lunch time).
 * We can infer that most of the users work full-time jobs.

### We can plot multiple columns against time by pivoting our data to long format. The variation between weekdays wasn't as pronounced as between hours, so let's try it with our hourly average data.


```python
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
```


<table class="dataframe">
<caption>A matrix: 3 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>steps</th><th scope=col>calories</th><th scope=col>intensity</th></tr>
</thead>
<tbody>
	<tr><th scope=row>steps</th><td>1.0000000</td><td>0.9924138</td><td>0.9948438</td></tr>
	<tr><th scope=row>calories</th><td>0.9924138</td><td>1.0000000</td><td>0.9989693</td></tr>
	<tr><th scope=row>intensity</th><td>0.9948438</td><td>0.9989693</td><td>1.0000000</td></tr>
</tbody>
</table>




    
![png](output_59_1.png)
    


We can clearly see an almost perfect correlation between all 3, with values lowest in the early morning, rising steadily towards midday, with an early evening spike. Calories remain relatively higher overnight because we continue to burn calories while we sleep whereas steps and intensity should be minimal.

### Next we'll plot the different activity intensity levels against calories using the same pivoting method.


```python
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
```

    [1m[22m`geom_smooth()` using formula = 'y ~ x'



<table class="dataframe">
<caption>A matrix: 4 × 4 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>calories</th><th scope=col>lightly_active_minutes</th><th scope=col>fairly_active_minutes</th><th scope=col>very_active_minutes</th></tr>
</thead>
<tbody>
	<tr><th scope=row>calories</th><td>1.0000000</td><td> 0.13806884</td><td>0.28758920</td><td> 0.58634050</td></tr>
	<tr><th scope=row>lightly_active_minutes</th><td>0.1380688</td><td> 1.00000000</td><td>0.06412997</td><td>-0.04098602</td></tr>
	<tr><th scope=row>fairly_active_minutes</th><td>0.2875892</td><td> 0.06412997</td><td>1.00000000</td><td> 0.28572058</td></tr>
	<tr><th scope=row>very_active_minutes</th><td>0.5863405</td><td>-0.04098602</td><td>0.28572058</td><td> 1.00000000</td></tr>
</tbody>
</table>




    
![png](output_61_2.png)
    


We can that all active minutes are positively correlated with calories, with correlations strengthening as the intensity of thoe minutes increases.

## Export Data
### We can visualize multiple plots even more powerfully with Tableau, so let's write our data to csv files and transfer them to Tableau.


```python
#Export to CSV
write.csv(daily_activity, file = 'fitbit_daily_activity_03122016_05122016.csv')
write.csv(hourly_activity, file = 'fitbit_hourly_activity_03122016_05122016.csv')
write.csv(daily_activity_intensity_long, file = 'fitbit_daily_activity_intensity_long_03122016_05122016.csv')
```

# 6. Act

### Tableau Dashboards
Now that we've migrated our data to Tableau, we can create cleaner and more descriptive visualizations.

## [User Categories](https://public.tableau.com/views/BellabeatCaseStudy-UserCategories/UserCategories?:language=en-US&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)

Our first dashboard is a breakdown of our User Categories that visualizes the data by user type:

<div class='tableauPlaceholder' id='viz1729791998047' style='position: relative'><noscript><a href='#'><img alt='User Categories ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Be&#47;BellabeatCaseStudy-UserCategories&#47;UserCategories&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='BellabeatCaseStudy-UserCategories&#47;UserCategories' /><param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Be&#47;BellabeatCaseStudy-UserCategories&#47;UserCategories&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /><param name='language' value='en-US' /></object></div>                <script type='text/javascript'>                    var divElement = document.getElementById('viz1729791998047');                    var vizElement = divElement.getElementsByTagName('object')[0];                    if ( divElement.offsetWidth > 800 ) { vizElement.style.width='1366px';vizElement.style.height='795px';} else if ( divElement.offsetWidth > 500 ) { vizElement.style.width='1366px';vizElement.style.height='795px';} else { vizElement.style.width='100%';vizElement.style.height='1227px';}                     var scriptElement = document.createElement('script');                    scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    vizElement.parentNode.insertBefore(scriptElement, vizElement);                </script>


* We can see that the users are fairly spread out amongst the categories (Tudor-Locke and Bassett classification system), aside from the 2 users in the 'Very Active' category.
* We've replotted a couple of graphs that we plotted using R, but now we we've colored each data point based on that user's category.
 * Since the categories are based on steps, we'd expect the colors to dark as steps increase, but we can also see clearly defined areas of the graph along the Calories and Intensity Score axes.
 * The R-Squared value of Intensity Score vs. Calories is ~.39 while Steps vs. Calories is ~.29, so increasing intensity score is more beneficial than taking more steps.
* Unsurprisingly, there is a steady increase in steps and intensity score as the user category level increases, but there are also some interesting measures.
 * The Moderately Active group burned more calories on average the the Physically Active group.
  * This might give us some insight into why the Moderately Active group burned more calories on average. The Very Active Minutes may be the key to higher calorie burn.
 * The Very Active group has some of the least lightly and fairly active minutes, while having more than double the very active minutes as any other group.

## [Intensity Breakdown](https://public.tableau.com/views/BellabeatCaseStudy-IntensityBreakdown/IntensityBreakdown?:language=en-US&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)

Our next dashboard is a side-by-side look at the relationship between the intensity level of active minutes and calories, similar to the one we plotted with R, but this time adding steps.

<div class='tableauPlaceholder' id='viz1729792836209' style='position: relative'><noscript><a href='#'><img alt='Intensity Breakdown ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Be&#47;BellabeatCaseStudy-IntensityBreakdown&#47;IntensityBreakdown&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='BellabeatCaseStudy-IntensityBreakdown&#47;IntensityBreakdown' /><param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Be&#47;BellabeatCaseStudy-IntensityBreakdown&#47;IntensityBreakdown&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /><param name='language' value='en-US' /></object></div>                <script type='text/javascript'>                    var divElement = document.getElementById('viz1729792836209');                    var vizElement = divElement.getElementsByTagName('object')[0];                    if ( divElement.offsetWidth > 800 ) { vizElement.style.width='1366px';vizElement.style.height='795px';} else if ( divElement.offsetWidth > 500 ) { vizElement.style.width='1366px';vizElement.style.height='795px';} else { vizElement.style.width='100%';vizElement.style.height='727px';}                     var scriptElement = document.createElement('script');                    scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    vizElement.parentNode.insertBefore(scriptElement, vizElement);                </script>

* In all cases, we can see that an increase in any active minutes is associated with an increase in calories and steps.
* What's interesting is that we can see the correlation (trend line) between active minutes and calories/steps increase as the intensity of those active minutes increases.
 * This further supports that observation that the Moderately Active group burned more calories on average than the higher intensity Physically Active group because they had more very active minutes.

## [Averages by Day/Time](https://public.tableau.com/views/BellabeatCaseStudy-AveragesbyDayTime/AveragesbyDayTime?:language=en-US&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)

Next we can look at our average data grouped by day of the week and time instead of user category. These plots were also already done using R, but are now presented with easier to read labels, color coding, annotations, and general formatting. The most interesting difference from our R generated plots is the separation of weekday and weekend plots.

<div class='tableauPlaceholder' id='viz1729792814664' style='position: relative'><noscript><a href='#'><img alt='Averages by Day&#47;Time ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Be&#47;BellabeatCaseStudy-AveragesbyDayTime&#47;AveragesbyDayTime&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='BellabeatCaseStudy-AveragesbyDayTime&#47;AveragesbyDayTime' /><param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Be&#47;BellabeatCaseStudy-AveragesbyDayTime&#47;AveragesbyDayTime&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /><param name='language' value='en-US' /></object></div>                <script type='text/javascript'>                    var divElement = document.getElementById('viz1729792814664');                    var vizElement = divElement.getElementsByTagName('object')[0];                    if ( divElement.offsetWidth > 800 ) { vizElement.style.width='1366px';vizElement.style.height='795px';} else if ( divElement.offsetWidth > 500 ) { vizElement.style.width='1366px';vizElement.style.height='795px';} else { vizElement.style.width='100%';vizElement.style.height='1977px';}                     var scriptElement = document.createElement('script');                    scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    vizElement.parentNode.insertBefore(scriptElement, vizElement);                </script>

* Our Weekdays plot looks a lot similar to our hourly averages plot, which makes sense given that there are only 2 weekend days and 5 weekday days.
* There is still a peak from 5pm to 7pm indicating a preference for post work rather than pre work exercise.
* Our Weekends plot shows a much more gradual increase in activity until a 2pm peak.
* While activity very gently and gradually declines over the second half of the day on weekends, there is a small spike at 7pm.
 * This might users engaging in evening exercise, keeping with their weekday schedule.
 * It might also be users partaking in the evening activities that generally accompany weekends, like going out.

## [Sleep Data](https://public.tableau.com/views/BellabeatCaseStudy-SleepData/SleepData?:language=en-US&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)

And last, but not least, we have our sleep data. We'll end with this one remembering that we only had 8 users, so our data isn't the most robust and is highly susceptible to skewing. This dashboard presents the sleep data relative to user category.

<div class='tableauPlaceholder' id='viz1729792844748' style='position: relative'><noscript><a href='#'><img alt='Sleep Data ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Be&#47;BellabeatCaseStudy-SleepData&#47;SleepData&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='BellabeatCaseStudy-SleepData&#47;SleepData' /><param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Be&#47;BellabeatCaseStudy-SleepData&#47;SleepData&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /><param name='language' value='en-US' /></object></div>                <script type='text/javascript'>                    var divElement = document.getElementById('viz1729792844748');                    var vizElement = divElement.getElementsByTagName('object')[0];                    if ( divElement.offsetWidth > 800 ) { vizElement.style.width='1366px';vizElement.style.height='795px';} else if ( divElement.offsetWidth > 500 ) { vizElement.style.width='1366px';vizElement.style.height='795px';} else { vizElement.style.width='100%';vizElement.style.height='1327px';}                     var scriptElement = document.createElement('script');                    scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    vizElement.parentNode.insertBefore(scriptElement, vizElement);                </script>

* By plotting Steps vs. Sleep Score, I wanted to see if there was any relationship between a user's activity and their sleep, even when separating the user categories. At first it seemed like there was none, until I plotted average sleep scores by user category.
 * There was an extremely small, but steady percentage increase in sleep score going from category to category except for the the Physically Active Category, which had a relatively large decrease.
 * I then color coded the data points on the original plot and realized that there were a large amount of Physically Active data points - they were all from User ID: 3977333714
 * Adding a box plot onto the graph, we can see that all of that user's data points fell well below the lower whisker.
* I decided to replot everything filtering out the outliers.
 * We can see that the averages have leveled out much more gently.
  * The percentage increase is really only significant in the Very Active group, but that group only contains 2 people (and possibly only 1 with sleep data).
 * On the scatter plot we can see some very interesting trends.
  * As the user category increases, so does the R-Squared value (with the exception of the very small Very Active group).
   * It would seem that users who are generally more active are more resilient in their ability to sleep after higher activity days.
   * Sedentary and Physically Inactive users tend to not sleep well after a high step count day whereas Moderately Active and Physically Active users actually sleep a little better. 
   * While the Very Active group sleeps a little worse the more steps they take, they still sleep far better than the other groups.
  * The trend lines of the first four categories all seem to intersect around 3500 steps and a sleep score of 94%.
   * We can't say exactly why from this data alone, but what the data tells us is that if any user (besides a Very Active user) has a 3500 step day, they will also have a 94% sleep score night (and vice versa)
   * We can assume that the steps from the day are more likely to cause the sleep score because they come first, but we also can't conclude that steps affect sleep score in general.

## Conclusion

### As a reminder, the business task was to analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smart devices, while answering three specific questions:
1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?

#### 1. What are some trends in smart device usage?

* Users are segmented fairly evenly by activity level
* Users are significantly more active than the average American
* Users work 9-5 jobs
* Users prefer evening exercise
* Higher intensity level is more beneficial than more steps, although the two are strongly correlated anyways
* Saturdays, Mondays, and Tuesdays are the most active days
* Sundays, Thursdays, and Fridays are the least active days
* All users consistently surpass the AHA's daily recommendation of 22 active minutes (and even the above-and-beyond recommendation of 43) by a large amount
* Users who were generally less active had their sleep negatively affected by sporadic, high activity days
* The inverse is also true, where users who were generally more active slept better after high activity days
* The strongest indicator of more time asleep was more time in bed

#### 2. How could these trends apply to Bellabeat customers?

Bellabeat customers could benefit from the following:
* Dynamic goals rather than static ones - Sundays are low activity days, so the Bellabeat app could recommend a step or active minute goal based on the user's Sunday average steps or active minutes
* Gamified Intensity Score - Presenting the idea of Very Active Minutes being more "valuable" than Fairly or Lightly Active Minutes
* Bedtime Reminders - If users want to sleep more, the best way to do that is to go to bed earlier
* Daily Recaps - A nightly summary of the day's activity, which can also provide recommendations for sleep duration or activity level for the following day(s)

#### 3. How could these trends help influence Bellabeat marketing strategy?

We know that Bellabeat's mission is to use "beautifully designed [wellness] technology to inform and inspire women." Based on that information, we can narrow our target demographic to women who might not have high level knowledge of their own health, but value insightful information and most importantly, want to use that information to improve their own health.
* We know that FitBit users are probably full time employees, who prefer to exercise after work, so one strategy we could employ would be to target potential customers during the post work commute. Whether that's traditional advertising on high traffic roads or in public transportation spaces, we could present Bellabeat products as a way to take control of their health after a long day of sedentary work.
* Bellabeat focuses more on digital marketing, investing in Google Search, so we could target potential customers who search for more basic health questions like "how do I lose weight?" or "how many steps should I take?" These questions are indicative of a population that wants to improve their health, but maybe don't know where to start. Bellabeat is there to get them started.
* Knowing that users exercise a lot on Mondays and Tuesdays, but far less for the rest of the work week, we can narrow our digital marketing to Wednesdays to inspire potential customers to continue their strong start, using Bellabeat products as inspirational tools.

### Thanks for taking the time to read all of this! If you've made it this far, you might be:
* considering me for an open position (thank you again for your time and consideration)
* my mom, incredibly proud of her son's work (whether it be macaroni art or data analysis)
* super interested in FitBit data and/or data cleaning! (cool!)
