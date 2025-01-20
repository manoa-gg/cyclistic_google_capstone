About
This is my final test for graduating from google data analytics course in coursera. On this analysis over fictional company called cyclistic I'm using R to do data cleaning, transformation, analysis, and graph plotting.
The tasked is to act as a junior data analyst trying to finding out the characteristics of both users type which are member and casual. 
Cyclistic have great profit from member user that is why they want to see how can they attract the casual user to become member using existing data.
I'm using latest data from last 3 months.
Started by cleaning up the data of its blank cells, inconsistent data, and makes no sense data like ride time less than 60 secs which is does not make any sense. In cleaning I'm using drop_na, distinct, filter, grepl, as.character, as.factor and is.na statement.
Then transforming the data to suit my analysis of objective like converting date to days of the week, extracting month, separate between date and time, converting time to 24 hours time, 
  giving labels to time spent on ride from short trip to longest trip and finally renaming the column. In transforming data I'm using lubridate for converting 24 hours time, separate, unite, mutate, joint, rename, select(-), ymd, hms, month and joint statement.
Then I analyze by find out how many users belong to the members and casuals, after that I tried to compare the data of users with each different type of variable to see difference of characteristics between the two types of user from ride time, 
  daily usage, which time they usually start, which time they usually end, from which station they usually start, and so on. For analysis I use aggregate, count, mutate, summarise, joint, n_distinct statement. 
Compiling the majority characteristics of member users and use the same characteristic through joint to exclude the subset of casual users that can become the primary target for cyclistic campaign.
By the end I create plot using ggplot2 over several metrics that are important including the subset of casual users compare to its total casual users using ggplot, geom bar (bar and pie chart), geom label, scaling, and theme.
For package I use ggplot2 for creating plots, lubridate to transform date and time variable, also dplyr for data manipulation, and tidyr for cleaning the data.
Raw data and final workspace is compiled in drive https://drive.google.com/drive/folders/1aTT8Kky81hUQcfRv3r_LzAks93m53S1O?usp=sharing
