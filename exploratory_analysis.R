#exploratory analysis script
library(tidyverse)
library(lubridate)
load("dat.rda")

dat %>%
  group_by(location_name) %>%
  summarize(mean_count = mean(count),
            prop_zerocount = mean(count==0),
            entries = n()) %>%
  arrange(desc(entries))  
#Shows the bike counter along Colonel By Drive near the Corktown footbridge (COBY) 
#as the counter with the most overall entries/fewest missing entries.
#COBY also has a relatively low proportion of "0" count/no bike entries (1.7%).
#While not the busiest counter, the consistent entries seem promising from a 
#predictive perspective.

#A city of Ottawa legend for the bike counter data provides the following description
#for the COBY counter:  "3_COBY: National Capital Commission (NCC) Eastern Canal 
#Pathway approximately 100m north of the Corktown Bridge. #WINTER counter"

#"The data provides counts of bike trips (both directions summed unless otherwise 
#noted)" COBY has not footnote associated with it so the data must be both directions.

coby <- dat %>% filter(location_name == "COBY")

hist(coby$count)
hist(dat$count)
#distribution of coby is similar though not as smooth as the overall dist of trips

dat %>% filter(location_name == c("COBY", "LMET", "LLYN", "LBAY", "SOMO", "ADAWE BIKE")) %>%
  ggplot(aes(count, col=location_name)) + geom_density() 
#selecting for winter counters

coby %>% ggplot(aes(count, col=location_name)) + geom_density()
#density plot coby counts follows similar patter as most other winter counters

coby_rides <- c(median(coby$count), mean(coby$count))
total_rides <- c(median(dat$count), mean(dat$count))
diff <- c(median(coby$count)-median(dat$count), mean(coby$count)-mean(dat$count))
data.frame(coby_rides, total_rides, diff, row.names = c("median", "mean"))

#median and mean of coby counts are similar to overall counts

#Predictive model created by coby data should have applicability to other 
#winter counters

save(coby, file = "coby.rda") # saving COBY entries only.

coby %>% group_by(day_of_week) %>%
  summarize(avg_count = mean(count)) %>%
  ggplot(aes(day_of_week, avg_count)) + geom_col() + 
  ggtitle("Col By Counter - Average Count of Rides by Day of Week")
#consistently busy through the week - a little busier on weekdays


coby %>% mutate(year = year(date)) %>%
  ggplot(aes(day_of_year, count, col=year)) +
  geom_point(alpha=.5) +
  ggtitle("Col By Counter - Rides by Day of the Year (2010-2018)")
#clear trend over course of the year.  First 100 days (3 or so months) of year are
#dead of winter for Ottawa - not much riding.  Peaks in summer months and then drops
#back off in the fall.  Appears relatively consistent year over year.

coby %>% mutate(year = year(date), month = month(date)) %>%
  ggplot(aes(day_of_year, count, col=MaxTemp)) +
  geom_point(alpha=.5) +
  ggtitle("Col By Counter - Rides by Day of the Year and Max Temp")
#Higher temps align with summer months - and more rides

coby %>% mutate(year = year(date), month = month(date)) %>%
  ggplot(aes(day_of_year, count, col=TotalPrecipmm)) +
  geom_point(alpha=.5) +
  ggtitle("Col By - Rides by Day of the Year and Total Precipmm")
#Notice that there are a few days of high precipitation appear to be less busy.


#Seems like there are fewer rides when there is rain - 
#I decide to stratify data by precip to have  better look using facet view  

#creating a factor with levels
precip_levels <- coby %>% 
  mutate(Precipmm_levels = factor(round(TotalPrecipmm/10, digits=0)))

#labelling factor levels
levels(precip_levels$Precipmm_levels) <- c("<=5", ">5-14.9", "15-24.9", "25-34.9", "35-44.9", "45-54.9", "65-74.9", "75-84.9", "NaN")

#Visualizing by stratified precipitation levels
precip_levels %>% mutate(year = year(date), month = month(date)) %>%
  filter(TotalPrecipmm<55) %>%
  ggplot(aes(day_of_year, count)) +
  geom_point(alpha=.5) +
  geom_smooth(method = "loess") +
  facet_wrap(facets = Precipmm_levels~.) +
  ggtitle("Col By - Rides by Day of the Year Stratified by TotalPrecipmm")
#Stratifying by Totalprecipmm and overlaying a bin smoothing line using local weighted regression
#We see that only at very high precip levels there seems to be an effect on Rides.
#the apparent visual difference between the low/no precip days and high precip days
#may simply be due to random variation.
#Day of the year seems to be a stronger indicator - the summer months have more rides
#even in the rain than during the Ottawa winter months (Dec-March)

#Since precipitation does not appear significant predictor and we have seen temp,
#lets look at snow on the ground
coby %>% mutate(year = year(date), month = month(date)) %>%
  filter(SnowonGrndcm > 0) %>%
  ggplot(aes(day_of_year, count, col=SnowonGrndcm)) +
  geom_point(alpha=.5) +
  ggtitle("Col By Counter - Rides with Snow on Ground > 0cm")

#clear trend that any recorded snow on the ground reduces the number of rides 
#significantly.


#running a series of pairwise correlations by various predictors
day_of_year <- cor(coby$count, coby$day_of_year, use = "pairwise.complete.obs", method = "spearman")
day_of_week <- cor(coby$count, coby$day_of_week, use="pairwise.complete.obs", method = "spearman")
Max_Temp <- cor(coby$MaxTemp, coby$count, use="pairwise.complete.obs", method = "spearman")
Mean_Temp <- cor(coby$MeanTemp, coby$count, use = "pairwise.complete.obs", method = "spearman")
Min_Temp <- cor(coby$MinTemp, coby$count, use="pairwise.complete.obs", method = "spearman")
Total_Precipmm <- cor(coby$TotalPrecipmm, coby$count, use="pairwise.complete.obs", method = "spearman")
Total_Rainmm <- cor(coby$TotalRainmm, coby$count, use = "pairwise.complete.obs", method = "spearman")
Total_Snowcm <- cor(coby$TotalSnowcm, coby$count, use = "pairwise.complete.obs", method = "spearman")
Snow_on_Grd <- cor(coby$SnowonGrndcm, coby$count, use = "pairwise.complete.obs", method = "spearman")

#create a data.frame with all the Spearman correlations
data.frame(day_of_year, day_of_week, Max_Temp, Mean_Temp, Min_Temp, Total_Precipmm, Total_Rainmm, Total_Snowcm, Snow_on_Grd, row.names = "Correlation with Rides")
#The Spearman correlation was used in order to help control for outliers by computing correlation based 
#on the ranks of values, of which there seem to be many in the data.

#while visualizing the data shows that day of the year is a strong predictor
#looking at correlations of the various features against rides 
#suggests that using temperature, max temp in particular,
#and snow on the ground are most closely associated with rides.

#Higher temps are strongly correlated with more rides, while increased snow on the ground 
#is negatively correlated with rides.  Precipitation, rain or snow, seems less of a predictor.

#While causation is not clear (is it higher temperature or the fact it is the summer months that drive h
#additional ridership on the Col By pathway?) the patterns is clear.

#I will look at creating a ML model that looks at Max Temp, Snow on Ground and Day of year features
#in order to predict number of rides.



