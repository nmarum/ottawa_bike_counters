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
  ggtitle("Corktown Counter - Average Count of Rides by Day of Week")
#consistently busy through the week - a little busier on weekdays


coby %>% mutate(year = year(date)) %>%
  ggplot(aes(day_of_year, count, col=year)) +
  geom_point(alpha=.5) +
  ggtitle("Corktown Counter - Rides by Day of the Year (2010-2018)")
#clear trend over course of the year.  First 100 days (3 or so months) of year are
#dead of winter for Ottawa - not much riding.  Peaks in summer months and then drops
#back off in the fall.  Appears relatively consistent year over year.

coby %>% mutate(year = year(date), month = month(date)) %>%
  ggplot(aes(day_of_year, count, col=MaxTemp)) +
  geom_point(alpha=.5) +
  ggtitle("Corktown Counter - Rides by Day of the Year and Max Temp")
#Higher temps align with summer months - however no other real pattern - looks random

coby %>% mutate(year = year(date), month = month(date)) %>%
  ggplot(aes(day_of_year, count, col=TotalPrecipmm)) +
  geom_point(alpha=.5) +
  ggtitle("Corktown - Rides by Day of the Year and Total Precipmm")
#few days of high precipitation appear to be less busy.

coby %>% mutate(year = year(date), month = month(date)) %>%
  filter(TotalPrecipmm > 5) %>% #entries with 5mm of rain or more
  ggplot(aes(day_of_year, count, col=TotalPrecipmm)) +
  geom_point(alpha=.5) +
  ggtitle("Corktown - Rides by Day of the Year and Total Precipmm >5mm")
#clearly fewer rides when there is rain - maybe I need to stratify data by precip
#to have  better look using facets?  
#Would have to create a factor with discrete levels...





