#data import and wrangling

library(tidyverse)

#Files were  found in zip format on Kaggle at the following address:
#https://www.kaggle.com/m7homson/ottawa-bike-counters/download
#To access it on Kaggle you have to create an account and login which 
#is a challenge to do in the R environment.  

#for convenience I have uploaded the two key datasets to my Github repo:

url1 <- "https://raw.githubusercontent.com/nmarum/ottawa_bike_counters/master/ml_friendly_bike_counters.csv"
url2 <- "https://raw.githubusercontent.com/nmarum/ottawa_bike_counters/master/ottawa_bike_counters.csv"

download.file(url1, destfile = "ml_friendly_bike_counters.csv")
download.file(url2, destfile = "ottawa_bike_counters.csv")

dat <- read_csv("ml_friendly_bike_counters.csv")
alt <- read_csv("ottawa_bike_counters.csv", col_types = "Ddddddddddddddddd")

df <- as_tibble(unique(dat$day))
df2 <- as_tibble(unique(as.Date(alt$Date)))
df <- cbind(df, df2)
colnames(df) <- c("day", "date")
head(df)

dat <- dat %>% left_join(df, by="day") #adding a actual date field rather than just using day of year
#I am hoping this will make data exploration easier.
head(dat)
dat <- dat[1:28776,] #selecting entries to end of 2018, which contain weather data.

save(dat, file = "dat.rda") #save the data file for analysis.
rm("df", "df2")


