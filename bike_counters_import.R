#data import and wrangling

library(tidyverse)

#Files can be found in zip format on "https://www.kaggle.com/m7homson/ottawa-bike-counters/downloadhttps://www.kaggle.com/m7homson/ottawa-bike-counters/download"
#to use Kaggle you have to create an account and log in.  
#I have directly downloaded zip and extracted into the working directory.

dat <- read_csv("ml_friendly_bike_counters.csv")
alt <- read_csv("ottawa_bike_counters.csv")

df <- as_tibble(unique(dat$day))
df2 <- as_tibble(unique(as.Date(alt$Date)))
df <- cbind(df, df2)
colnames(df) <- c("day", "date")
view(df)

dat <- dat %>% left_join(df, by="day") #adding a actual date field rather than day of year
#I am hoping this will make data exploration easier.
view(dat)
dat <- dat[1:28776,] #selecting entries to end of 2018, which contain weather data.

save(dat, file = "dat.rda")


