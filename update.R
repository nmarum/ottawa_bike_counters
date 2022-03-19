#importing updated weather and counter data from 2019 to evaluate accuracy of previous model.

library(weathercan)#package providing access to Env Canada weather data
library(tidyverse)
library(readxl) #for reading xlsx table with updated bike counter data
library(lubridate) #for parsing of dates
library(caret) #includes several ML algorithms, including KNN3.
library(rpart) #provide a regression tree ML algorithm
library(Rborist)


#create object with updated Ottawa International Airport (station 49568) weather data for 2019
weather_2019 <- weather_dl(station_ids = 49568, start = "2019-01-01", end = "2019-12-31", interval = "day")


#import new bike counter data from city of ottawa - in xlsx and selecting 2019 worksheet
count_2019 <- read_xlsx("bike_counter.xlsx", sheet = 3)

#add day of year
day_of_year <- c(0:364)
count_2019 <- cbind(count_2019, day_of_year)

#modifying data from wide format to tidy format
count_2019 <- count_2019 %>% pivot_longer(cols = c(`1^ALEX`, `2^ORPY`, `3^COBY`, `4^CRTZ`, `5^LMET`, `10^OGLD`, `11 OBVW`, `12a^ADAWE`, `12b^ADAWE`), names_to = "location", values_to = "count")

#cleaning up location names
location_name <- str_remove(count_2019$location, pattern = "\\d\\d?\\^")
location_name <- str_replace_all(location_name, c("12a\\^" = "a", "12b\\^" = "b", "11 " = ""))

count_2019 <- cbind(count_2019, location_name) #overwrite location name with cleaned up ids.

count_2019 <- count_2019 %>% rename_with(tolower) #matching "date" column name case for join

count_2019 <- count_2019 %>% select(date, location_name, count, day_of_year)


#joining weather and count data
dat_2019 <- left_join(count_2019, weather_2019, by="date")

#selecting out key features and the count data
dat_2019 <- dat_2019 %>%
  select(date, location_name, count, day_of_year, min_temp, max_temp, mean_temp, snow_grnd, total_rain, total_snow)

summary(dat_2019) #checking...

dat_2019 <- dat_2019 %>% rename(MaxTemp = max_temp, MeanTemp = mean_temp, SnowonGrndcm = snow_grnd, MinTemp = min_temp, TotalRainmm = total_rain, TotalSnowcm = total_snow)

rm("count_2019", "weather_2019", "day_of_year")

##Loading original ott_bike_counters project data (2010 to 2018)
dat <- read_csv("ml_friendly_bike_counters.csv")
alt <- read_csv("ottawa_bike_counters.csv")

df <- as_tibble(unique(dat$day))#single col tibble of "days" from ML friendly csv
df2 <- as_tibble(unique(as.Date(alt$Date)))#single col tibble of "dates"
df <- cbind(df, df2)#binding two together 
colnames(df) <- c("day", "date")
head(df)

dat <- dat %>% left_join(df, by="day") #adding a actual date field to ML friendly data.

dat <- dat[1:28776,] #selecting entries which contain weather data (to end of 2018) .

rm("df", "df2")

coby <- dat %>% filter(location_name == "COBY", !is.na(SnowonGrndcm))

coby_2019 <- dat_2019 %>% filter(location_name == "COBY")

##COBY counter seems to be missing date from November/December 2019.
coby_2019 %>% ggplot(aes(date, count)) + geom_point()
coby_2019$date[which(is.na(coby_2019$count))] #from Nov 4 onward to Dec 31.
#Weather data exists until end of year however the last bike counter entry is row 307 of 365

fit_final <- rpart(count ~ MaxTemp + day_of_year + SnowonGrndcm, data = coby)

knn_final <- train(count ~ day_of_year + MaxTemp + MeanTemp + MinTemp + TotalRainmm + TotalSnowcm, method="knn",
                   tune.grid = data.frame(k=25), #best tune from original maxtemp model
                   data = coby, na.action = na.omit)

## testing on 2019 data

preds <- predict(fit_final, newdata = coby_2019, type = "vector")

RMSE(preds[1:307], coby_2019$count[1:307])  #RMSE for regression tree is ~450 (up to Nov 4, 2019)
#performance is similar to original model performance using data from 2010 to 2018.

plot(fit_final)
text(fit_final, use.n = TRUE)

preds2 <- predict(knn_final, newdata = coby_2019, type = "raw")

RMSE(preds2[1:307], coby_2019$count[1:307]) #RMSE of 410 for predictions up to Nov 4
#KNN performed similarly to original model.


###Visualizing the residuals

#Is there any patterns in the residuals that could give insight on how to improve the models?

#subtracting observed bike ride counts from predictions to generate residuals.  
#Positive residual means the model underestimated the number of rides:
rpart_residuals <- coby_2019$count[1:307]-preds[1:307]
hist(rpart_residuals)#normally distributed with cluster around zero
knn_residuals <- coby_2019$count[1:307]-preds2[1:307]
hist(knn_residuals) #KNN residuals are clustered largely between +-500

residuals <- tibble(rpart_residuals, knn_residuals) %>%
  pivot_longer(cols = c(rpart_residuals, knn_residuals), values_to = "residual",
               names_to = "model")
residuals %>% ggplot(aes(residual, col=model))+geom_density()+
  geom_vline(xintercept = 250, alpha=.5, linetype=2) + geom_vline(xintercept = 750, alpha=.5, linetype=2)
#Both models follow a similar Gaussian distribution - however with noticeable bump in the right tail.
#will try to identify which observations relate to the tail for residuals between 250 and 750.

ind <- between(knn_residuals, 250, 750)

bump <- coby_2019[ind,]
diff_prediction <- tibble(prediction = preds2[ind], residuals = knn_residuals[ind])
bump <- cbind(bump, diff_prediction)
summary(bump)
#the underestimated entries range from April to late October, include counts of 730 to 2616
bump %>% qplot(date, residuals, data = ., geom = c("smooth", "point"))


coby_2019_residuals <- tibble(coby_2019[1:307,], knn_residuals)
coby_2019_residuals %>% qplot(date, knn_residuals, data = ., geom = c("smooth", "point"))

coby_2019_residuals %>% qplot(MaxTemp, knn_residuals, data = ., geom = c("smooth", "point"))

coby_2019_residuals %>% mutate(day_of_week = as.factor(weekdays(date))) %>% qplot(date, knn_residuals, data = ., color = day_of_week, geom = "smooth")
#it looks like saturday and sunday volumes were overestimated from June to October.
#Workweek volumes, most notably Wednesday and Monday were underestimated June to August

coby_2019_residuals %>% mutate(day_of_week = as.factor(weekdays(date))) %>% qplot(date, rpart_residuals, data = ., color = day_of_week, geom = "smooth")
#similar issue with rpart - Weekdays were underestimated in the summer months 
#while weekends were underestimated from July to October

#adding day of the week as a feature for the model may help improve performance.

##Revised Model

coby_revised <- coby %>% mutate(day_of_week = weekdays(date))
  
coby_revised$day_of_week <- factor(coby_revised$day_of_week,
         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

coby_2019_revised <- coby_2019 %>% mutate(day_of_week = weekdays(coby_2019$date))

coby_2019_revised$day_of_week <- factor(coby_2019_revised$day_of_week,
         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


rpart_revised <- rpart(count ~ MaxTemp + day_of_year + SnowonGrndcm + day_of_week, data = coby_revised)

preds_revised <- predict(rpart_revised, newdata = coby_2019_revised, type = "vector")

RMSE(preds_revised[1:307], coby_2019_revised$count[1:307])   #no change...

plot(rpart_revised, compress = TRUE, margin = .1)
text(rpart_revised, use.n = TRUE)


knn_revised <- train(count ~ day_of_year + day_of_week + MaxTemp + MeanTemp + MinTemp + TotalRainmm + TotalSnowcm, method="knn",
                   tuneGrid = data.frame(k=25), #best tune from original maxtemp model
                   data = coby_revised, na.action = na.omit)



preds2_revised <- predict(knn_revised, newdata = coby_2019_revised, type = "raw")

RMSE(preds2_revised[1:307], coby_2019_revised$count[1:307]) #actually got slightly worse

##It clearly did not work.  According to varImp, the day of week model showed little overall importance
varImp(rpart_revised)

#I could try to adjust by adding a "month" variable to capture the bumps over the summer period, but
#I suspect it unlikely to do more than what the day_of_year variable is already capturing.

#The results may vary from different counters, but it still looks like weather and the time of the year are the 
#major determinants of bike rides along Colonel By Drive.  This could be different at one at other counters.


##Examing a Laurier Bike Lane counter at Metcalfe - downtown core of Ottawa

lmet <- dat %>% filter(location_name == "LMET")
lmet_2019 <- dat_2019 %>% filter(location_name =="LMET")

lmet <- lmet %>% mutate(day_of_week = weekdays(date))

lmet$day_of_week <- factor(lmet$day_of_week,
                                   levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

lmet_2019 <- lmet_2019 %>% mutate(day_of_week = weekdays(lmet_2019$date))

lmet_2019$day_of_week <- factor(lmet_2019$day_of_week,
                                        levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

lmet_2019 <- lmet_2019 %>% filter(!is.na(count))

rpart_lmet <- rpart(count ~ MaxTemp + day_of_year + SnowonGrndcm + day_of_week, data = lmet)

preds_lmet <- predict(rpart_revised, newdata = lmet_2019, type = "vector")

RMSE(preds_lmet, lmet_2019$count)

plot(rpart_lmet, compress = TRUE, margin = .1)
text(rpart_lmet, use.n = TRUE)
varImp(rpart_lmet)


knn_lmet <- train(count ~ MaxTemp + day_of_year + day_of_week + TotalRainmm + TotalSnowcm, method="knn",
                     tuneGrid = data.frame(k= 25), #best tune from original maxtemp model
                     data = lmet, na.action = na.omit) #omitted "SnowonGrndcm" due to too many NAs.
knn_lmet$results

preds2_lmet <- predict(knn_lmet, newdata = lmet_2019, type = "raw")

RMSE(preds2_lmet, lmet_2019$count) #significantly better than rpart

#From the regression tree and the variable importance function, we find day of the week is a 
#more significant predictor.  However, it still trails behind day of the year, max temp and 
#the presence of snow on the ground in terms of predicting

lmet %>% qplot(day_of_year, count, data = ., color = day_of_week, geom = "smooth")

coby_revised %>% qplot(day_of_year, count, data = ., color = day_of_week, geom = "smooth")



#also tried a random forest model
rf_lmet <- train(count ~ day_of_year + day_of_week + MaxTemp + TotalRainmm + TotalSnowcm, method="Rborist",
                 tuneGrid = data.frame(predFixed = 2, minNode = 3), #identified as best tune
                 data = lmet, na.action = na.omit)

rf_lmet$results #model results are impressive...

preds_rf <- predict(rf_lmet, newdata = lmet_2019)
RMSE(preds_rf, lmet_2019$count) #however the predictions on 2019 data is comparable to other models.
varImp(rf_lmet) 
