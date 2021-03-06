#Ottawa Bike Counter Project - final script

#INTRO:  Script below represents data import, wrangling, exploratory analysis and 
#machine learning model building for a machine learning project for the HarvardX
#capstone project.  It is uses publicly available City of Ottawa bike ride counter
#data from public pathways in Ottawa, Ontario CANADA as well as publicly
#available weather data from Environment Canada.

##REQUIRED LIBRARIES

if(!require(lubridate)) install.packages("lubridate")
if(!require(caret)) install.packages("caret")
if(!require(rpart)) install.packages("rpart")
if(!require(arm)) install.packages("arm")
if(!require(randomForest)) install.packages("randomForest")

library(tidyverse)
library(lubridate) #for parsing of dates
library(caret) #includes several ML algorithms, including KNN3.
library(rpart) #provide a regression tree ML algorithm
library(arm) #for naive bayesian ML algorithm
library(randomForest) #for random forest ML algorithm

##DATA WRANGLING
#Data files were  found in zip format on Kaggle at the following address:
#https://www.kaggle.com/m7homson/ottawa-bike-counters/download
#To access it on Kaggle you have to create an account and login which 
#is a challenge to do in the R environment.  

#for convenience I have uploaded the two key datasets to my Github repo:

url1 <- "https://raw.githubusercontent.com/nmarum/ottawa_bike_counters/master/ml_friendly_bike_counters.csv"
url2 <- "https://raw.githubusercontent.com/nmarum/ottawa_bike_counters/master/ottawa_bike_counters.csv"

download.file(url1, destfile = "ml_friendly_bike_counters.csv") #ML friendly csv with weather info
download.file(url2, destfile = "ottawa_bike_counters.csv")#bike ride count data only BUT with date

dat <- read_csv("ml_friendly_bike_counters.csv")
alt <- read_csv("ottawa_bike_counters.csv")

df <- as_tibble(unique(dat$day))#single col tibble of "days" from ML friendly csv
df2 <- as_tibble(unique(as.Date(alt$Date)))#single col tibble of "dates"
df <- cbind(df, df2)#binding two together 
colnames(df) <- c("day", "date")
head(df)

dat <- dat %>% left_join(df, by="day") #adding a actual date field to ML friendly data.
#I am hoping this will make data exploration easier.

head(dat)
dat <- dat[1:28776,] #selecting entries which contain weather data (to end of 2018) .

rm("df", "df2")

##EXPLORATORY ANALYSIS

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
#selecting for "winter" counters (i.e., counters that operate year round even when there is snow on the ground.)

coby %>% ggplot(aes(count, col=location_name)) + geom_density()
#density plot coby counts follows similar patter as most other winter counters

coby_rides <- c(median(coby$count), mean(coby$count))
total_rides <- c(median(dat$count), mean(dat$count))
diff <- c(median(coby$count)-median(dat$count), mean(coby$count)-mean(dat$count))
data.frame(coby_rides, total_rides, diff, row.names = c("median", "mean"))
#median and mean of coby counts are similar to overall counts

tibble(sd(coby$count), sd(dat$count))
#sd is similar as well.  814 rides.  Quite a large range in terms of overall count.

#A predictive model created by coby data should have applicability to other 
#winter counters

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
#I decided to stratify data by precip to have  better look using facet view  

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

#lets take a look at rides by temp stratified
temp_levels <- coby %>% 
  mutate(maxtemp_levels = factor(round(MaxTemp/10, digits=0)))

#labelling factor levels
levels(temp_levels$maxtemp_levels) <- c("-20", "-10", "0", "10", "20", "30", "40","NaN")

temp_levels %>% mutate(year = year(date), month = month(date)) %>%
  filter(maxtemp_levels %in% c("-20", "-10", "0", "10", "20", "30", "40")) %>%
  ggplot(aes(MaxTemp, count)) +
  geom_point(alpha=.5) +
  geom_smooth(method = "loess") +
  ggtitle("Col By Bike Counter - Count of rides by Max Temp")
#this shows a smooth relationship between an increase in temperature and more rides
#however the effect seems to tail off at higher temperatures - presumably too hot 
#for some riders.


#Visualizing ride counts over the course of the year stratified by temp
temp_levels %>% mutate(year = year(date), month = month(date)) %>%
  filter(maxtemp_levels %in% c("-20", "-10", "0", "10", "20", "30")) %>%
  ggplot(aes(day_of_year, count)) +
  geom_point(alpha=.5) +
  geom_smooth(method = "loess") +
  facet_wrap(facets = maxtemp_levels~.) +
  ggtitle("Col By - Rides by Day of the Year Stratified by Max Temp")
#we can see that warmer days are clustered in summer months and have more rides
#days in the same broad temperature range have wide variability

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
data.frame(day_of_year, day_of_week, Max_Temp, Mean_Temp, Min_Temp, Total_Precipmm, Total_Rainmm, Total_Snowcm, Snow_on_Grd, row.names = "Spear Cor with Rides")
#The Spearman correlation was used in order to help control for outliers by 
#computing correlation based on the ranks of values, of which there seem to be
#many in the data.

#while visualizing the data shows that day of the year is a strong predictor
#looking at correlations of the various features against rides 
#suggests that using temperature, max temp in particular,
#and snow on the ground are most closely associated with rides.

#Higher temps are strongly correlated with more rides, while increased snow on the ground 
#is negatively correlated with rides.  Precipitation, rain or snow, seems less of a predictor.

#While causation is not clear (is it higher temperature or the fact it is the summer months that drive
#additional ridership on the Col By pathway?) the pattern is clear.

#I will look at creating a ML model that looks at Max Temp, Snow on Ground and Day of year 
#as key features to predict number of rides.

##MACHINE LEARNING MODEL BUILDING

#select for COBY data and remove some na entries to avoid errors for some algorithms
coby <- dat %>% filter(location_name == "COBY", !is.na(SnowonGrndcm))


#create data partition for training and evaluating of model
set.seed(20201021, sample.kind = "Rounding")
ind <- createDataPartition(coby$count, p=.9, list = FALSE)

train <- coby[ind,]
test <- coby[-ind,]

#using K nearest neighbours with the three key predictors.  Intuitively, this seemed the best
#model due to the very different relationships that the key features seem to have on rides.
control <- trainControl(method = "cv", number = 10, p = .9)
knn_cv <- train(count ~ day_of_year + MaxTemp + SnowonGrndcm, method = "knn", 
                data = train, na.action = na.omit, #omit NAs
                tuneGrid = data.frame(k = seq(1, 50, 2)),
                trControl = control)
ggplot(knn_cv, highlight = TRUE)#graph of turning based on lowest RMSE for continuous data
knn_cv$bestTune #K of 25 and RMSE of about 400

rf_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "rf", #randomForest algorithm
               data = train, na.action = na.omit, #omit NAs
               tuneGrid = data.frame(mtry = seq(1:3)),
               trControl = control)
ggplot(rf_cv, highlight = TRUE)
rf_cv$bestTune #mtry of 1  #best is about RMSE of 415

bayes_glm_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "bayesglm", #bayesian algorithm
                      data = train, na.action = na.omit, #omit NAs
                      trControl = control)
bayes_glm_cv$results #no tuning parameters = RMSE 470

lm_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "lm", #linear regression
               data = train, na.action = na.omit, #omit NAs
               trControl = control)
lm_cv$results #no tuning - simple linear regression - RMSE 470

rpart2_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "rpart2", #regression tree
                   data = train, na.action = na.omit, #omit NAs
                   tuneGrid = data.frame(maxdepth = seq(1:10)),
                   trControl = control)

ggplot(rpart2_cv, highlight = TRUE)
rpart2_cv$results #best tune is Max depth of 4 and RMSE of 439

#visualizing a regression tree for this.
fit <- rpart(count ~ MaxTemp + day_of_year + SnowonGrndcm, data = coby)

plot(fit, compress = TRUE, margin = .1)
text(fit)
#Easily interpretable but day of year is not used.  Given clear pattern from day of 
#year we see in the data, this impacts the effectiveness of the regression tree.

#using k nearest neighbours using primary predictors provides for the best results.
#probably because it is better able to handle the non linear relationship from day of year
#it is also logical that regardless of temperature or weather, someone may be more 
#or less inclined to keep their bike out and go for a ride during fairer seasons
#as opposed to when their bike is put away for long periods of time (i.e., winter).

#lets try with all weather predictors
knn_all_cv <- train(count ~ day_of_year + MaxTemp + SnowonGrndcm + MeanTemp +MinTemp + TotalRainmm + TotalSnowcm + TotalPrecipmm, method = "knn", 
                    data = train, na.action = na.omit, #omit NAs
                    tuneGrid = data.frame(k = seq(1, 50, 2)),
                    trControl = control)
ggplot(knn_all_cv, highlight = TRUE)
knn_all_cv$bestTune #K of 33 and RMSE of about 392 - not much of an improvement with precip 
#predictors

#lets do the same with rtree - all predictors

fit_all <- rpart(count ~ day_of_year + MaxTemp + SnowonGrndcm + MeanTemp +MinTemp + TotalRainmm + TotalSnowcm + TotalPrecipmm, data = coby)

plot(fit_all, compress = TRUE, margin = .1)
text(fit_all)
#similar results to the top 3 predictors, but uses mean temp rather than max temp.

variables <- varImp(fit_all)
variables %>% arrange(desc(Overall))
#The rpart has the advantage of being able to provide the importance of variables 
#within the model using a simple function, varImp().  We can see that the
#mean and max temp features are top predictors followed by snow on ground.
#day_of_year is a predictor as well but less important than the temp predictors.

cor(train$MeanTemp, train$MaxTemp, use = "pairwise.complete.obs")
cor(train$MeanTemp, train$MinTemp, use = "pairwise.complete.obs")
#If we look at correlations, we naturally the temps are highly correlated.  
#Since regression tree suggested MeanTemp is the best predictor we will swap that
#in for  the regression tree and examine it for the KNN approach.

cor(train$day_of_year, train$MeanTemp, use = "pairwise.complete.obs")
#There is a relatively low correlation with day or year and mean temperature: .369.  
#While weaker in terms of predictive power, the day of year can make a algorithm 
#more robust as it is not highly correlated with temperature overall.  Particularly 
#this provide an advantage to the distance based KNN method.

knn_meantemp <-  train(count ~ day_of_year + MeanTemp + SnowonGrndcm, method = "knn", 
                       data = train, na.action = na.omit, #omit NAs
                       tuneGrid = data.frame(k = seq(1, 50, 2)),
                       trControl = control)
ggplot(knn_meantemp, highlight = TRUE)
knn_meantemp$bestTune 
#Will close, Maxtemp performs slightly better using KNN than mean temp.

#we will use KNN and the Rtree on the test set to get a sense of the algorithms true 
#accuracy.


#FINAL MODELS AND RESULTS

fit_final <- rpart(count ~ MaxTemp + day_of_year + SnowonGrndcm, data = coby)

rtree_res <- predict(fit_final, newdata = test, type = "vector")
RMSE(rtree_res, test$count)

knn_final <- train(count ~ day_of_year + MaxTemp + SnowonGrndcm, method="knn",
                   tune.grid = data.frame(k=25), #best tune from original maxtemp model
                   data = train, na.action = na.omit)
knn_res <- predict(knn_final, newdata=test, type = "raw")
RMSE(knn_res, test$count) #max temp performs better in Knn than mean temp

knn_final2 <- train(count ~ day_of_year + MaxTemp + SnowonGrndcm + MeanTemp +MinTemp + TotalRainmm + TotalSnowcm + TotalPrecipmm, method="knn",
                    tune.grid = data.frame(k=33), #best tune from all feature model
                    data = train, na.action = na.omit)
knn_res2 <- predict(knn_final2, newdata=test, type = "raw")
RMSE(knn_res2, test$count)  
#the all feature model does perform better than the 3 feature KNN model but by
#less than five rides.  

##COBY RESULTS
#The best performing model, KNN with all features, has an RMSE of 397.98.
#Rtree does not have the same predictive power as KNN but its performance is better
#than a lot of other algorithms (bayesglm, rf, and lm) and has very attractive 
#interpretability.  From a City of Ottawa operations and planning perspective, 
#being able to provide staff a simple heuristic to estimate how many riders one 
#can expect on a given day using the regression tree model would be very 
#attractive and its performance is not that far off (+/-10%) from more 
#sophisticated algorithms.

#This model is based on one pathway bike counter.  However we can see how well it applies on
#another winter bike counter in the Ottawa area for fun!

##APPLYING MODEL TO ANOTHER BIKE RIDE COUNTER

#The Ottawa River pathway bike counter had the second most number of entries.

ORPY <- dat %>% filter(location_name == "ORPY", !is.na(SnowonGrndcm), !is.na(TotalSnowcm))
statistic <- c("min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
tibble(statistic, summary(ORPY$count), summary(coby$count))
#We can see that on average the Ottawa River pathway has more riders, with higher median,
#mean and max number of rides than the Col By pathway.  However rides appear more
#skewed with a greater distance between the mean and median.

tibble(sd(ORPY$count), sd(coby$count))
#The Ottawa river pathway also has a higher standard deviation, suggesting greater variability.

set.seed(20201021, sample.kind = "Rounding")
ind <- createDataPartition(ORPY$count, p=.9, list = FALSE)#Partitioning ORPY ride data

train_o <- ORPY[ind,]
test_o <- ORPY[-ind,]

fit_final_o <- rpart(count ~ MaxTemp + day_of_year + SnowonGrndcm, data = train_o)#fitting ORPY regression tree

rtree_res_o <- predict(fit_final_o, newdata = test_o, type = "vector")
RMSE(rtree_res_o, test_o$count) #RMSE of 618 - about 200 higher RMSE than the COBY,

plot(fit_final_o, compress = TRUE, margin = .1)
text(fit_final_o) 
#snow on ground is no longer a determinant on the regression tree.  

knn_final_o <- train(count ~ day_of_year + MaxTemp + SnowonGrndcm + MeanTemp +MinTemp + TotalRainmm + TotalSnowcm + TotalPrecipmm, method="knn",
                     data = train_o, na.action = na.omit,
                     tuneGrid = data.frame(k = seq(1, 50, 2)), #best tune from all feature model
                     trControl = control)
knn_res_o <- predict(knn_final_o, newdata=test_o, type = "raw")
RMSE(knn_res_o, test_o$count) 

#Result is higher RMSE under KNN than COBY, however results are similar in that the RMSE is equal 
#to about about 1/2 a standard deviation.

##CONCLUSION

#The results of applying the model approach to another counter suggests that while
#the overall approach for forecasting rides based on the weather could still be 
#used across other counters, each counter has slight different factors that 
#impact the number of rides.  Therefore some tuning and selection of optimized parameters
#would be required for each.  It is not one size fits all.

#Potential ways to further improve the performance would be to find some way to model the effects
#of holidays or other significant annual festivals and events would have on traffic.

#Also, a more complex model using previous year data as a baseline but would adjust 
#for new urban developments that feed traffic to pathways, adjustments to public transit, etc., 
#would likely provide improved accuracy - but is well beyond my capacity to do at this time.