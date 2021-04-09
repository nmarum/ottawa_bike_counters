#Model building

library(tidyverse)
library(lubridate)
library(caret) #includes several ML algorithms, including KNN3.
library(rpart) #provide a regression tree model
library(arm) #for bayesian model
library(randomForest) #for random forest model
load("dat.rda")

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


rf_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "rf", 
                data = train, na.action = na.omit, #omit NAs
                tuneGrid = data.frame(mtry = seq(1:3)),
                trControl = control)
ggplot(rf_cv, highlight = TRUE)
rf_cv$bestTune #mtry of 1  #best is about RMSE of 415

bayes_glm_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "bayesglm", 
               data = train, na.action = na.omit, #omit NAs
               trControl = control)
bayes_glm_cv$results #no tuning parameters = RMSE 470

lm_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "lm", 
                           data = train, na.action = na.omit, #omit NAs
                           trControl = control)
lm_cv$results #no tuning - simple linear regression - RMSE 470

rpart2_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "rpart2", 
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
knn_all_cv$results
knn_all_cv$bestTune #K of 33 and RMSE of about 392 - not much of an improvement with precip 
#predictors

#lets do the same with rtree - all predictors

fit_all <- rpart(count ~ day_of_year + MaxTemp + SnowonGrndcm + MeanTemp +MinTemp + TotalRainmm + TotalSnowcm + TotalPrecipmm, data = coby)

plot(fit_all, compress = TRUE, margin = .1)
text(fit_all)
#similar results to the top 3 predictors, but uses mean temp rather than max temp.

variables <- varImp(fit_all)
variables %>% arrange(desc(Overall))

#mean and max temp are top predictors followed by snow on ground.  day_of_year is sinficant predictor as well
#but less meaningful than the temp predictors.

cor(train$MeanTemp, train$MaxTemp, use = "pairwise.complete.obs")
cor(train$MeanTemp, train$MinTemp, use = "pairwise.complete.obs")
#naturally the temps are highly correlated.  Since regression tree suggested 
#MeanTemp is the best predictor we will swap that in for  the regression tree
#and examine it for the KNN approach.

cor(train$day_of_year, train$MeanTemp, use = "pairwise.complete.obs")
#relatively low correlation with day or year .369.  While weaker in terms of predictive
#power, the day of year can make a algorithm more robust since it is not highly correlated
#with temperature overall.  An advantage to the distance based KNN method.

knn_meantemp <-  train(count ~ day_of_year + MeanTemp + SnowonGrndcm, method = "knn", 
                      data = train, na.action = na.omit, #omit NAs
                      tuneGrid = data.frame(k = seq(1, 50, 2)),
                      trControl = control)
ggplot(knn_meantemp, highlight = TRUE)
knn_meantemp$bestTune 
#seems close but Maxtemp actually performs slightly better here on KNN.

#we will use KNN and the Rtree on the test set to get a sense of the algorithms true 
#accuracy.


#final models

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

#Rtree does not have the same predictive power as KNN but its performance is better
#than a lot of other algorithms and has very attractive interpretability.  From a 
#City of Ottawa operations and planning perspective, being able to provide staff
#a simple heuristic to estimate how many riders one can expect on a given day using the
#regression tree model would be very attractive and its performance is not that far 
#off (+/-10%) from more sophisticated algorithms.

#This model is based on one pathway bike counter.  However we can see how well it applies on
#another winter bike counter in the Ottawa area for fun!

#The Ottawa river bike counter had the second most number of entries.

ORPY <- dat %>% filter(location_name == "ORPY", !is.na(SnowonGrndcm), !is.na(TotalSnowcm))
statistic <- c("min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
tibble(statistic, summary(ORPY$count), summary(coby$count))
#we can see that on average the Ottawa River pathway has more riders, with higher median,
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

#this suggests that while the overall approach for forecasting rides based on the weather
#could still be used, each counter has slight different factors that impact the number of rides.

#ways to further improve the performance would be to find some way to model the effects
#of holidays or other significant annual festivals and events would have on traffic.

#also, a more complex model using previous year data as a baseline but would adjust 
#for new urban developments that feed traffic to pathways, adjustments to public transit, etc., 
#would likely provide improved accuracy - but is well beyond my capacity to do at this time.

