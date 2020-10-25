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

fit_final <- rpart(count ~ MeanTemp + day_of_year + SnowonGrndcm, data = coby)

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
#than a lot of other algorithms and has very attractive interpretability.  You
#can easily imagine from a City of Ottawa perspective, being able to give staff
#a quick heuristic to indicate how many riders to expect on a given day using the
#regression tree model would be very attractive and its performance is not that far 
#off more sophisticated methods like KNN.
=======

bayes_glm_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "bayesglm", 
               data = train, na.action = na.omit, #omit NAs
               trControl = control)
ggplot(bayes_glm_cv, highlight = TRUE)
bayes_glm_cv$results #no tuning parameters = RMSE 472

lm_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "lm", 
                           data = train, na.action = na.omit, #omit NAs
                           trControl = control)
lm_cv$results #no tuning - simple linear regression - RMSE 471

rpart2_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "rpart2", 
               data = train, na.action = na.omit, #omit NAs
               tuneGrid = data.frame(maxdepth = seq(1:10)),
               trControl = control)

ggplot(rpart2_cv, highlight = TRUE)
rpart2_cv$results #best tune is Max depth of 4 and RMSE of 443

#visualizing a regression tree for this.
fit <- rpart(count ~ MaxTemp + day_of_year + SnowonGrndcm, data = coby)

plot(fit, compress = TRUE, margin = .1)
text(fit)
#Easily interpretable but day of year is not used.  Given clear pattern from day of 
#year we see in the data, this impacts the effectiveness of the regression tree.

#using k nearest neighbours using primary predictors provides for the best results.

#ways to further improve the performance would be to find some way to model the effects
#of holidays or other significant annual festivals and events would have on traffic.

