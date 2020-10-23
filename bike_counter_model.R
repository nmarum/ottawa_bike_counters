#Model building

library(tidyverse)
library(lubridate)
library(caret)
load("dat.rda")

coby <- dat %>% filter(location_name == "COBY")

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
ggplot(knn_cv, highlight = TRUE)
knn_cv$bestTune #K of 23 and RMSE of about 400


rf_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "rf", 
                data = train, na.action = na.omit, #omit NAs
                tuneGrid = data.frame(mtry = seq(1:3)),
                trControl = control)
ggplot(rf_cv, highlight = TRUE)
rf_cv$bestTune #mtry of 1  #best is about RMSE of 415

bayes_glm_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "bayesglm", 
               data = train, na.action = na.omit, #omit NAs
               trControl = control)
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


