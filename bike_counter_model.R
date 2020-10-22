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

#using K nearest neighbours with the three key predictors
control <- trainControl(method = "cv", number = 10, p = .9)
knn_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "knn", 
                      data = train, na.action = na.omit, #omit NAs
                      tuneGrid = data.frame(k = seq(1, 30, 2)),
                      trControl = control)
ggplot(knn_cv, highlight = TRUE)
knn_cv$bestTune #K of 23 and RMSE of about 400


rf_cv <- train(count ~ MaxTemp + day_of_year + SnowonGrndcm, method = "rf", 
                data = train, na.action = na.omit, #omit NAs
                tuneGrid = data.frame(mtry = seq(1:5)),
                trControl = control)
ggplot(rf_cv, highlight = TRUE)
rf_cv$bestTune #mtry of 1



#using k nearest neighbours with all predictors
