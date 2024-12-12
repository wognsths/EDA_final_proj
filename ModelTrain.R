library(dplyr)
library(caret)
library(xgboost)
library(randomForest)
library(nnet)

# source("~/EDA_final_proj/data_prep.R")

data <- CD %>%
  select(Crm.Cd.Group, `AREA NAME`, vict_age, vict_sex, vict_descent, period) %>%
  filter(!is.na(Crm.Cd.Group), 
         !is.na(`AREA NAME`), 
         !is.na(vict_age), 
         !is.na(vict_sex),
         !is.na(vict_descent),
         !is.na(period))

data$Crm.Cd.Group <- as.factor(data$Crm.Cd.Group)
data$`AREA NAME` <- as.factor(data$`AREA NAME`)
data$vict_age <- as.factor(data$vict_age)
data$vict_sex <- as.factor(data$vict_sex)
data$vict_descent <- as.factor(data$vict_descent)
data$period <- as.factor(data$period)

data <- data %>% rename(AREA_NAME = `AREA NAME`)

set.seed(123)
trainIndex <- createDataPartition(data$Crm.Cd.Group, p = 0.7, list = FALSE)
train <- data[trainIndex,]
test <- data[-trainIndex,]

multi_logit_model <- nnet::multinom(Crm.Cd.Group ~ AREA_NAME + vict_age + vict_sex + vict_descent + period, data = train, trace=FALSE)

pred_logit <- predict(multi_logit_model, newdata=test, type="class")
confusionMatrix(pred_logit, test$Crm.Cd.Group)

prob_logit <- predict(multi_logit_model, newdata=test, type="probs")
head(prob_logit)

dmy <- dummyVars("~ AREA_NAME + vict_age + vict_sex + vict_descent + period", data = train)
train_x <- data.frame(predict(dmy, newdata=train))
test_x <- data.frame(predict(dmy, newdata=test))

train_y <- as.numeric(train$Crm.Cd.Group) - 1
test_y <- as.numeric(test$Crm.Cd.Group) - 1

dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)

params <- list(
  objective = "multi:softprob",
  num_class = length(levels(data$Crm.Cd.Group)),
  eval_metric = "mlogloss"
)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, watchlist = list(train=dtrain), verbose = 0)

pred_xgb <- predict(xgb_model, newdata = dtest)
pred_xgb_matrix <- matrix(pred_xgb, ncol=length(levels(data$Crm.Cd.Group)), byrow=TRUE)

colnames(pred_xgb_matrix) <- levels(data$Crm.Cd.Group)
head(pred_xgb_matrix)

pred_xgb_class <- levels(data$Crm.Cd.Group)[max.col(pred_xgb_matrix)]
confusionMatrix(as.factor(pred_xgb_class), test$Crm.Cd.Group)


rf_model <- randomForest(Crm.Cd.Group ~ AREA_NAME + vict_age + vict_sex + vict_descent + period, data=train, ntree=100)
pred_rf <- predict(rf_model, newdata=test, type="prob")
head(pred_rf) # 각 범죄 그룹별 확률
pred_rf_class <- predict(rf_model, newdata=test, type="response")
confusionMatrix(pred_rf_class, test$Crm.Cd.Group)


## File save
# saveRDS(multi_logit_model, file = "~/EDA_final_proj/EDA_Shinyapps_Final/multi_logit_model.rds")
# saveRDS(xgb_model, file = "~/EDA_final_proj/EDA_Shinyapps_Final/xgb_model.rds")
# saveRDS(rf_model, file = "~/EDA_final_proj/EDA_Shinyapps_Final/rf_model.rds")
