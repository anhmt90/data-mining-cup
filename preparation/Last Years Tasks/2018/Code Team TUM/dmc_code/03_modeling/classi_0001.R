
source('03_modeling/utils.R')

data <- split.dataset('large.time')

#prep
data$train$id <- NULL
data$test$id <- NULL
data$train$pid <- NULL
#data$test$pid <- NULL
data$train$units<- NULL
#data$test$units <- NULL


# Calculate weights for the attributes using Info Gain and Gain Ratio
# weights_info_gain = information.gain(class ~ ., data=data$train)
# cutoff.k(weights_info_gain, 20)
# weights_gain_ratio = gain.ratio(class ~ ., data=data$train)
# cutoff.k(weights_gain_ratio, 20)
# most_important_attributes <- cutoff.k(weights_info_gain, 20)

# Alternative attribute importance through lvq model
# prepare training scheme
control <- trainControl(method="repeatedcv", number=5, repeats=2)
# train the model
# model <- train(class~., data=data$train , method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
# importance <- varImp(model, scale=FALSE)
# print(importance)
# plot(importance)

most_important_attributes <- c("unitsTotalSoldPerItem","zeroCumulative","totalUnitsSoldYesterday","discountToRPP","priceChangeLast7Days","unitsYesterday","numberOfCompetingBrands","priceDropYesterday","previousUnits_2")
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "class")
model_dt = train(formula_with_most_important_attributes, data=data$test, method="J48", trControl=control, metric="Accuracy",  na.action=na.omit)
#model_rf <- randomForest(formula_with_most_important_attributes, data = data$train)

confusionMatrix(model_dt)
prediction_classes = predict.train(object=model_dt, newdata=data$test, na.action=na.pass)
confusionMatrix(prediction_classes, data$test$class)




# prediction <- predict.sales(data, model_dt)
set.seed(17)
prediction <- predict.train(model_dt, data$test)
result <- data$test#[, .(pid, size, date, stock, units, class)]
result[, prediction := prediction]
#result[, predictedUnits := c(.SD[1, floor(cumsum(prediction))], diff(floor(cumsum(prediction)))), by = .(pid, size)]
saveRDS(result, file = '03_modeling/output/prediction.rds')

#measure.prediction('classifiaction_0001', model_dt, prediction)
sqrt(mean((as.integer(result$prediction)-result$units)^2, rm.na = T)) #rmse


#Evaluating Model and Features
#varImp(model_dt)
#vif(model_dt)
#summary(model_dt)
#u = union(result$prediction, result$class)
#t = table(factor(prediction$predictedUnits, u), factor(prediction$units, u))
#conf_matrix <- confusionMatrix(t)
#conf_matrix


