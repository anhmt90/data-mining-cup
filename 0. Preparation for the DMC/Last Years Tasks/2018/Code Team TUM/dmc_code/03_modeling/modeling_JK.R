#
# This file creates a prediction model based on the stored data set in `01_preparation/output/dataset.rds`
#

# Load the prepared input data
dataset <- readRDS('01_preparation/output/dataset.rds')

# Extract the subset on which the training should take place
trainset <- dataset[date < '2018-02-01']

#create binary variable sale for manual classification step
trainset$sale <-0
trainset[trainset$units>0,]$sale <- 1
trainset$sale <- as.factor(trainset$sale)

# split into training and test data 
n <- nrow(trainset)

shuffled_df <- trainset[sample(n), ] #random split
train_indices <- 1:round(0.7 * n) #separate so we can change these easily later for specific test data
test_indices <- (round(0.7 * n) + 1):n

train <- shuffled_df[train_indices, ]
test <- shuffled_df[test_indices, ]

#take smaller subset for practice
#train <- train[1:200000, ]
#test <- test[1:200000, ]

#build model(s) 

#zero inflation with logistic regression and Poisson regression
#summary(model1 <- zeroinfl(units ~ percentageOfRPP + weekday + weekNr + brand |percentageOfRPP + weekday + weekNr + brand, data = train, dist = "Poisson"))

#zero inflation with logistic regression and NegBin regression
summary(model2 <- zeroinfl(units ~ percentageOfRPP + weekday |percentageOfRPP + weekday, data = train, dist = "negbin"))

####
#begin manual implementations
####
#random forests classifier feeding into linear regression
#for now, since data cleaning not yet complete, remove incomplete rows
#rf_train = na.omit(train)
#rf_train = rf_train[,c("sale", "category", 'mainCategory', "subCategory", "price", "weekday", "weekNr", "timeSinceRelease", "totalUnitsSoldYesterday", "NumberOfCompetingBrands")]

#classify as sale or not sale
#model3_class <- randomForest(sale ~ price + weekday + weekNr + totalUnitsSoldYesterday , data = rf_train)
#regression on positive values only
#rf_train_pos[,rf_train$sale>0]

#rf_test = na.omit(test)
#rf_test = rf_test[,c("sale", "category", 'mainCategory', "subCategory", "price", "weekday", "weekNr", "timeSinceRelease", "totalUnitsSoldYesterday", "NumberOfCompetingBrands")]

#predict_class <- predict(model3_class, rf_test)
#confusionMatrix(predict_class, rf_test$sale)
#summary(predict_class)
#sum(predict_class==1 & rf_test$sale ==1)/sum(rf_test$sale ==1)
#sum(predict_class==0 & rf_test$sale ==0)/sum(rf_test$sale ==0)

###
#train cost sensitive classifier with NaiveBayes
###
#csc <- CostSensitiveClassifier(sale ~ price + weekNr + totalUnitsSoldYesterday+category+weekday, data = train, control = Weka_control(`cost-matrix` = matrix(c(0, 5, 1, 0), ncol = 2), W = "weka.classifiers.bayes.NaiveBayes", M = TRUE))
#eval_csc <- evaluate_Weka_classifier(csc, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
#eval_csc
#predicted_sale <- as.factor(predict(csc, test))

#train regression 
#summary(m1 <- glm.nb(units ~ sale + percentageOfRPP + weekday + weekNr, data = train))

#predict on test set
#dummy_test <- test[,!c("sale")]
#dummy_test[,sale := predicted_sale]
#predicted <- predict(m1, dummy_test)

####
#end manual implementations
####

#predict on test set
predicted <- predict(model2, test)

#rough evaluation & try to find good rounding point
sqrt((1/nrow(test)) * sum( (test$units - predicted) ^ 2)) #RMSE
sd(test$units) #stdev

sum((test$units == 0))/nrow(test)
sum((test$units > 0) & (predicted > 0.1))/sum((test$units > 0)) #use this to check where to round, insert below...


rounded_pred <- predicted
for (i in 1:nrow(test)){
  if(predicted[i] < 0.15){
    rounded_pred[i] <- 0
  } 
  else{
    rounded_pred[i] <- ceiling(predicted[i])
  }
}

#rough evaluation of rounded case
sqrt((1/nrow(test)) * sum( (test$units - rounded_pred) ^ 2)) #RMSE
sum((test$units > 0) & (rounded_pred > 0))/sum((test$units > 0))

#visualisation of results
results <- data.frame(date = test$date, true = test$units, pred = predicted,round = rounded_pred)

plot_true <- ggplot(data=results, aes(x=date, y=true))
plot_true + geom_bar(stat = "identity")

plot_predicted <- ggplot(data=results, aes(x=date, y=pred))
plot_predicted + geom_bar(stat = "identity")

plot_rounded_pred <- ggplot(data=results, aes(x=date, y=results$round))
plot_rounded_pred + geom_bar(stat = "identity")


#some notes: 
#this model cannot handle if there are more variables than observations -> group classes
#need to incorporate seasonality or the time-series element -> make feature of previous day sales 
#unixepoch had zero contribution - not a good way to incorporate time 


#save final model for evaluation and deployment
saveRDS(model, file = '03_modeling/output/model.rds')
saveRDS(model, file = '03_modeling/output/predictions.rds')
