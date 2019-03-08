
################################
##    Features Selection
################################
data <- split.dataset('large.time')

data$train$id <- NULL
data$test$id <- NULL
data$train$pid <- NULL
data$test$pid <- NULL
data$train$units<- NULL
data$test$units <- NULL


#correlation
fcorrelationMatrix <- function(vars, dat) sapply(vars, function(y) sapply(vars, function(x) assocstats(table(dat[,x], dat[,y]))$cramer))
factorF <- names(Filter(is.factor, data$train))
fcorrelationMatrix(factorF, as.data.frame(data$train))

numberF <- as.list(names(Filter(is.numeric, data$train))) 
correlationMatrix <- cor(data$train[,colnames(data$train) %in% numberF, with=FALSE])


# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(units_test ~ ., data=data$train)
cutoff.k(weights_info_gain, 20)
weights_gain_ratio = gain.ratio(units_test ~ ., data=data$train)
cutoff.k(weights_gain_ratio, 20)

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_info_gain, 20)
most_important_attributes


#manuel selection
allNUM <- c("rrp","stock", "price", "unixEpoch", "weekNr", "timeSinceRelease", "zeroCumulative", 
                                 "unitsYesterday","previousUnits_2", "previousUnits_3", "movingAverageUnits", "totalUnitsSoldPerDay", 
                                 "priceYesterday", "movingAveragePrice","priceChangeLast7Days","priceDropYesterday", 
                                 "percentageOfRPP","trend11teamsports", "numberOfCompetingCategoryItems", "numberOfCompetingBrands", 
                                 "relPriceCompetingStyle", "unitsTotalSoldPerItem")

most_important_attributes <- c("timeSinceRelease",
                               "unitsYesterday","previousUnits_2", "previousUnits_3", 
                               "movingAveragePrice", 
                               "percentageOfRPP","trend11teamsports", "numberOfCompetingCategoryItems", "numberOfCompetingBrands", 
                               "unitsTotalSoldPerItem")

most_important_attributes <- c("discountToRPP", 
                               "trend11teamsports", 
                               "unitsYesterday")

most_important_attributes <- c("unitsTotalSoldPerItem",
                               "zeroCumulative","totalUnitsSoldYesterday", "discountToRPP", 
                               "priceChangeLast7Days", 
                               "unitsYesterday","numberOfCompetingBrands")
# teo: a bit better results
# adding weekday makes the results worse
most_important_attributes <- c("unitsTotalSoldPerItem",
                               "zeroCumulative","totalUnitsSoldYesterday", "discountToRPP", 
                               "priceChangeLast7Days", 
                               "unitsYesterday","numberOfCompetingBrands","priceDropYesterday","previousUnits_2")

#Linear - bin
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "units")
model <- glm(formula_with_most_important_attributes , data = data$train, family = poisson, epsilon = 0.1, trace = T)

prediction.bin <- predict.sales(data, model)
#set Cutoff: 0.5
prediction.bin[,predictedUnits:=ifelse((prediction>0.5),1,0)]


#Evaluating Model and Features

varImp(model)
vif(model)
summary(model)

  u = union(prediction.bin$predictedUnits, prediction.bin$units_test)
  t = table(factor(prediction.bin$predictedUnits, u), factor(prediction.bin$units, u))
conf_matrix7 <- confusionMatrix(t)
conf_matrix7

