library(caret)
library(AER)
library(pscl)
library(MASS)
library(randomForest)
library(keras)
library(ramify)
library(tensorflow)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(yardstick)
library(forcats)

source('03_modeling/utils.R')

data <- split.dataset('master')
#data = data2$train

#prep



data$train$id <- NULL
data$test$id <- NULL
data$train$pid <- NULL
data$test$pid <- NULL
data$train$units<- NULL
data$test$units <- NULL
data$train$class <- NULL
data$test$class <- NULL
data$train$units_test<- NULL
data$test$units_test <- NULL


data$train$units_test = NULL
data$test$units_test = NULL
data$train$class = NULL
data$test$class = NULL
data$train$unitsYesterday = NULL
data$test$unitsYesterday = NULL
data$train$totalUnitsSoldYesterday = NULL
data$test$totalUnitsSoldYesterday = NULL
data$train$previousUnits_2 = NULL
data$test$previousUnits_2 = NULL
data$train$previousUnits_3 = NULL
data$test$previousUnits_3 = NULL
data$train$movingAverageUnits = NULL
data$test$movingAverageUnits = NULL
data$train$UnitsChangeLast7Days = NULL
data$test$UnitsChangeLast7Days = NULL
data$train$totalUnitsSoldPerDay = NULL
data$test$totalUnitsSoldPerDay = NULL
data$train$unitsTotalSoldPerItem = NULL
data$test$unitsTotalSoldPerItem = NULL

data$train$newSize = as.factor(data$train$newSize)
data$train$size = as.factor(data$train$size)
data$test$size = as.factor(data$test$size)
data$test$newSize = as.factor(data$test$newSize)
#data$train$isHoliday = as.factor(data$train$isHoliday)
#data$test$isHoliday = as.factor(data$test$isHoliday)
#data$train$isHolidaySeason = as.factor(data$train$isHolidaySeason)
#data$test$isHolidaySeason = as.factor(data$test$isHolidaySeason)
#data$train$totalUnitsSoldPerDay = as.factor(data$train$totalUnitsSoldPerDay)
#data$test$totalUnitsSoldPerDay = as.factor(data$test$totalUnitsSoldPerDay)

#dataall = rbind(data$train, data$test)

#dataall = data.matrix(dataall)
#mean <- apply(dataall, 2, mean)
#std <- apply(dataall, 2, sd)
#dataall <- scale(dataall, center = mean, scale = std)
#dataall = as.data.frame(dataall)

#data_tr = dataall[1:1473665, ]
#data_te = dataall[1473666:1832737, ]

#data_te = data.matrix(data$test)
# Keras Neural Net with multiclass prediction
data_tr = data.matrix(data$train)


#mean <- apply(data_te, 2, mean)
#std <- apply(data_te, 2, sd)
#data_te <- scale(data_te, center = mean, scale = std)
#data_te = as.data.frame(data_te)
#data_te$isHoliday = numeric(nrow(data_te))
#data_te$isHolidaySeason = numeric(nrow(data_te))
#data_te$totalUnitsSoldPerDay = numeric(nrow(data_te))

mean <- apply(data_tr, 2, mean)
std <- apply(data_tr, 2, sd)
data_tr <- scale(data_tr, center = mean, scale = std)
data_tr = as.data.frame(data_tr)
#data_$newSize = NULL
#data$size = NULL
#data$movingAverageUnits = NULL



data_tr$bins <- cut(split.dataset('master')$train$units, breaks=c(-1,0,1,2,3,4,5,6,7,8,9,500), labels=c("0","1","2","3","4","5","6","7","8","9","10"))


#sampling methods

#random partition
#data_index <- createDataPartition(data$bins, p = 0.75, list = FALSE)

#y_train_vec <- to_categorical(pull(data_te, bins))
#y_test_vec  <- to_categorical(pull(data[-data_index,], bins))

#data_tr <- data

#manual partition

y_train_vec = to_categorical(pull(data_tr[1:1000000,], bins))
y_test_vec = to_categorical(pull(data_tr[1000000:1473665,], bins))
data_tr1 <- data_tr[1:1000000,]
data_te1 <- data_tr[1000000:1473665,]



data_tr1 = upSample(data_tr1, data_tr1$bins)

#data_te <- data[-data_index, ]

y_train_vec <- to_categorical(pull(data_tr1, bins))
#y_test_vec  <- to_categorical(pull(data_te, bins))

y_train_vec = y_train_vec[,-1]
y_test_vec = y_test_vec[,-1]



x_train_tbl <- as.data.frame(data.matrix(data_tr1[,-37]))
x_test_tbl  <- as.data.frame(data.matrix(data_te1[,-37]))
x_train_tbl <- as.data.frame(data.matrix(x_train_tbl[,-37]))
#x_test_tbl = as.data.frame(data_te)
#x_test_tbl  <- as.data.frame(data.matrix(x_test_tbl[,-15]))
#x_train_tbl <- as.data.frame(data.matrix(x_train_tbl[,-15]))
#x_test_tbl  <- as.data.frame(data.matrix(x_test_tbl[,-15]))

glimpse(x_train_tbl)
glimpse(x_test_tbl)



#ctrl <- trainControl(method = "repeatedcv", repeats = 5)

#x_train_tbl = as.data.frame(x_train_tbl)
#x_train_tbl$Class = as.factor(x_train_tbl$Class)
#set.seed(5627)
#orig_fit <- train(Class ~ ., data = x_train_tbl,
#                  method = "J48",
#                  metric = "Accuracy",
#                  trControl = ctrl)







x_train_tbl = as.matrix(x_train_tbl)

model_keras <- keras_model_sequential()

model_keras %>%
  
  # First hidden layer
  layer_dense(
    units              = 500,
    kernel_initializer = "uniform",
    activation         = "relu",
    input_shape        = ncol(x_train_tbl)) %>%
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Second hidden layer
  layer_dense(
    units              = 500,
    kernel_initializer = "uniform",
    activation         = "relu") %>%
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Third hidden layer
  layer_dense(
    units              = 500,
    kernel_initializer = "uniform",
    activation         = "relu") %>%
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Output layer
  layer_dense(
    units              = 11,
    kernel_initializer = "uniform",
    activation         = "sigmoid") %>%
  
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

keras_model


history <- fit(
  object           = model_keras,
  x                = x_train_tbl,
  y                = y_train_vec,
  batch_size       = 50,
  epochs           = 1,
  validation_split = 0.30
  #class_weight = list("0"=1,"1"=7,"2"=7,"3"=7,"4"=7,"5"=7,"6"=7,"7"=7,"8"=7,"9"=7,"10"=7,"11"=7)
)

#save_model_hdf5(model_keras, "kerasmodel", overwrite = TRUE, include_optimizer = TRUE)

#load_model_hdf5("kerasmodel", custom_objects = NULL, compile = TRUE)

plot(history)

x_test_tbl = as.matrix(x_test_tbl)
#x_test_tbl = as.data.frame(x_test_tbl)
x_train_tbl = as.data.frame(x_train_tbl)
#source('03_modeling/utils.R')
#prediction = rolling.predict.sales(x_test_tbl, x_train_tbl, model_keras)



pred <- model_keras %>% predict_classes(x_test_tbl, batch_size = 50)
pred = as.data.frame(pred)
#prediction = pred
#result <- data$test#[, .(pid, size, date, stock, units, class)]
#result$pid = split.dataset('master')$test$pid
#result[, prediction := prediction]


table(pred)
actual = as.data.frame(pull(data_te1, bins))
#actual = as.data.frame(pull(data[-data_index,], bins))
table(actual)
#testtable = as.data.frame(c(actual, pred))
y_test_vec = as.matrix(y_test_vec)
score <- model_keras %>% evaluate(x_test_tbl, y_test_vec, batch_size = 50)

# Print the score
print(score)


#set.seed(17)
#prediction <- predict.train(model_dt, data$test)
#result <- data$test#[, .(pid, size, date, stock, units, class)]
#result[, prediction := prediction]
#result[, predictedUnits := c(.SD[1, floor(cumsum(prediction))], diff(floor(cumsum(prediction)))), by = .(pid, size)]
#saveRDS(result, file = '03_modeling/output/prediction.rds')

backup = y_test_vec
y_test_vec = backup
y_test_vec = as.data.frame(argmax(y_test_vec))-1
#measure.prediction('classifiaction_0001', model_dt, prediction)
sqrt(mean((as.integer(pred$pred)-y_test_vec)^2, rm.na = T)) #rmse

varx = as.data.frame(factor(pred$pred, levels=min(y_test_vec):max(y_test_vec)))
y_test_vec = as.data.frame(factor(y_test_vec$`argmax(y_test_vec)`))
#confusionMatrix(as.integer(y_test_vec$y_test_vec), as.integer(pred$pred))

unli = unlist(varx)
unli2 = unlist(y_test_vec)
varx = as.data.frame(unli)
y_test_vec = as.data.frame(unli2)

table(varx$unli, y_test_vec$unli2)


