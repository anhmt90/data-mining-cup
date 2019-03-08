
#
# Creates a training and a testing subset based on the prepared data.
# 
# Based on the provided mode, different methods are used to create 
# training and testing subsets for modeling. For the mode, use a 
# combination of the size (`small`, `medium`, `large` or `all`) and 
# the split method (`random` or `time`), preferably separated by a dot
# e.g. `'medium.random'`.
# 
# @example
# ```
# data <- split.dataset('small.time')
# View(data$train)
# View(data$test)
# ```
#
# @param {string} mode The mode used for splitting the dataset.
# 
# @returns {list<data.table, data.table>} Returns a list with two entries, `train` and `test`.
#
split.dataset <- function(mode = 'small.time') {
  set.seed(17)
  
  # Load the prepared input data
  dataset <- readRDS('01_preparation/output/dataset.rds')[order(date, size, pid)]
  
  if(mode=='master'){
    print('master')
    train <- dataset[date<"2018-02-01"]
    test <- dataset[date>="2018-02-01"]
    
  }else{
    
    if (grepl('small', mode)) {
      dataset <- dataset[pid %in% sample(unique(dataset$pid), 10) & date < '2018-02-01']
    } else if (grepl('medium', mode)) {
      dataset <- dataset[pid %in% sample(unique(dataset$pid), length(unique(dataset$pid)) * 0.2) & date < '2018-02-01']
    } else if (grepl('large', mode)) {
      dataset <- dataset[pid %in% sample(unique(dataset$pid), length(unique(dataset$pid)) * 0.6) & date < '2018-02-01']
    }
    
    if (grepl('time', mode)) {
      weight <- .75
      dates <- seq(min(dataset$date), max(dataset$date), by = 'day')
      splitdate <- dates[length(dates)*weight]
      train <- dataset[date <= splitdate]
      test <- dataset[date > splitdate]
    } else {
      weight <- .75
      samples <- sample(c(T, F), nrow(dataset), replace = T, prob = c(weight, 1 - weight))
      train <- dataset[samples]
      test <- dataset[!samples]
    }
    
    
  }

  return(list('train' = train, 'test' = test))
}

#
# Applies the provided model on the training set.
# 
# Provide the previously created datasets and a function that
# that can execute a model. The function receives the traning 
# set on which the model should be applied.
# 
# @example
# ```
# model <- train.model(data, function(data) {
#   glm(units ~ date + price, data = data, family = poisson, epsilon = 0.1, maxit = 5, trace = T)
# })
# ```
#
# @param {list<data.table, data.table>} data The splitted dataset.
# @param {function} model A callback function receiving the training set.
# 
# @returns {Model} Returns the trained model.
#
train.model <- function(data, model) {
  set.seed(17)
  do.call(model, list(data$train))
}

#
# Predicts the sales using the provided model.
# 
# The model will be applied to the labeled test set to predict the sales for 
# each day. The returned dataset is a simple `data.table` with the minimum 
# required columns to estimate the performance of the model.
# 
# @example
# ```
# prediction <- predict.sales(data, model)
# ```
#
# @param {list<data.table, data.table>} data The splitted dataset.
# @param {Model} model The trained model.
# 
# @returns {data.table} Returns a new `data.table` with the labeled and predicted sales.

predict.sales <- function(data, model, keep = F) {
  set.seed(17)

  
  prediction <- predict(model, data$test, type = 'response')
  
  if (keep) {
    result <- copy(data$test)
  } else {
    result <- data$test[, .(pid, size, date, stock, units, units_test)]
  }
  result[, prediction := prediction]
  result[, predictedUnits := c(.SD[1, floor(cumsum(prediction))], diff(floor(cumsum(prediction)))), by = .(pid, size)]
  
  # Save the created data set for deployment
  saveRDS(result, file = '03_modeling/output/prediction.rds')
  
  
  return(result)
}

#
# Predicts the sales in a rolling window for each day using the provided model.
# 
# The model will be applied to the labeled test set to predict the sales for 
# each day. The returned dataset is a simple `data.table` with the minimum 
# required columns to estimate the performance of the model.
# 
# @example
# ```
# prediction <- predict.sales(data, model)
# ```
#
# @param {list<data.table, data.table>} data The splitted dataset.
# @param {Model} model The trained model.
# 
# @returns {data.table} Returns a new `data.table` with the labeled and predicted sales.
#

data = x_test_tbl
data2 = x_train_tbl
model = model_keras

rolling.predict.sales2 <- function(data, data2, model, keep = F) {
  set.seed(17)
  dataset <- split.dataset('master')$test
  dataset2 = split.dataset('master')$train
  dates<-unique(data)
  
  dataset$units<-NA
  dataset$unitsYesterday<-NA
  dataset$totalUnitsSoldPerDay<-NA
  dataset$totalUnitsSoldYesterday<-NA
  dataset$zeroCumulative<-NA
  
  dataset2$testdata<-FALSE
  dataset$testdata<-TRUE
  dataset2$units = split.dataset('master')$train$units
  #print(glimpse(dataset))
  #print(glimpse(dataset2))
    
  dat <- as.data.table(rbind(dataset, dataset2))
  #dat <- dat[pid==19843,] #to predict just PID 19843 for testing
  dat[, prediction:=0]
  
  bundle <- function(x) {
    print(dates[x])
    #prepare
    dat[, unitsYesterday := shift(units, 1L), by = .(id)]
    dat[, previousUnits_2 := shift(units, 2L), by = .(id)]
    dat[, previousUnits_3 := shift(units, 3L), by = .(id)]
    dat[, zeroCumulative := shift(zeroCumulative, 1L), by = .(id)]
    dat[, totalUnitsSoldPerDay := sum(units), by = date]
    dat[, totalUnitsSoldYesterday := shift(totalUnitsSoldPerDay, 1L), by = .(id)]
    #predict per date
    subset<-dat[date==dates[x]]
    #predictions <- predict(model, subset, type = 'response')
    predictions = model_keras %>% predict_classes(as.matrix(dat[date==dates[x]]), batch_size = 50)
    dat[date==dates[x], prediction:=predictions]
    dat[date==dates[x], units:=as.integer(prediction>0.5)]
    dat[, predictedUnits := c(.SD[1, floor(cumsum(prediction))], diff(floor(cumsum(prediction)))), by = .(pid, size)]
  }
  t(sapply(1:length(dates), bundle))
  return(dat[testdata==TRUE, .(pid, size, date, stock, units, units_test, prediction, predictedUnits)])
}



rolling.predict.sales <- function(data, model, keep = F) {
  set.seed(17)
  
  subset <- copy(split.dataset('master'))
  dates<-unique(subset$test$date)
  
  subset$test[, `:=`(
    units_original = units,
    units = NA,
    unitsYesterday = NA,
    totalUnitsSoldPerDay = NA,
    totalUnitsSoldYesterday = NA,
    zeroCumulative = NA,
    testdata = TRUE
  )]
  
  subset$train[, `:=`(
    units_original = units,
    testdata = FALSE
  )]
  
  dat <- rbind(subset$train, subset$test)
  #dat <- dat[pid==19843,] #to predict just PID 19843 for testing
  dat[, prediction:=0]
  dateback = dat$date
  
  
  bundle <- function(x) {
    print(dates[1])
    #prepare
    dat[, unitsYesterday := shift(units, 1L), by = .(id)]
    dat[, previousUnits_2 := shift(units, 2L), by = .(id)]
    dat[, previousUnits_3 := shift(units, 3L), by = .(id)]
    dat[, zeroCumulative := shift(zeroCumulative, 1L), by = .(id)]
    dat[, totalUnitsSoldPerDay := sum(units), by = date]
    dat[, totalUnitsSoldYesterday := shift(totalUnitsSoldPerDay, 1L), by = id]
    dat[is.na(unitsYesterday), unitsYesterday:= units]
    dat[is.na(previousUnits_2), previousUnits_2:= units]
    dat[is.na(previousUnits_3), previousUnits_3:= units]
    dat[is.na(zeroCumulative), zeroCumulative:= 0]
    dat[is.na(totalUnitsSoldYesterday), totalUnitsSoldYesterday:= 0]
    
    
    #normalization
    dat = dat[date==dates[1]]
    
    dat$size = as.factor(dat$size)
    dat$newSize = as.factor(dat$newSize)
    dat = data.matrix(dat)
    mean <- apply(dat, 2, mean)
    std <- apply(dat, 2, sd)
    dat <- scale(dat, center = mean, scale = std)
    dat = as.data.frame(dat)
  
    
    #dat$units = NULL
    #dat$id = NULL
    #dat$pid = NULL
    
    #predict per date
    predictions <- predict(model, dat, type = 'response')
    dat[date==dates[x], prediction:=predictions]
    dat[date==dates[x], units:=predictions]
  }
  
  t(sapply(1:length(dates), bundle))
  
  #t(sapply(1:length(dates), bundle))
  dat[, `:=`(
    units = units_original,
    units_units_original = NA
  )]
  dat[, predictedUnits := c(.SD[1, floor(cumsum(prediction))], diff(floor(cumsum(prediction)))), by = .(pid, size)]
  
  return(dat[testdata==TRUE, .(pid, size, date, stock, units, prediction, predictedUnits)])
}

#
# Creates benchmarks for the model and the prediction.
# 
# Based on the model and prediction performance, several KPIs are calculated to compare
# the models' performance. The results are written in a log file.
# 
# @example
# ```
# measure.prediction(prediction)
# ```
#
# @param {string} name A unique name of the model. You could use the following naming
#   convention: `<model-name>_<dataset-size><dataset-order>_<autoincrement for different features.>`.
#   For a glm model applied to the 'small.random' dataset, this would be: `glm_sr_0001`.
# @param {Model} model The model used for prediction.
# @param {data.table} prediction The result of predicting sales with a particular model.
#
measure.prediction <- function(name, model, prediction, append = F) {
  path <- '03_modeling/output/'
  print(paste0('Writing results to file ', path, name, '.log'))
  dir.create(path, showWarnings = FALSE)
  sink(paste0(path, name, '.log'), append = append)
  
  rmse <- function(x1, x2) sqrt(mean((x1-x2)^2, rm.na = T))
  
  # error term according to task description: sqrt(sum(betrag(dPredicted-d)))
  et <- function(x1, x2) 1/(sqrt(length(x1))) * sqrt(sum(abs(x1-x2))) 
  
  cat('Model results for model ', name, '\n')
  cat('=============================', '\n\n')
  
  # Model summary
  cat('Model Summary', '\n')
  cat('-------------', '\n')
  print(summary(model))
  
  # Prediction summary
  cat('Prediction Summary', '\n')
  cat('------------------', '\n')
  u = union(prediction$predictedUnits, prediction$units)
  t = table(factor(prediction$predictedUnits, u), factor(prediction$units, u))
  conf_matrix <- confusionMatrix(t)
  print(conf_matrix)
  cat('\n')
  
  # KPIs
  cat('KPIs', '\n')
  cat('----', '\n')
  cat('RSME (rounded): ', rmse(prediction$predictedUnits, prediction$units), '\n')
  cat('RSME (float): ', rmse(prediction$prediction, prediction$units), '\n')
  
  
  sink()
}
