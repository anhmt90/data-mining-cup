
source('03_modeling/utils.R')

data <- split.dataset('small.time')

model <- train.model(data, function(data) {
  zeroinfl(units ~ date + price, data = data, dist = 'poisson', maxit = 1000, trace = T)
})

prediction <- predict.sales(data, model)

measure.prediction('zeroinfl_st_0001', model, prediction)
