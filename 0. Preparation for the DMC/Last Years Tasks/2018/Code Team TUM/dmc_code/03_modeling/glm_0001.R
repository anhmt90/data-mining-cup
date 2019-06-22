
source('03_modeling/utils.R')

data <- split.dataset('small.time')

model <- train.model(data, function(data) {
  glm(units ~ date + price, data = data, family = poisson, epsilon = 0.1, maxit = 5, trace = T)
})

prediction <- predict.sales(data, model)

measure.prediction('glm_st_0001', model, prediction)
