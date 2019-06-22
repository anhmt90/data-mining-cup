source('03_modeling/utils.R')

data <- split.dataset('large.time')

model <- train.model(data, function(data) {
  hurdle(units ~ totalUnitsSoldPerDay+ brand + weekday|unitsTotalSoldPerItem + zeroCumulative + totalUnitsSoldYesterday+discountToRPP+priceChangeLast7Days+unitsYesterday+numberOfCompetingBrands, data = data, dist = "geometric", zero.dist = "binomial",link = "logit", maxit = 10000, trace = T)
})


prediction <- predict.sales(data, model)
measure.prediction('hurdle_st_0001', model, prediction, append=T)

