#
# This file creates a prediction model based on the stored data set in `01_preparation/output/dataset.rds`
#
source('03_modeling/utils.R')


# Load the prepared input data
data <- split.dataset('master')

# Train a very basic model
model <- train.model(data, function(data) {
  hurdle(units ~ trend11teamsports+ brand + weekday|unitsTotalSoldPerItem + zeroCumulative + totalUnitsSoldYesterday+discountToRPP+priceChangeLast7Days+unitsYesterday+numberOfCompetingBrands, data = data, dist = "geometric", zero.dist = "binomial",link = "logit", maxit = 10000, trace = T)
})

# Save the final model for evaluation and deployment
dir.create('03_modeling/output', showWarnings = FALSE)
saveRDS(model, file = '03_modeling/output/model.rds')

# Predict sales
#prediction <- predict(model, predictset, type = 'response')
prediction <-rolling.predict.sales(data, model)
prediction[pid=="13454"]
prediction[is.na(units)]
prediction[predictedUnits>5]

# Save the final prediction for evaluation and deployment
dir.create('03_modeling/output', showWarnings = FALSE)
saveRDS(prediction, file = '03_modeling/output/prediction.rds')

