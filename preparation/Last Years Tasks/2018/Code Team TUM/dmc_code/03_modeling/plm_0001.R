
source('03_modeling/utils.R')

data <- split.dataset('small.random')

data_train <- data$train
data_test <- data$test

#transform to panel data
data_train = pdata.frame(data_train, index=c("id","date")) 
data_test = pdata.frame(data_test, index=c("id","date"))

 


model <- train.model(data, function(data) {
  model = plm(units ~ paydayDE + price, data=data_train, model="pooling", family = poisson)
})

# see also Business Analytics Tut 3_4
# create fixed effects model 
#model_fe = plm(units ~ paydayDE + price, data=data_train, model="within")

# create random effects model
#model_re = plm(units ~ paydayDE + price, data = data_train, model = "random", random.method="walhus") 



prediction <- predict.sales(data, model)

measure.prediction('plm_st_0001', model, prediction)
