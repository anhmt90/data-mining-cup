################################################################################
# Exploring the data
################################################################################

# @Teo

# How high is the stock for the items at the beginning of february?
boxplot(items$stock)
table(items$stock)
# 60% of items have one item in stock at the beginning of february
length(which(items$stock == 1)) / nrow(items)
# there is one outlier item which has 459 items in stock
items[items$stock == 459,]

# Which colors are there? Can they be ranked from most popular to least popular?
# There are 17 colors 
# From the items in stock there are 4 dominant colors: black, white, blue, red
datasetColors <- as.factor(items$color)
plot(datasetColors, las=2)

# Whick colors are most sold?
# black, white, blue, red are most sold
salesByColor <- data.frame(tapply(dataset$units, as.factor(dataset$color), sum))
colnames(salesByColor)[1] <-"SoldUnits"
sortedSalesByColor <- salesByColor[order(salesByColor$SoldUnits, decreasing = TRUE),]
data.frame(sortedSalesByColor)
# Pie chart
pie(sortedSalesByColor)

# What are the most sold brands?
# 1.Nike, 2.adidas, 3.puma
salesByBrand <- data.frame(tapply(dataset$units, as.factor(dataset$brand), sum))
colnames(salesByBrand)[1] <-"SoldUnits"
sortedSalesByBrand <- salesByBrand[order(salesByBrand$SoldUnits, decreasing = TRUE),]
data.frame(sortedSalesByBrand)

# On which days were the most items sold?
# The 24th of November (black friday) sold by far most items. 
# Other days woth mentioning are the last week of november, first week of december and 31th of January
calendarPlot(dataset, pollutant = "totalUnitsSoldPerDay")

# How many items were released before 1-10-2017?
# 85% of them were release before 1-10-2017
length(which(items$releaseDate == "2017-10-01")) / nrow(items)

# Which are rare items? Sold only once
# There are 2205 items (not taking into account different colours) that were sold only once
unitSoldPerItemAggregated <- aggregate(dataset$units, by=list(id=dataset$id), FUN=sum)
length(which(unitSoldPerItemAggregated == 1))

# is there a correlation between daily price and daily sale?
# -0,04
cor(dataset$units, dataset$price)
# is there a correlation between number of competing category items and number of competing brands?
cor(dataset$NumberOfCompetingCategoryItems, dataset$NumberOfCompetingBrands)
# is there a correlation between numerical values?
onlyNumericalData <- Filter(is.numeric, dataset)
res <- cor(onlyNumericalData)
round(res, 2)
library(xlsx)
write.xlsx(res, "02_interpretation/correlation_matrix.xlsx")

# @Marina
# Mean discount per day grows gradually reaching its peak by February 27th
# The dynamics is very subtle, the increase in October is not even shown on the graph for all months

dataset[,meanDayDiscount := lapply(.SD, mean), by = date, .SDcols = "discountToRrp"]
salesPerDay <- unique(dataset[, c('totalUnitsSoldPerDay', 'meanDayDiscount','date','weekNr'), by= date, with=FALSE])
ggplot(salesPerDay, aes(x=date)) + geom_line(aes(y=meanDayDiscount))
ggplot(salesPerDay, aes(x=date)) + geom_line(aes(y=totalUnitsSoldPerDay))

# Sales per week and sales within a week
salesPerDay[, sum(totalUnitsSoldPerDay), by = weekNr]
salesPerDay[weekNr==47]
salesPerDay[weekNr==50]
salesPerDay[weekNr==51]

# Issue: do small price changes affect the model? Do we need to transform the discount values?

# dates with highest discounts are 2017-10-31,2017-11-29,2017-12-16,2018-01-31,2018-02-27

salesPerDay %>% group_by(months(date)) %>% slice(which.max(meanDayDiscount))

# dates with highest sales are 2017-10-31,2017-11-24,2017-12-06,2018-01-31
salesPerDay %>% group_by(months(date)) %>% slice(which.max(totalUnitsSoldPerDay))

# Issue - is there an automated way to define the Christmas season?
# 2097.704 - mean sales per day before BF 
salesPerDay %>% filter(date<fastPOSIXct('2017-11-24', tz = 'GMT')) %>% summarise(mean(totalUnitsSoldPerDay))
# 3562.607 - mean sales per day between BF and December 22nd 
salesPerDay %>% filter(date>fastPOSIXct('2017-11-23', tz = 'GMT')&date<fastPOSIXct('2017-12-22', tz = 'GMT')) %>% summarise(mean(totalUnitsSoldPerDay))
# 2381.512 - mean sales per day between Dec-22 and Feb-01 
salesPerDay %>% filter(date>fastPOSIXct('2017-12-21', tz = 'GMT')&date<fastPOSIXct('2018-02-01', tz = 'GMT')) %>% summarise(mean(totalUnitsSoldPerDay))


################################
##    Features Selection
################################
# Load the prepared input data
dataset <- readRDS('01_preparation/output/dataset.rds')


# Extract the subset on which the training should take place
trainset <- dataset[date < '2018-02-01']

dummy_cols(trainset, select_columns=c("brand"),remove_first_dummy=TRUE)
dummy_cols(trainset, select_columns=c("categoryPath"),remove_first_dummy=TRUE)
str(trainset)


#PCA - Selection
prin_comp <- prcomp(Filter(is.numeric, trainset), scale. = T)
summary(prin_comp)
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

cumsum(prop_varex)

train.data <- data.frame(units = trainset$units, prin_comp$x)
#we are interested in first 30 PCAs
train.data <- train.data[,1:66]
head(train.data)

# Train a very basic model
model <- glm(units ~ ., data = train.data, family = poisson)

#transform test into PCA
test.data <- predict(prin_comp, newdata = dataset[date >= '2018-02-01'])
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:66]
#predict the test data
predict <- predict(model,test.data)
