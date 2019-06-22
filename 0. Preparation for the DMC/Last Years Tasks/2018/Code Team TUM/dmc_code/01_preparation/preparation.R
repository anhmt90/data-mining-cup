################################################################################
# This file should work on the initial data set and create new features, modify 
# existing ones and in the end provide the datasets ready for modeling.
################################################################################

# Load the initial input data into data.table objects
items  <- fread('input/items.csv')
prices <- fread('input/prices.csv')
train  <- fread('input/train.csv')

# Create indicies on each data.table to improve performance
id <- c('pid', 'size')
setkeyv(items, id)
setkeyv(prices, id)
setkeyv(train, c('date', id))

# The goal is to have a separate row for each item an date, with the appropriate price as one column
prices <- melt(prices, id.vars = id, variable.name = 'date', value.name = 'price')[!is.na(price)]

# Merge data sets
dataset <- merge(items[prices, nomatch=0], train, by = c('date', id), all.x = TRUE)
dataset <- dataset[is.na(units), units := 0]
setindex(dataset, mainCategory, category, subCategory)

# Set correct data types for each column
dataset[, `:=`(
  id = paste(pid,"-",size),
  date = fastPOSIXct(date, tz = 'GMT'),
  releaseDate = fastPOSIXct(releaseDate, tz = 'GMT'),
  brand = as.factor(brand),
  color = as.factor(color),
  mainCategory = as.factor(mainCategory),
  category = as.factor(category),
  subCategory = as.factor(subCategory)
)]

# Remove data set entries with date before release date
dataset <- dataset[date >= releaseDate]

# Add Date and Time related features here:
################################################################################

# @Ludwig
# Feature: For potential calculations, add the date in form of a unix epoch
dataset[, unixEpoch := as.numeric(date)]

# @Ludwig
# Feature: Distinguish between work day and weekend
# Add day of the week as new column (0 = Sunday, 6 = Saturday)
dataset[, weekday := as.integer(format(date, '%w'))]

# Add a logical to identify weekends
dataset[, isWeekend := weekday<2 ]
dataset[, weekday := as.factor(weekday)]


# @Johannes
# Feature: Distinguish between different weeks and months
# Add calendar week as new column. Those are not exactly calendar weeks but an auto-increment
# integer to identify weeks.
dataset[, weekNr := {
  day <- as.integer(format(date, format = "%j")) - 274
  day <- ifelse(day < 0, day + 365, day)
  floor(day / 7)
}]

# @Johannes
# Feature: Enhance info about product life cycle by adding time since release date
# Issue: Most products were release before 2017-10-01, but data is missing --> set to high value of 13000000 secs
dataset[, timeSinceRelease := ifelse(dataset$releaseDate == fastPOSIXct("2017-10-01"), 13000000, dataset$date - dataset$releaseDate)]

# @Johannes
# Features; Payday (in Germany last working day of the month, in US every two weeks on Friday)
#dataset[, paydayUS := dataset$date %in% fastPOSIXct(c('2017-10-06','2017-10-20','2017-11-03','2017-11-17','2017-12-01','2017-12-15','2017-12-29','2018-01-12','2018-01-26','2018-02-09','2018-01-23'), tz = 'GMT')]
dataset[, paydayDE := dataset$date %in% fastPOSIXct(c('2017-10-31','2017-11-30','2017-12-29','2018-01-31','2018-02-28'), tz = 'GMT')]

# @Johannes
# Feature: Payweek (take payday + next 6 days)
# Issue: Not sure if this would be useful for US as half of the time would be payweek
dataset[, payweekDE := dataset$date %in% fastPOSIXct(c('2017-10-31', as.character(as.Date('2017-10-31') + 1:6),
                                                     '2017-11-30', as.character(as.Date('2017-11-30') + 1:6),
                                                     '2017-12-29', as.character(as.Date('2017-12-29') + 1:6),
                                                     '2018-01-31', as.character(as.Date('2018-01-31') + 1:6),
                                                     '2018-02-28', as.character(as.Date('2018-02-28') + 1:6)), tz = 'GMT')]

# @Marina
# Feature: Public holidays in Germany according to https://xn--brckentage-beb.info/2017/feiertage-deutschland-2017.html
# Issue: We might need to use several days before or after the holiday instead of one date
dataset[, isHoliday := date %in% as.POSIXct(c("2017/10/01","2017/10/31","2017/12/25","2017/12/26","2018/01/01"), tz="GMT")]

## TODO Check for peaks For Black Friday 2017/11/24 and Cyber Monday 2017/11/27 
# @Teo: yes, there was a peak in black friday. See data exploration. 

## TODO Check if sales between Black Friday and Christmas are higher than for other periods
# @Teo: see data exploration
dataset[, isHolidaySeason := fastPOSIXct('2017-11-23', tz = 'GMT') < date & date < fastPOSIXct('2017-12-20', tz = 'GMT')]

## TODO One var or many dummies for holidays?

##
# TODO: Public Holidays - past week
##

# @Chrisotph
# Feature: zeroCumulative
# A Feature to coniser itermittent Demand 
dataset[, tmp_grp := cumsum(c(0, diff(units) != 0)), by = .(pid, size)][, zeroCumulative := ifelse(units == 0, seq_len(.N), 1L), by = .(pid, size, tmp_grp)][, zeroCumulative := zeroCumulative-1][, tmp_grp := NULL]
#subset <- dataset[order(id),list(id,date,units,zeroCumulative)]

# @Christoph
# Feature: Compare Units of each day with yesterday'sUnits
# Moving Price Average of last 7 days
# UnitsChangeLast7 bedeuted: der absolute Price Unterschied zwischen dem heutigem Datum und dem Durchschnitt der letzten 7 Tage (moving Average)
cols = paste('previousUnits', c(1:7), sep="_")
cols[1] <- "unitsYesterday"
subset <- dataset[,list(id,date,units)]
subset[, (cols) := shift(units, 1:7), by = id]
  subset[is.na(unitsYesterday), unitsYesterday := units]
  subset[is.na(previousUnits_2), previousUnits_2 := units]
  subset[is.na(previousUnits_3), previousUnits_3 := units]
  subset[is.na(previousUnits_4), previousUnits_4 := units]
  subset[is.na(previousUnits_5), previousUnits_5 := units]
  subset[is.na(previousUnits_6), previousUnits_6 := units]
  subset[is.na(previousUnits_7), previousUnits_7 := units]

subset[, movingAverageUnits:=(unitsYesterday+previousUnits_2+previousUnits_3+previousUnits_4+previousUnits_5+previousUnits_6+previousUnits_7)/7]
subset[, UnitsChangeLast7Days:=ifelse((movingAverageUnits-units)>0,TRUE,FALSE)]
subset[, (cols[-(1:3)]):=NULL] # add more than 1 units lag
subset[, units:=NULL]
dataset <- merge(x=dataset, y=subset, by = c('id', 'date'), all.x = TRUE)


# @Christoph
# Feature: number of units sold in total per day
dataset[, totalUnitsSoldPerDay := sum(units), by = date]

# @Jacqueline
# Feature: total (over all Items) Units sold yesterday 
dataset[, totalUnitsSoldYesterday := shift(totalUnitsSoldPerDay, 1L), by = .(id)]
# Units sold per 01.10.2018 set to unitsSoldPerDay
dataset[is.na(totalUnitsSoldYesterday), totalUnitsSoldYesterday := totalUnitsSoldPerDay]

################################################################################
# Add Pricing related features here:
################################################################################

# @Christoph
# Feature: Compare price of each day with yesterday's price
# Moving Price Average of last 7 days
# priceChangeLast7 bedeuted: der absolute Price Unterschied zwischen dem heutigem Datum und dem Durchschnitt der letzten 7 Tage (moving Average)
cols = paste('previousPrice', c(1:7), sep="_")
cols[1] <- "priceYesterday"
subset <- dataset[,list(id,date,price)]
subset[, (cols) := shift(price, 1:7), by = id]
  subset[is.na(priceYesterday), priceYesterday := price]
  subset[is.na(previousPrice_2), previousPrice_2 := price]
  subset[is.na(previousPrice_3), previousPrice_3 := price]
  subset[is.na(previousPrice_4), previousPrice_4 := price]
  subset[is.na(previousPrice_5), previousPrice_5 := price]
  subset[is.na(previousPrice_6), previousPrice_6 := price]
  subset[is.na(previousPrice_7), previousPrice_7 := price]

subset[, movingAveragePrice:=(priceYesterday+previousPrice_2+previousPrice_3+previousPrice_4+previousPrice_5+previousPrice_6+previousPrice_7)/7]
subset[, priceChangeLast7Days:=ifelse((movingAveragePrice-price)>0,1,0)]
subset[, (cols[-1]):=NULL] # add more than 1 price lag
subset[, price:=NULL]
dataset <- merge(x=dataset, y=subset, by = c('id', 'date'), all.x = TRUE)

# # @Ludiwg
# # Feature: Bin prducts by sales per day
# subset <- dataset[date < '2018-02-01', c('id', 'units')]
# subset[, meanSalesPerProduct := mean(units), by = 'id']
# subset$Bin_MeanSalesPerProduct <- discretize(subset$meanSalesPerProduct, method='cluster', categories=10)
# subset[, units:=NULL]
# dataset <- merge(x=dataset, y=subset, by='id', all.x = TRUE)

# Add relative price drop as new column
dataset[, priceDropYesterday := (price / priceYesterday - 1)]

# @Ludwig
# Feature: Compare the recommended retail price with the actual price
# Add the difference between rrp and price as new column
dataset[, rppMINUSprice := (rrp - price)]
# Add the relative difference between rrp and price as new column
dataset[, percentageOfRPP := (price / rrp - 1)]
# Add the relative difference (discount) between rrp and price as new column
dataset[, discountToRPP := (1 - price / rrp)]
# Add a binning for relative difference between rrp and price as new column
dataset[, binPercentageOfRPP := cut(percentageOfRPP, breaks = 10, ordered_result = TRUE, right = FALSE)]
dataset[, binDiscountToRPP := cut(discountToRPP, method = "interval", breaks = 10, ordered_result = TRUE, right = FALSE)]

# @Teo
# Feature: Revenue per day from sold items
#dataset[, revenue := (price * units)]
#dataset[, revenuePerDay := sum(revenue), by = date]

################################################################################
# Add Product related features here:
################################################################################

# @Johannes
# Feature: Bin different sizes
# 0-4 is used for shoe sizes, XS-XL is used for adult clothing, KXS-KL is used for kids clothing
sizemapping  <- fread('01_preparation/external_data/sizemapping.csv')
sizemapping[, category := as.factor(category)]
dataset <- merge(dataset, sizemapping, by = c('category', 'size'), all.x = TRUE)
dataset[is.na(newSize), newSize := 'XX']

#@Christoph
# Feature: Google Trend for Trend11teamsports
GoogleTrendShop <- fread('01_preparation/external_data/GoogleTrend11teamsports.csv')
GoogleTrendShop[, date := fastPOSIXct(date, tz = 'GMT')]
dataset <- merge(dataset, GoogleTrendShop[,list(date, trend11teamsports)], by = c('date'), all.x = TRUE)

# @Christoph
# Feature: CategoryPath
# Concatinate the different categories levels to make a clear combiniation possbiel
dataset[!is.na(subCategory), categoryPath := as.factor(paste(mainCategory,"-",category,"-",subCategory))]
dataset[is.na(subCategory), categoryPath := as.factor(paste(mainCategory,"-",category))]

dataset[, categoryPathSort := as.factor(paste(mainCategory,"-",category))]


# @Ludwig:
# Feature: Bin by sales info


# @Christoph
# Feature: NumberOfCompetingCategoryItems
# Number of items per CategroyPath
dataset[, numberOfCompetingCategoryItems := uniqueN(id), by = categoryPath]

# @Christoph
# Feature: NumberOfCompetingBrands
# Number of items per CategroyPath
dataset[, numberOfCompetingBrands := uniqueN(brand), by = categoryPath]

# @Ludwig
# Feature: Compare prices of competing styles
# Add the relative difference between price and the mean of competing styles prices as new column
# issue -> use category path here - to have more granular competing styles
dataset[, relPriceCompetingStyle := (price / mean(price) - 1), by = categoryPath]

# @Christoph:
# Bin Brands (Nike, Adidas, Puma, and OTHER)
dataset[!(brand %in% list("Nike","adidas", "PUMA")), brand:=as.factor("other")]
dataset[,brand:=factor(brand)]

# @Christoph:
# Bin Color (schwarz , blau,weiss, rot grau)
dataset[!(color %in% list("schwarz","blau", "weiss","rot", "grau")), color:=as.factor("other")]
dataset[,color:=factor(color)]

# @Teo
# Feature: RareItem
# wether the product was sold only once from october to february
dataset[, unitsTotalSoldPerItem := sum(units), by = id]
dataset[, rareItem := (unitsTotalSoldPerItem == 1)]

dataset[, units_test:=ifelse((units)>0,1,0)]
dataset[, class := ifelse(units>0,ifelse(units>1,ifelse(units>3,ifelse(units>9,ifelse(units>8,10,8),9),3),1),0)]
dataset$class <- as.factor(dataset$class)


# remove old Features / Clean dataset
dataset[, `:=`(
  category = NULL,
  subCategory = NULL
)]

# @Ludwig:
## Preprocessing Step: Outlier Handling
#barplot(dataset$units)
#Outliers <- c()
#max <- quantile(dataset[,units],0.99999, na.rm=TRUE) + (IQR(dataset[,units], na.rm=TRUE) * 1.5 )
#idx <- which(dataset[,units] > max)
#Outliers <- c(Outliers, idx) 
#dataset <- dataset[-Outliers,]
#barplot(dataset$units, ylim=c(0, max(dataset$units)))

#setKey
setkeyv(dataset, c('pid', 'size', 'date'))

# Save the created data set for modeling
dir.create('01_preparation/output', showWarnings = FALSE)
saveRDS(dataset, file = '01_preparation/output/dataset.rds')
