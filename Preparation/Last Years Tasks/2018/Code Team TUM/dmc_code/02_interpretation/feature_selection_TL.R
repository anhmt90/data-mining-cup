# Use mRMR to identify best numerical features
# Documentation http://cran.revolutionanalytics.com/web/packages/mRMRe/mRMRe.pdf

# first run preparation file 


# # is something sold on not
datasetToIdentifyFeatures <- dataset
#datasetToIdentifyFeatures[, isSold:=ifelse(units>0,1,0)]
datasetToIdentifyFeatures$date <- NULL
datasetToIdentifyFeatures$id <- NULL
datasetToIdentifyFeatures$releaseDate <- NULL

onlyNumericalData <- Filter(is.numeric, datasetToIdentifyFeatures)

# transform int to num
onlyNumericalData$pid <- as.numeric(onlyNumericalData$pid)
onlyNumericalData$stock <- as.numeric(onlyNumericalData$stock)
onlyNumericalData$units <- as.numeric(onlyNumericalData$units)
onlyNumericalData$unitsYesterday <- as.numeric(onlyNumericalData$unitsYesterday)
onlyNumericalData$previousUnits_2 <- as.numeric(onlyNumericalData$previousUnits_2)
onlyNumericalData$previousUnits_3 <- as.numeric(onlyNumericalData$previousUnits_3)
onlyNumericalData$totalUnitsSoldPerDay <- as.numeric(onlyNumericalData$totalUnitsSoldPerDay)
onlyNumericalData$totalUnitsSoldYesterday <- as.numeric(onlyNumericalData$totalUnitsSoldYesterday)
onlyNumericalData$trend11teamsports <- as.numeric(onlyNumericalData$trend11teamsports)
onlyNumericalData$numberOfCompetingCategoryItems <- as.numeric(onlyNumericalData$numberOfCompetingCategoryItems)
onlyNumericalData$numberOfCompetingBrands <- as.numeric(onlyNumericalData$numberOfCompetingBrands)
onlyNumericalData$unitsTotalSoldPerItem <- as.numeric(onlyNumericalData$unitsTotalSoldPerItem)

# delete unique features
onlyNumericalData$pid <- NULL
onlyNumericalData$units_test <- NULL
onlyNumericalData$isSold <- NULL
#onlyNumericalData$units <- NULL
str(onlyNumericalData)

# preparation for using mRMRe
data(cgps)
data <- mRMR.data(data = onlyNumericalData)
# target_indices is the index of the feature you want to predict
results <- mRMR.ensemble(data = data, target_indices = 4, 
                         feature_count = 10, solution_count = 1)

# selected 10 features, by index 
solutions(results)
# relevance scores for selected 10 features
print(scores(results))
