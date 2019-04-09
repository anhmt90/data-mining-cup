##15.04.2018 - CS

# Load predicted  data
items  <- fread('input/items.csv')
id <- c('pid', 'size')
setkeyv(items, id)

prediction <- readRDS('03_modeling/output/prediction.rds')
prediction = result
prediction = as.data.table(prediction)
# calculate predicted stock
#prediction[, predicted_stock := stock - cumsum(prediction), by = .(pid, size)]
prediction[, predicted_stock := as.integer(stock) - as.double(cumsum(as.integer(prediction))), by = .(pid, size)]

#12700 verkauft
#prepare _Output
result <- items[, .(pid, size)]

# Create the result file
stock_out <- prediction[predicted_stock < 1, .SD[1], by = list(pid, size)]
stock_out <- stock_out[, .(pid, size, soldOutDate=date)]
stock_out$soldOutDate <- format(stock_out$soldOutDate, format="%Y-%m-%d")

result <- merge(result, stock_out, by = c('pid','size'), all.x = TRUE)
  if(any(is.na(result$soldOutDate))) {
    warning(paste(sum(is.na(result$soldOutDate)),' Items are not sold out in Feb!'))
    result <- result[is.na(soldOutDate), soldOutDate := "2018-02-28"]
  }


if((nrow(result)-nrow(items)!=0) | any(is.na(result$soldOutDate))) warning('Not all IDs are included!')
head(stock_out)

#Save Result  
dir.create('05_deployment/output', showWarnings = FALSE)
saveRDS(result, file = '05_deployment/output/prediction.rds')
dir.create('output', showWarnings = FALSE)
fwrite(result, 'output/TU_Munich_1.csv', sep = '|', dateTimeAs = 'write.csv')


ggplot(result) +
  geom_bar(aes(x = soldOutDate), 
           position = "dodge", stat = "count") 

#DONE
print("Berlin, Berlin, wir fahren nach Berlin!")
