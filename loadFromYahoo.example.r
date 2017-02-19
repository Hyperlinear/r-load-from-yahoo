
### LOAD DATA FROM YAHOO - EXAMPLE (R)

stockCodes <- c("AAPL","MSFT","IBM","GOOGL","TWTR","FB","INTC")
stockNames <- c("Apple","Microsoft","IBM","Google","Twitter","Facebook","Intel")
stocks <- lapply(stockCodes,loadFromYahoo)
stockCloseVals <- lapply(stocks,function(stockData){stockData$Adj.Close})
stocks.matrix <- do.call(cbind,stockCloseVals)
colnames(stocks.matrix) <- stockNames
