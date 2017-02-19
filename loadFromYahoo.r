# R function to download stock data from Yahoo 
require(xts)

loadFromYahoo <- function(stockCode="AAPL",startDate=c("00","01","1980"),endDate=c("00","01","2017")){
	### WARNING: month value must range from 0 (Jan) to 11 (Dec).
	### giving month = 12 will produce all stock data since 1980
	url <- paste("http://ichart.finance.yahoo.com/table.csv?s=",stockCode,"&a=",startDate[1],"&b=",startDate[2],"&c=",startDate[3],"&d=",endDate[1],"&e=",endDate[2],"&f=",endDate[3],"&g=d&ignore=.csv",sep="")
	result <- read.table(url,header=TRUE,sep=",")
	xts( 
	  result[,-1], 
	  order.by=as.Date(result[,1])
	)
}

