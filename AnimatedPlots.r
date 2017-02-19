source("animatedPlots.r")

sxSubListFiles <- Sys.glob("split.out/*.rds")


sxD0 <- readRDS(sxSubListFiles[1])[[1]]
sxDlast <- readRDS(sxSubListFiles[20])
sxDlast <- sxDlast[[length(sxDlast)]]

finalReturns <- getReturns(sxDlast,sxD0)

### Sort to get best performing shares since 2001
best <- sort(finalReturns,decreasing=TRUE)

library(RColorBrewer)



stockBarPlot <- function(dayReturns,dateText,stockLabs=NULL,ylim,...){
	widths <- dayReturns
	date <- as.Date(dateText)
	par(bg="black",fg="white",col.axis="white",col.lab="white",col.main="white",col.sub="white")
	##bp <- barplot(dayReturns,sub=format(date,"%b %Y"),width=widths,axisnames=FALSE,col="yellow",border=NA,yaxt="n",ylim=ylim,cex.sub=3,...)
	pal <- rep_len(brewer.pal(9,"Set3"),length.out=length(dayReturns))
	names(pal) <- names(dayReturns)
	bp <- barplot(dayReturns,sub=format(date,"%b %Y"),width=widths,axisnames=FALSE,col=pal,border=NA,main="Return on $1 investment",yaxt="n",ylim=ylim,cex.sub=3,...)
	tickpoints <- pretty(ylim)
	##full.tickpoints <- seq(0 , ylim[2], by=tickpoints[2])
	ticklabs <- paste("$",CurrencyFormat(tickpoints),sep="")
	##axis(2, at = ylim, labels = c("",""))
	axis(2, at = tickpoints, labels = ticklabs,las=1)
	### plot top 10 names
	##top10.ind <- order(dayReturns,decreasing=TRUE)[1:10]
	##text(bp[top10.ind],dayReturns[top10.ind],labels=nice.names[top10.ind],pos=3,col="yellow")
	
	text(
		bp[match(names(stockLabs),names(dayReturns))]
		,dayReturns[names(stockLabs)]
		,labels=stockLabs
		,pos=3
		,col=pal[names(stockLabs)]
	)
	
}


saveGIF({
	ani.options(interval = 0.2)
	
	yRegisterSize <- 20 #### window for resizing y axis
	yMaxRegister <- rep(10,times=yRegisterSize)  ### track max y limit
	yMax <- 100
	prevReturns <- -1
	jump <- 30
	
	## data for trace line
	#traceXmax <- length(sxSubListFiles) * floor(length(sxSubList) / jump)
	#traceYmax <- sum(finalReturns)
	#traceX <- 0
	#traceY <- 0
	
	### title
	plotTitle <- function(text,ypos,cex=10){
		par(bg="black",fg="black",col.axis="black",col.lab="black",col.main="black",col.sub="black")
		plot(0,0,xlim=c(-1,1),ylim=c(-1,1))
		for(i in 1:length(text)) text(0,ypos[i],text[i],col="white",cex=cex)
	}
	for(n in 1:5) plotTitle("$1",0)
	for(n in 1:5) plotTitle("invested",0.3,cex=5)
	for(n in 1:5) plotTitle(c("invested","in"),c(0.3,-0.3),cex=5)
	for(n in 1:10) plotTitle(c("2001"),0)

	for(file in sxSubListFiles){
		print(file)
		sxSubList <- readRDS(file)
		for(j in 1:floor(length(sxSubList) / jump)){
		    ###for(i in 1:20){
			i <- (j * jump)-jump+1
			print(paste(file,i))
			dayReturns <- getReturns(sxSubList[[i]],sxD0)
			
			##netReturn <- sum(dayReturns)
			
			
			## reorder dayReturns
			##dayReturns <- dayReturns[mOrder]
			dayReturns[is.element(names(dayReturns),c("YAHOO.NS_HCC","YAHOO.038680","YAHOO.TW_1416"))] <- 0  ### hide HCC
			if(sum(prevReturns != dayReturns) > 0){  ## values not all same (so not a weekend)
				##yMaxRegister <- c(yMaxRegister[2:yRegisterSize],max(dayReturns))
				##yMax <- max(yMaxRegister) * 1.2
				if(yMax < max(dayReturns)) yMax <- 1.2*max(dayReturns)
				
				
				### make stock labels
					sortedStocks <- sort(dayReturns,decreasing=TRUE)[1:10]				
					### show full title of stock?
					stockLabs <- nice.names[names(sortedStocks)[1:10]]				
					filter1 <- stocks$row.names == names(sortedStocks)[1]
					stockLabs[1] <- paste(stocks[filter1,"Name"]," (",stocks[filter1,"SX"],")",sep="")
					filter2 <- stocks$row.names == names(sortedStocks)[2]
					stockLabs[2] <- paste(stocks[filter2,"Name"]," (",stocks[filter2,"SX"],")",sep="")
					filter3 <- stocks$row.names == names(sortedStocks)[3]
					stockLabs[3] <- paste(stocks[filter3,"Name"]," (",stocks[filter3,"SX"],")",sep="")
					filter4 <- stocks$row.names == names(sortedStocks)[4]
					stockLabs[4] <- paste(stocks[filter4,"Name"]," (",stocks[filter4,"SX"],")",sep="")

				
				stockBarPlot(dayReturns,dateText=names(sxSubList)[i],stockLabs=stockLabs,ylim=c(0,yMax))
			}
			prevReturns <- dayReturns
		}
	}
	### pause
	for(n in 1:20) stockBarPlot(dayReturns,dateText=names(sxSubList)[i],stockLabs=stockLabs,ylim=c(0,yMax))
})



##### gif optimize
##convert animation.gif -dither none -deconstruct -layers optimize -depth 2 \( -clone 0--1 -background none +append -quantize transparent  -colors 8  -unique-colors -write mpr:cmap +delete \) -map mpr:cmap optimized.gif

