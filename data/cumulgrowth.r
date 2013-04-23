require(PerformanceAnalytics)

data(managers)

managers.cumul <- cumprod(1+managers[,c(1,8,9)])

xtsMelt <- function(xtsData,metric){
  df <- data.frame(index(xtsData),coredata(xtsData),stringsAsFactors=FALSE)
  df.melt <- melt(df,id.vars=1)
  df.melt <- data.frame(df.melt,rep(metric,NROW(df.melt)))
  #little unnecessary housekeeping
  df.melt <- df.melt[,c(1,2,4,3)]
  colnames(df.melt) <- c("date","indexname","metric","value")
  df.melt$date <- as.Date(df.melt$date)
  return(df.melt)
}

managers.melt <- xtsMelt(managers.cumul, "CumulativeGrowth")

#remove troublesome . using modified method from this Stack Overflow
#http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
i <- sapply(managers.melt, is.factor)
managers.melt[i] <- lapply(managers.melt[i], gsub, pattern="\\.", replacement="")
#get date as text
managers.melt$date <- format(managers.melt$date,"%Y-%m-%d")  

p1 <- rPlot(value ~ date, data = managers.melt, color = 'indexname', type = 'point', size=list(const=1), height = 500) 
#p1$set( verticalSpacing =  25 )
p1$set( legendPosition = "top" )
p1$facet(type= "wrap",var="metric",cols=1,formatter="function(object) {return object.metric;}")
p1

