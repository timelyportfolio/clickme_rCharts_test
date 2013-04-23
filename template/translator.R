get_rCharts_Cumul <- function(opts, div="chart"){
    require(rCharts)
    require(PerformanceAnalytics)
    
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
    
    data.cumul <- cumprod(1+opts$data)
    
    data.melt <- xtsMelt(data.cumul, "CumulativeGrowth")
    
    #remove troublesome . using modified method from this Stack Overflow
    #http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
    i <- sapply(data.melt, is.factor)
    data.melt[i] <- lapply(data.melt[i], gsub, pattern="\\.", replacement="")
    #get date as text
    data.melt$date <- format(data.melt$date,"%Y-%m-%d")  
    
    p1 <- rPlot(height = 400, value ~ date, data = data.melt, color = 'indexname', type = 'point', size=list(const=1)) 
    #p1$set( verticalSpacing =  25 )
    p1$set( legendPosition = "top" )
    p1$facet(type= "wrap",var="metric",cols=1,formatter="function(object) {return object.metric;}")
    
    html = p1$html(div)
    return(html)
}

get_rCharts_Drawdowns <- function(opts, div="chart") {
  require(rCharts)
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
  
  data.drawdowns <- Drawdowns(opts$data)
  
  data.melt <- xtsMelt(data.drawdowns, "Drawdown")
  
  #remove troublesome . using modified method from this Stack Overflow
  #http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
  i <- sapply(data.melt, is.factor)
  data.melt[i] <- lapply(data.melt[i], gsub, pattern="\\.", replacement="")
  #get date as text
  data.melt$date <- format(data.melt$date,"%Y-%m-%d")  
  
  p1 <- rPlot(height=200,value ~ date, data = data.melt, color = 'indexname', type = 'line', size=list(const=1)) 
  #p1$set( verticalSpacing =  25 )
  p1$set( legendPosition = "top" )
  p1$facet(type= "wrap",var="metric",cols=1,formatter="function(object) {return object.metric;}")
  
  html = p1$html(div)
  print(html)
  return(html) 
}