# Jay Michels and Maddy Petersen

packages <- c("TTR","Quandl","xts","zoo","forecast")
#install.packages(packages)
lapply(packages, require, character.only=T)

getSymbol <- function(symbol, startDate, endDate, interval, src) {
  # Gets the price history for the given parameters.
  #
  # Args:
  #   symbol: String symbol we are getting history for. Make sure the symbol can be found on Yahoo! Finance or is a valid Quandl code.
  #   startDate: The start date (yyyy-mm-dd) for our financial data
  #   endDate:  The start date (yyyy-mm-dd) for our financial data
  #   interval: String representation of the frequency of the data (["daily","weekly","monthly","quarterly","annual"]).
  #   src: String representation of the source of the data (["yahoo","quandl"]).
  #   print: Bool value. Print first 5 rows if true. Don't print if false.
  #
  # Returns:
  #   Data frame of price history for the given symbol and the given parameters.
  priceHistory <- ""
  if(src == 'yahoo' || src == 'y') {
    frequency <- "d"
    if(interval=="monthly") {
      frequency <- "m"
    } else if(interval=="weekly") {
      frequency <- "w"
    }
    
    startDay <- as.numeric(strftime(startDate, "%d"))
    startMonth <- as.numeric(strftime(startDate, "%m"))
    startYear <- as.numeric(strftime(startDate, "%Y"))
    
    endDay <- as.numeric(strftime(endDate, "%d"))
    endMonth <- as.numeric(strftime(endDate, "%m"))
    endYear <- as.numeric(strftime(endDate, "%Y"))
    
    dataUrl <- paste("http://real-chart.finance.yahoo.com/table.csv?s=",symbol,"&a=",startMonth-1,"&b=",startDay,"&c=",startYear,"&d=",endMonth-1,"&e=",endDay,"&f=",endYear,"&g=",frequency,"&ignore=.csv",sep="")
    priceHistory <- read.csv(dataUrl, sep=",", header=1)
  } else {
    
    if(interval == "monthly" || interval == "annual" || interval == "weekly" || interval == "quarterly") {
      priceHistory <- Quandl(symbol, trim_start=startDate, trim_end=endDate, collapse=interval, authcode="cMQNfncQJMzsAswesLZn")
    } else {
      priceHistory <- Quandl(symbol, trim_start=startDate, trim_end=endDate, authcode="cMQNfncQJMzsAswesLZn")
    }
  }
  
  # Convert the date column into a date type
  priceHistory$Date <- as.Date(priceHistory$Date, "%Y-%m-%d")
  
  # Flipping the rows so that data goes from 2008 to 2013
  priceHistory=priceHistory[order(priceHistory$Date),]
  
  return(priceHistory)
}

plotForecastDetails <- function(forecast,title,betaVal,gammaVal) {
  # Plots the forecast and ACF of the given forecast.
  #
  # Args:
  #   forecast: Forecast to be plotted
  #   title: String representation of the forecast (preferably short). 
  #   betaVal: True/false representation of beta.
  #   gammaa: True/false representation of gamma.
  #
  # Returns:
  #   Plotted forecast and ACF
  main <- paste(title," Holt-Winters [beta=",betaVal,",gamma=",gammaVal,"]")
  plot(forecast,main=main)
}

# This uses the same techniques found in 1. and 4. from Part A
calculateRatesFromPriceHistory <- function(history, frequency, startYear, startMonth, isPrice = T) {
  ts <- ts(history, start=c(startYear,startMonth),frequency=frequency)
  # Only use the adjusted closing price piece of the time series
  ts <- ts[,7]
  
  # Compute annualized simple rates of return for risk free rate
  if(isPrice) {
    return((diff(ts)/ts)*100)
  }
  else {
    return(diff(ts))
  }
}

calculatePortfolioRates <- function(priceHistories, symbols, dataPerYear, startYear, startMonth) {
  groupedRates <- data.frame()
  lapply(symbols, function(symbol) {
    assetRates <- calculateRatesFromPriceHistory(priceHistories[[which(symbols == symbol)]], dataPerYear, startYear, startMonth)
    
    if(nrow(groupedRates) == 0) {
      groupedRates <<- data.frame(assetRates)
    } else {
      groupedRates <<- data.frame(groupedRates,assetRates)
    }
  })
  return(groupedRates)
}

calculateWeights <- function(symbols, weights) {
  if(length(weights) > length(symbols)) {
    weights <- weights[1:length(symbols)]
  } else if(length(weights) < length(symbols)) {
    weights <- c(weights, rep(0,length(symbols)-length(weights)))
  }
  
  weights[is.na(weights)] <- 0
  val <- sum(weights)
  
  if(val != 1) {
    weights <- rep(1/length(weights), length(weights)) 
  }
  
  return(weights)
}

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)