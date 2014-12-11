# Jay Michels and Maddy Petersen
#
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library("forecast")
library("knitr")
library("quadprog")
library("lattice")
library("ggvis")
source("helper.R")

shinyServer(function(input, output, session) {
  lastTab <- reactive({ input$tabs })
  
  observe({
    if(input$navbar == "Price History") {
      updateTextInput(session,"symbols2", value=input$symbols1)
      updateTextInput(session,"symbols3", value=input$symbols1)
      updateTextInput(session,"symbols4", value=input$symbols1)
      updateTextInput(session,"symbols5", value=input$symbols1)
      updateTextInput(session,"symbols6", value=input$symbols1)
      updateDateRangeInput(session,"timeRange2", start=input$timeRange1[1], end=input$timeRange1[2])
      updateDateRangeInput(session,"timeRange3", start=input$timeRange1[1], end=input$timeRange1[2])
      updateDateRangeInput(session,"timeRange4", start=input$timeRange1[1], end=input$timeRange1[2])
      updateDateRangeInput(session,"timeRange5", start=input$timeRange1[1], end=input$timeRange1[2])
      updateDateRangeInput(session,"timeRange6", start=input$timeRange1[1], end=input$timeRange1[2])
      updateRadioButtons(session,"dataInterval2", selected=input$dataInterval1)
      updateRadioButtons(session,"dataInterval3", selected=input$dataInterval1)
      updateRadioButtons(session,"dataInterval4", selected=input$dataInterval1)
      updateRadioButtons(session,"dataInterval5", selected=input$dataInterval1)
      updateRadioButtons(session,"dataInterval6", selected=input$dataInterval1)
    } else if(input$navbar == "Time Series Analsis") {
      updateTextInput(session,"symbols1", value=input$symbols2)
      updateTextInput(session,"symbols3", value=input$symbols2)
      updateTextInput(session,"symbols4", value=input$symbols2)
      updateTextInput(session,"symbols5", value=input$symbols2)
      updateTextInput(session,"symbols6", value=input$symbols2)
      updateDateRangeInput(session,"timeRange1", start=input$timeRange2[1], end=input$timeRange2[2])
      updateDateRangeInput(session,"timeRange3", start=input$timeRange2[1], end=input$timeRange2[2])
      updateDateRangeInput(session,"timeRange4", start=input$timeRange2[1], end=input$timeRange2[2])
      updateDateRangeInput(session,"timeRange5", start=input$timeRange2[1], end=input$timeRange2[2])
      updateDateRangeInput(session,"timeRange6", start=input$timeRange2[1], end=input$timeRange2[2])
      updateRadioButtons(session,"dataInterval1", selected=input$dataInterval2)
      updateRadioButtons(session,"dataInterval3", selected=input$dataInterval2)
      updateRadioButtons(session,"dataInterval4", selected=input$dataInterval2)
      updateRadioButtons(session,"dataInterval5", selected=input$dataInterval2)
      updateRadioButtons(session,"dataInterval6", selected=input$dataInterval2)
    } else if(input$navbar == "Rates of Return") {
      updateTextInput(session,"symbols1", value=input$symbols3)
      updateTextInput(session,"symbols2", value=input$symbols3)
      updateTextInput(session,"symbols4", value=input$symbols3)
      updateTextInput(session,"symbols5", value=input$symbols3)
      updateTextInput(session,"symbols6", value=input$symbols3)
      updateDateRangeInput(session,"timeRange1", start=input$timeRange3[1], end=input$timeRange3[2])
      updateDateRangeInput(session,"timeRange2", start=input$timeRange3[1], end=input$timeRange3[2])
      updateDateRangeInput(session,"timeRange4", start=input$timeRange3[1], end=input$timeRange3[2])
      updateDateRangeInput(session,"timeRange5", start=input$timeRange3[1], end=input$timeRange3[2])
      updateDateRangeInput(session,"timeRange6", start=input$timeRange3[1], end=input$timeRange3[2])
      updateRadioButtons(session,"dataInterval1", selected=input$dataInterval3)
      updateRadioButtons(session,"dataInterval2", selected=input$dataInterval3)
      updateRadioButtons(session,"dataInterval4", selected=input$dataInterval3)
      updateRadioButtons(session,"dataInterval5", selected=input$dataInterval3)
      updateRadioButtons(session,"dataInterval6", selected=input$dataInterval3)
    } else if(input$navbar == "Moving Averages") {
      updateTextInput(session,"symbols1", value=input$symbols4)
      updateTextInput(session,"symbols2", value=input$symbols4)
      updateTextInput(session,"symbols3", value=input$symbols4)
      updateTextInput(session,"symbols5", value=input$symbols4)
      updateTextInput(session,"symbols6", value=input$symbols4)
      updateDateRangeInput(session,"timeRange1", start=input$timeRange4[1], end=input$timeRange4[2])
      updateDateRangeInput(session,"timeRange2", start=input$timeRange4[1], end=input$timeRange4[2])
      updateDateRangeInput(session,"timeRange3", start=input$timeRange4[1], end=input$timeRange4[2])
      updateDateRangeInput(session,"timeRange5", start=input$timeRange4[1], end=input$timeRange4[2])
      updateDateRangeInput(session,"timeRange6", start=input$timeRange4[1], end=input$timeRange4[2])
      updateRadioButtons(session,"dataInterval1", selected=input$dataInterval4)
      updateRadioButtons(session,"dataInterval2", selected=input$dataInterval4)
      updateRadioButtons(session,"dataInterval3", selected=input$dataInterval4)
      updateRadioButtons(session,"dataInterval5", selected=input$dataInterval4)
      updateRadioButtons(session,"dataInterval6", selected=input$dataInterval4)
    } else if(input$navbar == "Risk/Market Behavior") {
      updateTextInput(session,"symbols1", value=input$symbols5)
      updateTextInput(session,"symbols2", value=input$symbols5)
      updateTextInput(session,"symbols3", value=input$symbols5)
      updateTextInput(session,"symbols4", value=input$symbols5)
      updateTextInput(session,"symbols6", value=input$symbols5)
      updateDateRangeInput(session,"timeRange1", start=input$timeRange5[1], end=input$timeRange5[2])
      updateDateRangeInput(session,"timeRange2", start=input$timeRange5[1], end=input$timeRange5[2])
      updateDateRangeInput(session,"timeRange3", start=input$timeRange5[1], end=input$timeRange5[2])
      updateDateRangeInput(session,"timeRange4", start=input$timeRange5[1], end=input$timeRange5[2])
      updateDateRangeInput(session,"timeRange6", start=input$timeRange5[1], end=input$timeRange5[2])
      updateRadioButtons(session,"dataInterval1", selected=input$dataInterval5)
      updateRadioButtons(session,"dataInterval2", selected=input$dataInterval5)
      updateRadioButtons(session,"dataInterval3", selected=input$dataInterval5)
      updateRadioButtons(session,"dataInterval4", selected=input$dataInterval5)
      updateRadioButtons(session,"dataInterval6", selected=input$dataInterval5)
    } else if(input$navbar == "Portfolio Analysis") {
      updateTextInput(session,"symbols1", value=input$symbols6)
      updateTextInput(session,"symbols2", value=input$symbols6)
      updateTextInput(session,"symbols3", value=input$symbols6)
      updateTextInput(session,"symbols4", value=input$symbols6)
      updateTextInput(session,"symbols5", value=input$symbols6)
      updateDateRangeInput(session,"timeRange1", start=input$timeRange6[1], end=input$timeRange6[2])
      updateDateRangeInput(session,"timeRange2", start=input$timeRange6[1], end=input$timeRange6[2])
      updateDateRangeInput(session,"timeRange3", start=input$timeRange6[1], end=input$timeRange6[2])
      updateDateRangeInput(session,"timeRange4", start=input$timeRange6[1], end=input$timeRange6[2])
      updateDateRangeInput(session,"timeRange5", start=input$timeRange6[1], end=input$timeRange6[2])
      updateRadioButtons(session,"dataInterval1", selected=input$dataInterval6)
      updateRadioButtons(session,"dataInterval2", selected=input$dataInterval6)
      updateRadioButtons(session,"dataInterval3", selected=input$dataInterval6)
      updateRadioButtons(session,"dataInterval4", selected=input$dataInterval6)
      updateRadioButtons(session,"dataInterval5", selected=input$dataInterval6)
    }
  })
  
  startDate <- reactive({ input$timeRange1[1] })
  endDate <- reactive({ input$timeRange1[2] })
  symbols <- reactive({ strsplit(toupper(input$symbols1), "\\, |\\,| ")[[1]] })
  weights <- reactive({ calculateWeights(symbols(), as.double(strsplit(input$weights, "\\, |\\,| ")[[1]])) })
  
  startMonth <- reactive({ as.numeric(strftime(startDate(), "%m")) })
  startYear <- reactive({ as.numeric(strftime(startDate(), "%Y")) })

  priceHistories <- reactive({ 
    lapply(symbols(), function(symbol) getSymbol(symbol,startDate(),endDate(),input$dataInterval1,"yahoo"))
  })
  
  timeSeriesList <- reactive({
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
          { dataPerYear <- 252 })
    lapply(priceHistories(), function(priceHistory) ts(priceHistory$Adj.Close, start=c(startYear(),startMonth()),frequency=dataPerYear))
  })
  
  # Calculate rates for each asset
  portfolioRates <- reactive ({ 
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
          { dataPerYear <- 252 })
    calculatePortfolioRates(priceHistories(), symbols(), dataPerYear,startYear(),startMonth())
  })
  
  # Get price history for the 10-Year Interest Rates and Market History (S&P 500)
  riskFreeRates <- reactive({
    getSymbol("YAHOO/INDEX_TNX",startDate(),endDate(),input$dataInterval1,"quandl")
  })
  marketHistory <- reactive({
    getSymbol("YAHOO/INDEX_GSPC",startDate(),endDate(),input$dataInterval1,"quandl")
  })
  
  # Clean data, convert to TS, and calculate rates of return
  marketRates <- reactive({ 
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
          { dataPerYear <- 252 })
    calculateRatesFromPriceHistory(marketHistory(), dataPerYear, startYear(), startMonth())
  })
  
  output$priceHistoryPlots <- renderUI({
    colors <- rainbow(length(symbols()))
    names(colors) <- symbols()
    
    if(length(symbols()) > 1) {
      plotTitles <- c("Unified",symbols())
    } else {
      plotTitles <- symbols()
    }
    
    # Get the max/min bounds
    maxBounds <- c()
    minBounds <- c()
    lapply(symbols(), function(symbol) {
      ts <- timeSeriesList()[[which(symbols() == symbol)]]
      maxBounds <<- c(maxBounds, max(ts))
      minBounds <<- c(minBounds, min(ts))
    })
    
    # Create spots for all of the plots
    plotOutputList <- lapply(plotTitles, function(symbol) {
      plotname <- paste("plot", symbol, sep="")
      plotOutput(plotname, height=280, width=250)
    })
    
    # Convert the list to a tagList - this is necessary for proper display
    do.call(tagList, plotOutputList)
    
    # Generate and plot the unified plot
    lapply(plotTitles, function(plotname) {
      output[[plotname]] <- renderPlot({
        if(plotname == "Unified") {
          plot(timeSeriesList()[[1]], main="Unified Time Series", xlab="Year", ylab="Adj. Close", ylim=c(min(minBounds),max(maxBounds)), col = colors[[1]])
          lapply(symbols(), function(symbol) lines(timeSeriesList()[[which(symbols() == symbol)]], col = colors[[symbol]]))
        } else {
          plot(timeSeriesList()[[which(symbols() == plotname)]], main=plotname, xlab="Year", ylab="Adj. Close", col = colors[[plotname]])
        }
      })
    })
  })
  
  output$priceHistoryText <- renderText({
    paste("Symbol(s): ", paste(as.character(symbols()), collapse=", "))
  })
  
  output$tsAnalysisPlots <- renderUI({
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
          { dataPerYear <- 252 })
    
    # This loop is used to print the plots and box-tests of valid forecasts
    validForecastOutputList <- lapply(symbols(), function(symbol) {
      plotname <- paste("plotForecast", symbol, sep="")
      plotOutput(plotname, height=280, width=250)
    })
    
    # Convert the list to a tagList - this is necessary for proper display
    do.call(tagList, validForecastOutputList)
    
    # Loop through symbols and get the most accurate forecast
    lapply(symbols(), function(symbol) {
      # index of the symbol
      i <- which(symbols() == symbol)
      
      # Load price history for symobls based on parameters defined above
      priceHistory <- priceHistories()[[i]]
      ts <- timeSeriesList()[[i]]
      
      # Perform analysis using modeling techniques used in analyzing TS
      #Generate the forecasts and box tests for each Holt-Winters possibity
      forecast1 <- forecast.HoltWinters(HoltWinters(ts),h=dataPerYear)
      boxTest1 <- Box.test(forecast1$residuals,lag=20,type="Ljung-Box")
      forecast2 <- forecast.HoltWinters(HoltWinters(ts,gamma=FALSE),h=dataPerYear)
      boxTest2 <- Box.test(forecast2$residuals,lag=20,type="Ljung-Box")
      forecast3 <- forecast.HoltWinters(HoltWinters(ts,beta=FALSE,gamma=FALSE),h=dataPerYear)
      boxTest3 <- Box.test(forecast3$residuals,lag=20,type="Ljung-Box")
      
      #Compare the p-values to see which (if any) Holt-Winters forecast fits best
      pVals <- c(boxTest1$p.value, boxTest2$p.value, boxTest3$p.value)
      bestForecastIndex <- which(pVals == max(pVals))
      
      betaVal <- FALSE
      gammaVal <- FALSE
      main <- "Simple Exponential Model"
      forecastToPlot <- forecast3
      if(bestForecastIndex == 1) {
        betaVal <- TRUE
        gammaVal <- TRUE
        main <- "Holt-Winters Exponential Model"
        forecastToPlot <- forecast1
      } else if(bestForecastIndex == 2) {
        betaVal <- TRUE
        gammaVal <- FALSE
        main <- "Holt Exponential Model"
        forecastToPlot <- forecast2
      } 
      
      output[[symbol]] <- renderPlot({
        if(max(pVals) >= 0.1) {
          plot.forecast(forecastToPlot,main=paste(symbol, main))
        } else {
          main <- paste(symbol," Doesn't Fit Holt-Winters", sep="")
          plot(forecastToPlot, type="n", main=main)
        }
      })
    })
  })
  
  output$tsAnalysisText <- renderText({
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
          { dataPerYear <- 252 })

    finalMessage <- ""
    # Loop through symbols and get the most accurate forecast
    lapply(symbols(), function(symbol) {
      # index of the symbol
      i <- which(symbols() == symbol)
      
      # Load price history for symobls based on parameters defined above
      priceHistory <- priceHistories()[[i]]
      ts <- timeSeriesList()[[i]]
      
      # Perform analysis using modeling techniques used in analyzing TS
      #Generate the forecasts and box tests for each Holt-Winters possibity
      forecast1 <- forecast.HoltWinters(HoltWinters(ts),h=dataPerYear)
      boxTest1 <- Box.test(forecast1$residuals,lag=20,type="Ljung-Box")
      forecast2 <- forecast.HoltWinters(HoltWinters(ts,gamma=FALSE),h=dataPerYear)
      boxTest2 <- Box.test(forecast2$residuals,lag=20,type="Ljung-Box")
      forecast3 <- forecast.HoltWinters(HoltWinters(ts,beta=FALSE,gamma=FALSE),h=dataPerYear)
      boxTest3 <- Box.test(forecast3$residuals,lag=20,type="Ljung-Box")
      
      #Compare the p-values to see which (if any) Holt-Winters forecast fits best
      pVals <- c(boxTest1$p.value, boxTest2$p.value, boxTest3$p.value)
      bestForecastIndex <- which(pVals == max(pVals))
      
      betaVal <- FALSE
      gammaVal <- FALSE
      boxToPrint <- boxTest3
      if(bestForecastIndex == 1) {
        betaVal <- TRUE
        gammaVal <- TRUE
        boxToPrint <- boxTest1
      } else if(bestForecastIndex == 2) {
        betaVal <- TRUE
        gammaVal <- FALSE
        boxToPrint <- boxTest2
      } 
      temp <- paste(symbol, names(boxToPrint), sep=" - ")
      message <- paste(temp, boxToPrint,sep=": ", collapse="\n")
      finalMessage <<- c(finalMessage, message, sep="\n\n")
    })
    paste(finalMessage,sep="")
  })
  
  output$returnPlots <- renderUI({
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
           { dataPerYear <- 252 })
    
    # This loop is used to print the plots and box-tests of valid forecasts
    histogramPlots <- lapply(symbols(), function(symbol) {
      plotname <- paste("plotHistogram", symbol, sep="")
      plotOutput(plotname, height=280, width=250)
    })
    
    # Convert the list to a tagList - this is necessary for proper display
    do.call(tagList, histogramPlots)
    
    finalMessage <- ""
    lapply(symbols(), function(symbol) {
      # Load price history for symobls based on parameters defined above
      ts <- timeSeriesList()[[which(symbols() == symbol)]]
      
      ratesOfReturn <- (diff(ts)/ts)*100
      
      output[[symbol]] <- renderPlot({
        title <- paste(symbol, " Rates of Return", sep="")
        hist(ratesOfReturn, main=title)
      })
    })
  })
  
  output$returnText <- renderText({
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
          { dataPerYear <- 252 })

    finalMessage <- ""
    lapply(symbols(), function(symbol) {
      # index of the symbol
      i <- which(symbols() == symbol)
      
      # Load price history for symobls based on parameters defined above
      priceHistory <- priceHistories()[[i]]
      ts <- timeSeriesList()[[i]]
      
      ratesOfReturn <- (diff(ts)/ts)*100
      stdDevRatesOfReturn <- sd(ratesOfReturn)
      averageRateOfReturn <- mean(ratesOfReturn)
      averageAnnualRate <- averageRateOfReturn * dataPerYear
      returnStr <- paste(symbol,"'s Average Annual Rate of Return is ", specify_decimal(averageAnnualRate,2),"%\n",sep="")
      sdStr <- paste("The Standard Deviation of ",symbol,"'s Average Rate of Return is ", specify_decimal(stdDevRatesOfReturn,2),sep="")
      finalMessage <<- paste(finalMessage,paste(returnStr,sdStr,sep=""),"\n\n", sep="")
    })
    paste(finalMessage)
  })
  
  output$movingAveragePlots <- renderUI({
    if(input$dataInterval1 == "daily") {
      # This loop is used to print the plots and box-tests of valid forecasts
      movingAveragePlots <- lapply(symbols(), function(symbol) {
        plotname <- paste("plotMovingAverage", symbol, sep="")
        plotOutput(plotname, height=280, width=250)
      })
      
      # Convert the list to a tagList - this is necessary for proper display
      do.call(tagList, movingAveragePlots)
      
      # Loop through symbols and get the most accurate forecast
      lapply(symbols(), function(symbol) {
        output[[symbol]] <- renderPlot({
          # index of the symbol
          i <- which(symbols() == symbol)
          
          # Load price history for symobls based on parameters defined above
          priceHistory <- priceHistories()[[i]]
          ts <- timeSeriesList()[[i]]
          
          smallMovingAverage <- input$smallDayVal
          bigMovingAverage <- input$bigDayVal
          smallDay <- SMA(ts,n=smallMovingAverage)
          bigDay <- SMA(ts,n=bigMovingAverage)
          smallDayTs <- ts(smallDay,start=c(start(ts),1),frequency=frequency(ts))
          bigDayTs <- ts(bigDay,start=c(start(ts),1),frequency=frequency(ts))
          unifiedTs <- ts.union(smallDayTs,bigDayTs,ts)
          
          color<-rainbow(ncol(unifiedTs))
          fullTitle <- paste(symbol, " Simple Moving Averages",sep="")
          ts.plot(unifiedTs,col=color,xlab="Year",ylab="Adj. Close",main=fullTitle)
          smallCol <- paste(toString(smallMovingAverage)," Day",sep="")
          bigCol <- paste(toString(bigMovingAverage)," Day",sep="")
          colnames(unifiedTs) <- c(smallCol, bigCol, "Original")
          legend("bottomright",legend=colnames(unifiedTs), lty=1, col=color)
        })
      })
    }
  })
  
  output$movingAverageText <- renderText({
    if(input$dataInterval1 == "daily") {
      finalMessage <- ""
      # Loop through symbols and get the most accurate forecast
      lapply(symbols(), function(symbol) {
        # index of the symbol
        i <- which(symbols() == symbol)
        
        # Load price history for symobls based on parameters defined above
        priceHistory <- priceHistories()[[i]]
        ts <- timeSeriesList()[[i]]
        
        smallMovingAverage <- input$smallDayVal
        bigMovingAverage <- input$bigDayVal
        smallDay <- SMA(ts,n=smallMovingAverage)
        bigDay <- SMA(ts,n=bigMovingAverage)
        smallDayTs <- ts(smallDay,start=c(start(ts),1),frequency=frequency(ts))
        bigDayTs <- ts(bigDay,start=c(start(ts),1),frequency=frequency(ts))
        unifiedTs <- ts.union(smallDayTs,bigDayTs,ts)
        
        diffVector <- as.vector(smallDayTs - bigDayTs)
        buyPoints <- which(diff(sign(diffVector))>0)
        sellPoints <- which(diff(sign(diffVector))<0)
        buyDates <- paste(format(priceHistory$Date[buyPoints], format="%Y-%m-%d"),sep=", ")
        sellDates <- paste(format(priceHistory$Date[sellPoints], format="%Y-%m-%d"),sep=", ")
        buyStr <- paste("Buy ",symbol," on : ",buyDates,"\n",sep="")
        sellStr <- paste("Sell ",symbol," on : ",sellDates,"\n",sep="")
        finalMessage <<- c(finalMessage, buyStr,sellStr,"\n\n")
      })
      paste(finalMessage,sep="")
    } else {
      paste("You must use a 'Daily' interval in order to use Moving Average Analysis in this calculator.")
    }
  })
  
  output$riskMarketBehaviorText <- renderText({
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
          { dataPerYear <- 252 })

    # Compute annualized simple rates of return for risk free rate
    annualizedRiskFreeRate <- mean(riskFreeRates()[,7])
    riskFreeRateStr <- c("The annualized risk free rate is ",specify_decimal(annualizedRiskFreeRate,2),"%\n")
    
    # Compute annualized simple rates of return for marketTs
    averageRateOfReturn <- mean(marketRates())
    annualizedMarketRate <- averageRateOfReturn * dataPerYear
    marketRateStr <- c("The annualized market rate of return is ", specify_decimal(annualizedMarketRate,2),"%\n")
    
    # Calculate ERP
    erp <- annualizedMarketRate - annualizedRiskFreeRate
    erpStr<- c("The ERP is ", specify_decimal(erp,2),"%\n",sep="")
    
    # Vector used to manage portfolio weights
    portfolioWeights <- c()
    
    portfolioMessage <- ""
    lapply(symbols(), function(symbol) {
      # Calculate beta
      beta <- cov(marketRates(), portfolioRates()[which(symbols() == symbol)]$assetRates)/var(marketRates())
      betaStr <- c("The Beta value for ", symbol, " is ", specify_decimal(beta,2),"\n",sep="")
      
      # Expected annualized rate of return for each security
      expectedReturn <- annualizedRiskFreeRate + beta*erp
      expectedReturnStr <- c("The expected annualized rate of return for ", symbol, " is ", specify_decimal(expectedReturn,2),"%\n",sep="")
      
      # Define input weights for each security in portfolio
      portfolioWeights <- c(portfolioWeights, 1/length(symbols()))
      
      portfolioMessage <<- c(portfolioMessage, betaStr, expectedReturnStr, "\n\n")
    })
    
    paste(c(riskFreeRateStr,marketRateStr,erpStr,"\n",portfolioMessage), sep="")
  })
  
  output$covPlot <- renderPlot({
    covMatr <- cov(portfolioRates())
    colnames(covMatr) <- symbols()
    rownames(covMatr) <- symbols()
    levelplot(covMatr,main="Covariances",xlab=NULL, ylab=NULL,
              panel=function(...) {
                arg <- list(...)
                panel.levelplot(...)
                panel.text(arg$x, arg$y, round(arg$z,2))
              })
  })
  
  output$corrPlot <- renderPlot({
    corMatr <- cor(portfolioRates())
    colnames(corMatr) <- symbols()
    rownames(corMatr) <- symbols()
    levelplot(corMatr,main="Correlations",xlab=NULL, ylab=NULL,
              panel=function(...) {
                arg <- list(...)
                panel.levelplot(...)
                panel.text(arg$x, arg$y, round(arg$z,2))
              })
  })
  
  output$portfolioAnalysisPlot <- renderPlot({
    switch(input$dataInterval1,
           daily = { dataPerYear <- 252 },
           weekly = { dataPerYear<- 52 },
           monthly = { dataPerYear <- 12 },
           quarterly = { dataPerYear <- 4 },
           annual = { dataPerYear <- 1 },
           { dataPerYear <- 252 })

    if(length(symbols()) > 1) {
      # Compute annualized simple rates of return for risk free rate
      annualizedRiskFreeRate <- mean(riskFreeRates()[,7])
      
      # Compute annualized simple rates of return for marketTs
      averageRateOfReturn <- mean(marketRates())
      annualizedMarketRate <- averageRateOfReturn * dataPerYear
      
      # Calculate ERP
      erp <- annualizedMarketRate - annualizedRiskFreeRate
      
      possibleWeights <- data.frame()
      portfolioSDs <- c()
      portfolioBetas <- c()
      portfolioReturns <- c()
      
      lapply(symbols(), function(symbol) {
        assetRates <- portfolioRates()[which(symbols() == symbol)]$assetRates
        
        if(nrow(possibleWeights) == 0) {
          possibleWeights <<- data.frame(seq(0.00,1, by=0.025))
        } else {
          possibleWeights <<- data.frame(possibleWeights,seq(0.00,1, by=0.025))
        }
        
        # Calculate SDs
        portfolioSDs <<- c(portfolioSDs, sd(assetRates))
        
        # Calculate beta
        beta <- cov(marketRates(), assetRates)/var(marketRates())
        portfolioBetas <<- c(portfolioBetas, beta)
        
        # Expected annualized rate of return for each security
        expectedReturn <- annualizedRiskFreeRate + beta*erp
        portfolioReturns <<- c(portfolioReturns, expectedReturn)
      })
      
      # Get all combinations of weights which can equal 1
      possibleCombos <- expand.grid(possibleWeights)
      equalOneCombos <- possibleCombos[rowSums(possibleCombos) == 1,]
      possibleReturnRates <- apply(equalOneCombos, 1, function(row) row %*% portfolioReturns)
      possibleSDs <- apply(equalOneCombos, 1, function(row) sqrt((row %*% cov(portfolioRates()) %*% row)[1][1]))
      
      # Optimize the portfolio weights based on return and minimize variance
      risk.param <- 0
      returnsCovariance <- cov(portfolioRates())
      expectedReturns <- portfolioReturns * risk.param
      
      # Set desired return and make sure it's going to work
      expectedReturn <- as.double(input$expectedReturn)
      warningMsg <- ""
      if(is.na(expectedReturn)) {
        expectedReturn <- min(possibleReturnRates)+(max(possibleReturnRates) - min(possibleReturnRates))/2
      } else if(expectedReturn > max(possibleReturnRates)) {
        expectedReturn <- max(portfolioReturns)
        warningMsg <- paste("Your expected return was too large, so it was forced to the max possible return of ", specify_decimal(expectedReturn,2), sep="")
      } else if(expectedReturn < min(possibleReturnRates)) {
        expectedReturn <- min(possibleReturnRates)
        warningMsg <- paste("Your expected return was too small, so it was forced to the min possible return of ", specify_decimal(expectedReturn,2), sep="")
      }
      
      # Constraints: sum(x_i) = 1 & x_i >= 0 & Expected Return
      constraintMat <- cbind(1, c(portfolioReturns), diag(nrow(returnsCovariance)))
      constraintVec <- c(1, expectedReturn, rep(0, nrow(returnsCovariance)))
      meq <- 2
      qp <- solve.QP(returnsCovariance, expectedReturns, constraintMat, constraintVec, meq)
      qp$solution[abs(qp$solution) <= 1e-7] <- 0
      
      portfolioWeights <- weights()
      optimizedPortfolioWeights <- qp$solution
      weightsStr <- c("Your current portfolio was calculated with weights as follows:\n",paste("[",symbols(), specify_decimal(portfolioWeights,2),"]"),"\n")
      weightsStr <- c(weightsStr,"The portfolio weights needed to get your desired return are as follows:\n",paste("[",symbols(), specify_decimal(optimizedPortfolioWeights,2),"]"),"\n\n")
      
      # Calculate the expected return of the portfolio
      optimizedPortfolioReturn <- optimizedPortfolioWeights %*% portfolioReturns
      portfolioReturn <- portfolioWeights %*% portfolioReturns
      expectedReturnStr <- c("The expected mean annual rate of return for your current portfolio is: ", specify_decimal(portfolioReturn,2), "%\n\n", sep="")
      
      # Portfolio Variance and Standard Deviation
      optimizedPortfolioVariance <- (optimizedPortfolioWeights %*% cov(portfolioRates()) %*% optimizedPortfolioWeights)[1][1]
      portfolioVariance <- (portfolioWeights %*% cov(portfolioRates()) %*% portfolioWeights)[1][1]
      optimizedPortfolioSD <- sqrt(optimizedPortfolioVariance)
      portfolioSD <- sqrt(portfolioVariance)
      sdStr <- c("Your current portfolio has a standard deviation of:", specify_decimal(portfolioSD,2), "\n")
      sdStr <- c(sdStr, "The portfolio needed to get your desired return has a standard deviation of:", specify_decimal(optimizedPortfolioSD,2), "\n")
      
      # Plot the assets and the portfolio
      titles <- c(symbols(), "Current Portfolio", "Desired Portfolio")
      returns <- c(portfolioReturns, portfolioReturn, optimizedPortfolioReturn)
      stdDevs <- c(portfolioSDs, portfolioSD, optimizedPortfolioSD)
      
      par(mar=c(5.1,4.1,4.1,2.1))
      plot(possibleSDs, possibleReturnRates, xlab="Risk (Std. Dev.)", ylab="Return", main="Our Portfolio", col="steelblue",pch=20, cex=1.5)
      text(stdDevs, returns, titles, col="red", pos=1)
      points(stdDevs, returns, col="red", pch=10)
    }
    output$portfolioAnalysisText <- renderText({
      if(length(symbols()) == 1) {
        c("Please select at least one more ticker symbol to see more information about a portfolio!")
      } else {
        c(weightsStr, expectedReturnStr, warningMsg, "\n", sdStr)
      }
    })
  })
})
