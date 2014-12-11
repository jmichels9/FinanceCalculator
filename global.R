source("helper.R")
library(shiny)

allAnalysisOptions <- c("Price History" = "priceHistory",
                        "Time Series Analysis" = "tsAnalysis", 
                        "Rates of Return" = "ratesOfReturn",
                        "Moving Averages" = "movingAverages",
                        "Risk and Market Behavior" = "riskMarketBehavior",
                        "Portfolio Anlaysis" = "portfolioAnalysis")