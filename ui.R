# Jay Michels and Maddy Petersen
#
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage("Calculator", 
                   theme="bootstrap.css",id="navbar",
                   tabPanel("Price History",id="priceHistory",
                            sidebarPanel(
                              textInput("symbols1",
                                        "Symbol(s):",
                                        value = "IBM, YUM, T"),
                              dateRangeInput("timeRange1", 
                                             "Date Range: yyyy-mm-dd",
                                             start = "2011-01-01",
                                             end= "2013-12-31",
                                             min = "1980-01-01"),
                              radioButtons("dataInterval1", "Data interval:",
                                           c("Daily" = "daily",
                                             "Weekly" = "weekly",
                                             "Monthly" = "monthly",
                                             "Quarterly" = "quarterly",
                                             "Annually" = "annual")),
                              submitButton("Submit")
                            ), mainPanel(width=12,
                                         uiOutput("priceHistoryPlots"),
                                         verbatimTextOutput("priceHistoryText"))
                   ), tabPanel("Time Series Analysis",id="tsAnalysis",
                               sidebarPanel(
                                 textInput("symbols2",
                                           "Symbol(s):"),
                                 dateRangeInput("timeRange2", 
                                                "Date Range: yyyy-mm-dd",
                                                start = "2011-01-01",
                                                end= "2013-12-31",
                                                min = "1980-01-01"),
                                 radioButtons("dataInterval2", "Data interval:",
                                              c("Daily" = "daily",
                                                "Weekly" = "weekly",
                                                "Monthly" = "monthly",
                                                "Quarterly" = "quarterly",
                                                "Annually" = "annual")),
                                 submitButton("Submit")
                               ), mainPanel(width=12,
                                            uiOutput("tsAnalysisPlots"),
                                            verbatimTextOutput("tsAnalysisText"))
                   ), tabPanel("Rates of Return",id="ratesOfReturn",
                               sidebarPanel(
                                 textInput("symbols3",
                                           "Symbol(s):"),
                                 dateRangeInput("timeRange3", 
                                                "Date Range: yyyy-mm-dd",
                                                start = "2011-01-01",
                                                end= "2013-12-31",
                                                min = "1980-01-01"),
                                 radioButtons("dataInterval3", "Data interval:",
                                              c("Daily" = "daily",
                                                "Weekly" = "weekly",
                                                "Monthly" = "monthly",
                                                "Quarterly" = "quarterly",
                                                "Annually" = "annual")),
                                 submitButton("Submit")
                               ), mainPanel(width=12,
                                            verbatimTextOutput("returnText"),
                                            uiOutput("returnPlots"))
                   ), tabPanel("Moving Averages",id="movingAverages",
                               sidebarPanel(
                                 textInput("symbols4",
                                           "Symbol(s):"),
                                 dateRangeInput("timeRange4", 
                                             "Date Range: yyyy-mm-dd",
                                             start = "2011-01-01",
                                             end= "2013-12-31",
                                             min = "1980-01-01"),
                                 radioButtons("dataInterval4", "Data interval:",
                                              c("Daily" = "daily",
                                                "Weekly" = "weekly",
                                                "Monthly" = "monthly",
                                                "Quarterly" = "quarterly",
                                                "Annually" = "annual")),
                                 sliderInput("smallDayVal",
                                             "Small Value for Moving Average",
                                             0,100,50,step=5),
                                 sliderInput("bigDayVal",
                                             "Large Value for Moving Average",
                                             50,250,200,step=5),
                                 submitButton("Submit")
                               ), mainPanel(width=12,
                                            uiOutput("movingAveragePlots"),
                                            verbatimTextOutput("movingAverageText"))
                   ), tabPanel("Risk/Market Behavior",id="riskMarketBehavior",
                               sidebarPanel(
                                 textInput("symbols5",
                                           "Symbol(s):"),
                                 dateRangeInput("timeRange5", 
                                             "Date Range: yyyy-mm-dd",
                                             start = "2011-01-01",
                                             end= "2013-12-31",
                                             min = "1980-01-01"),
                                 radioButtons("dataInterval5", "Data interval:",
                                              c("Daily" = "daily",
                                                "Weekly" = "weekly",
                                                "Monthly" = "monthly",
                                                "Quarterly" = "quarterly",
                                                "Annually" = "annual")),
                                 submitButton("Submit")
                               ), mainPanel(width=12,
                                            verbatimTextOutput("riskMarketBehaviorText"))
                   ), tabPanel("Portfolio Analysis",id="portfolioAnalysis",
                               sidebarPanel(
                                 textInput("symbols6",
                                           "Symbol(s):"),
                                 textInput("weights",
                                           "Weight(s):",
                                           value="0.333, 0.333, 0.333"),
                                 textInput("expectedReturn",
                                           "Desired Portfolio Return:",
                                           value = "8"),
                                 dateRangeInput("timeRange6", 
                                                "Date Range: yyyy-mm-dd",
                                                start = "2011-01-01",
                                                end= "2013-12-31",
                                                min = "1980-01-01"),
                                 radioButtons("dataInterval6", "Data interval:",
                                              c("Daily" = "daily",
                                                "Weekly" = "weekly",
                                                "Monthly" = "monthly",
                                                "Quarterly" = "quarterly",
                                                "Annually" = "annual")),
                                 submitButton("Submit")
                               ), mainPanel(width=12,
                                            plotOutput("portfolioAnalysisPlot"),
                                            verbatimTextOutput("portfolioAnalysisText"),
                                            plotOutput("corrPlot"),
                                            plotOutput("covPlot"))
                   )
))