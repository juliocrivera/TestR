library(quantmod)
library(TTR)
library(xts)
setwd("E:\\Julio1\\Documents\\R_TestsR_Tests")
source("FilesIO.R")
source("Analysis.R")
#we load the following files which have the names of the stocks we want to look at
# stockNamesBMV<-read.csv("AccionesBMV.csv",header=TRUE)
# stockNamesSIC<-read.csv("AccionesSIC.csv",header=TRUE)
# stockNamesInter<-read.csv("AccionesInter.csv",header=TRUE)
 # stockNamesBMV<-read.csv("bmv2.csv",header=TRUE)
# stockMarket<-stockNamesBMV[,1]


# tmp_file <- "bmv.csv"
#
# # Create dat by reading tmp_file
# dat <- read.csv(tmp_file,header=TRUE)
# 
# # Convert dat into xts
# xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))
# 
# # Read tmp_file using read.zoo
# dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")
# 
# # Convert dat_zoo to xts
# dat_xts <- as.xts(dat_zoo)

# dataset<-xts()
# 
#  # findStocks<-function(stockMarket)
#  # {
#   # # cool progress bar to see the % of completion
#   n <- length(stockNamesBMV)
#   pb <- txtProgressBar(min = 0, max = n, style=3)
#   # Actual loop:
#   for(i in 1:length(stockNamesBMV)) {
#     symbol<-toString(stockNamesBMV[i])
#     # specify the "from" date to desired start date
#     tryit <- try(getSymbols(symbol, src='google'))
#     if(inherits(tryit, "try-error")){
#       print(toString(stockNamesBMV[i]))
#       i <- i+1
# 
#     } else {
#       # specify the "from" date to desired start date
#       data <- getSymbols(symbol,from="2017-01-01", src='google')
#       dataset <- merge(dataset, Ad(get(stockNamesBMV[i])))
#       # myStock[i]<-data
#       rm(symbol)
#     }
#     setTxtProgressBar(pb, i)
#    }
#  #  # return(myStock)
#  # }

# myStocks<-findStocks(stockMarketbmv)

#  Symbolname <- "BMV:BIMBOA"
#  mydata <- getSymbols(Symbolname, src = "google", auto.assign = FALSE)
#  allData <- data.frame(Date=index(mydata),coredata(mydata))
#  ThreeMonths <- tail(allData,186)
#  X<-100 
# fieldsAnalyze<-c("Open","High","Low","Close","Volume")
# #
# #TBD change names in table to a generic one instead of BMV.BIMBOA. Open change to only Open or something 
# #
# 
# BMVThreshold<-5
# BMVMinimum<-0.5#Minimum for knowing how close closing was compared to high
# oneDOpen<-tail(ThreeMonths[,"BMV.BIMBOA.Open"],1)
# oneDLow<-tail(ThreeMonths[,"BMV.BIMBOA.Low"],1)
# oneDHigh<-tail(ThreeMonths[,"BMV.BIMBOA.High"],1)
# oneDClose<-tail(ThreeMonths[,"BMV.BIMBOA.Close"],1)
# Results<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
# Results[1]<-CandleAnalysis(oneDOpen,oneDLow,oneDHigh,oneDClose,BMVThreshold)
# print(Results)
# print(sum(Results))
# CheckforSpikes(closeV)
# 
# Analysis:
#   check each one then we save in a xts file how many of the weights it has something like this? needs to be a summatory, about the weights...
# Candles-6 months MACD-6 months RSI-6 months  aroon - MFI - 6 months Candle-Week SMA-Month Candle-Month SMA-Week
#  1               1             1              0                     0           0         0             0

#This works...
#
# symb1<-getSymbols(c("DWDP","AAPL","MCD","MRK","IBM","MMM","KO","DIS","CSCO","PG","TRV","UTX","HD","PFE","MSFT","VZ","GS","WMT","GE","INTC","BA","JPM","AXP","XOM","NKE","CAT","CVX","JNJ","UNH","V"),src='google',from='2014-11-24',to='2017-11-24')

#Symbolname <- "BMV:BIMBOA"
# mydata <- getSymbols(Symbolname, src = "google", auto.assign = FALSE)

myDataUSA<-getSymbols("NASDAQ:INTC",src="google",from='2014-11-24',to='2018-03-21',auto.assign = FALSE)
# allData <- data.frame(Date=index(mydata),coredata(mydata))
DataUSA<-data.frame(Date=index(myDataUSA),coredata(myDataUSA))

ThreeMonthsUSA<-tail(DataUSA,200)

trendRSI<-RSI(ThreeMonthsUSA[,"NASDAQ.INTC.Close"])
CheckRSI(trendRSI)
 # tenWeeks <- tail(allData,500)


# https://www.quantmod.com/examples/intro/


# MFIUSA<-MFI(ThreeMonthsUSA[,c("NASDAQ..INTC.High","NASDAQ..INTC.Low","NASDAQ..INTC.Close")],ThreeMonthsUSA[,"NASDAQ..INTC.Volume"])
# print(tail(MFIUSA,19))
# # 
# # Needed and Weight
#  # Candles-6 months
#  # MACD-6 months
#  # RSI-6 months
#  # aroon - MFI - 6 months
#  # Candle-Week
#  # SMA-Month
#  # Candle-Month
#  # SMA-Week
#  # 
#  # Values to look out for
# #MFI: 80 Overbought-50 normal - 20 oversold(bad)--90 truly overbought=RISK-Below 10 - risk, price unsustainable sometimes oversold 10 can give a breakout
# #Look out for Lower lows and higher lows meaning we go back until 
# #Volume seems different in stock charts compared to google...
# # Splits, distributions, and dividends are "artificial" changes in the price of a ticker symbol that create gaps on technical charts 
# # causing misleading signals from technical indicators. To eliminate those gaps, we decrease our historical prices 
# # (and increase our historical volume data) in a way that removes these misleading signals.
# # In order to prevent these kinds of misleading signals from appearing on our charts, 
# # we adjust all the historical data prior to the event. In the case of a 2-for-1 split, 
# # we divide all of the historical prices for the stock by 2 and multiply all of the historical volume by 2, 
# # so that the bars prior to the split match up smoothly with the bars that appear after the split.
#  #RSI:70+ Overbought- 50 normal-20 oversold
# # 
# # trendAroon<-aroon(tenWeeks[,c("BMV.BIMBOA.High","BMV.BIMBOA.Low")],n=25)
# # # 
# # # trendAroon
# # 
# # trendMACD_EMA<-MACD(tenWeeks[,"BMV.BIMBOA.Close"],maType="EMA")
# # 
# # trendMACD_SMA<-MACD(tenWeeks[,"BMV.BIMBOA.Close"],maType="SMA")
# 
# trendMFI<-MFI(tenWeeks[,c("BMV.BIMBOA.High","BMV.BIMBOA.Low","BMV.BIMBOA.Close")],tenWeeks[,"BMV.BIMBOA.Volume"])
# 
# trendRSI<-RSI(tenWeeks[,"BMV.BIMBOA.Close"])
# 
# 
