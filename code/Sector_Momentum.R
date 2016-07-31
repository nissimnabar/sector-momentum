rm(list = ls())
library(quantmod)
library(quantstrat)
library(PerformanceAnalytics)
# List of ETFs to be used
assets <-c("SPY","VNQ","XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLV", "XLY", "XLU")

for(i in 1:length(assets)){
    getSymbols(assets[i],src = "google")
}

# Convert to Monthly OHLC-Vol-Adj.Close time series
SPY.monthly <- to.monthly(SPY)
VNQ.monthly <- to.monthly(VNQ)
XLB.monthly <- to.monthly(XLB)
XLE.monthly <- to.monthly(XLE)
XLF.monthly <- to.monthly(XLF)
XLI.monthly <- to.monthly(XLI)
XLK.monthly <- to.monthly(XLK)
XLP.monthly <- to.monthly(XLP)
XLV.monthly <- to.monthly(XLV)
XLY.monthly <- to.monthly(XLY)
XLU.monthly <- to.monthly(XLU)

colnames(SPY.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(VNQ.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLB.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLE.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLF.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLI.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLK.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLP.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLV.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLY.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")
colnames(XLU.monthly) <- c("Open", "High", "Low", "Close","Volume")#,"Adj.Close")

# Return could also be calculated as log(Close/Open)
# or as Close to Close

SPY.return <- NULL
VNQ.return <- NULL
XLB.return <- NULL
XLE.return <- NULL
XLF.return <- NULL
XLI.return <- NULL
XLK.return <- NULL
XLP.return <- NULL
XLV.return <- NULL
XLY.return <- NULL
XLU.return <- NULL



# Calculating the monthly return as (Close - Open)/Open

SPY.return$SPY.return <- (SPY.monthly$Close - SPY.monthly$Open)/SPY.monthly$Open
VNQ.return$VNQ.return <- (VNQ.monthly$Close - VNQ.monthly$Open)/VNQ.monthly$Open
XLB.return$XLB.return <- (XLB.monthly$Close - XLB.monthly$Open)/XLB.monthly$Open
XLE.return$XLE.return <- (XLE.monthly$Close - XLE.monthly$Open)/XLE.monthly$Open
XLF.return$XLF.return <- (XLF.monthly$Close - XLF.monthly$Open)/XLF.monthly$Open
XLI.return$XLI.return <- (XLI.monthly$Close - XLI.monthly$Open)/XLI.monthly$Open
XLK.return$XLK.return <- (XLK.monthly$Close - XLK.monthly$Open)/XLK.monthly$Open
XLP.return$XLP.return <- (XLP.monthly$Close - XLP.monthly$Open)/XLP.monthly$Open
XLV.return$XLV.return <- (XLV.monthly$Close - XLV.monthly$Open)/XLV.monthly$Open
XLY.return$XLY.return <- (XLY.monthly$Close - XLY.monthly$Open)/XLY.monthly$Open
XLU.return$XLU.return <- (XLU.monthly$Close - XLU.monthly$Open)/XLU.monthly$Open

# SPY.return$SPY.return <- Return.calculate(SPY.monthly)$Close
# VNQ.return$VNQ.return <- Return.calculate(VNQ.monthly)$Close
# XLB.return$XLB.return <- Return.calculate(XLB.monthly)$Close
# XLE.return$XLE.return <- Return.calculate(XLE.monthly)$Close
# XLF.return$XLF.return <- Return.calculate(XLF.monthly)$Close
# XLI.return$XLI.return <- Return.calculate(XLI.monthly)$Close
# XLK.return$XLK.return <- Return.calculate(XLK.monthly)$Close
# XLP.return$XLP.return <- Return.calculate(XLP.monthly)$Close
# XLV.return$XLV.return <- Return.calculate(XLV.monthly)$Close
# XLY.return$XLY.return <- Return.calculate(XLY.monthly)$Close
# XLU.return$XLU.return <- Return.calculate(XLU.monthly)$Close

# Set all the returns together
returnSeries <- cbind(SPY.return$SPY.return,
                      VNQ.return$VNQ.return,
                      XLB.return$XLB.return,
                      XLE.return$XLE.return,
                      XLF.return$XLF.return,
                      XLI.return$XLI.return,
                      XLK.return$XLK.return,
                      XLP.return$XLP.return,
                      XLV.return$XLV.return,
                      XLY.return$XLY.return,
                      XLU.return$XLU.return)


#write.csv(returnSeries,file = "~/Desktop/returnSeries.csv",row.names = FALSE)

returnSeries.df <- as.data.frame(returnSeries)
returnRank <- apply(-1*returnSeries,MARGIN = 1, FUN = rank)


returnWeight <- as.data.frame(matrix(data = 0,nrow = nrow(returnRank), ncol = ncol(returnRank)))

returnWeight[,2:ncol(returnRank)] <- ifelse(returnRank[,1:ncol(returnRank)-1] <4,0.33,ifelse(returnRank[,1:ncol(returnRank)-1] > 8,-0.33,0))
colnames(returnWeight) <- colnames(returnRank)

actualReturn <- t(as.matrix(returnWeight))*as.matrix(returnSeries)



totalReturn.monthly <- rowSums(actualReturn) 
totalReturn.monthly[1] <- 0

portfolioReturn <- cumprod(1+totalReturn.monthly)
plot(portfolioReturn,type = "l")
lsReturn <- totalReturn.monthly - SPY.return$SPY.return

LSportfolioReturn <- cumprod(1+lsReturn)
plot(LSportfolioReturn,type = "l")
