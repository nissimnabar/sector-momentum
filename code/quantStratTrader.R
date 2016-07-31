require(quantmod)
require(PerformanceAnalytics)
require(downloader)

LongSeeker <- function(symbols, weights, rebalance_on = "years", 
                       displayStats = TRUE, outputReturns = FALSE) {
    getSymbols(symbols, src='yahoo', from = '1990-01-01')
    prices <- list()
    for(i in 1:length(symbols)) {
        if(symbols[i] == "ZIV") {
            download("https://www.dropbox.com/s/jk3ortdyru4sg4n/ZIVlong.TXT", destfile="ziv.txt")
            ziv <- xts(read.zoo("ziv.txt", header=TRUE, sep=",", format="%Y-%m-%d"))
            prices[[i]] <- Cl(ziv)
        } else if (symbols[i] == "VXX") {
            download("https://dl.dropboxusercontent.com/s/950x55x7jtm9x2q/VXXlong.TXT", 
                     destfile="vxx.txt")
            vxx <- xts(read.zoo("vxx.txt", header=TRUE, sep=",", format="%Y-%m-%d"))
            prices[[i]] <- Cl(vxx)
        }
        else {
            prices[[i]] <- Ad(get(symbols[i]))
        }
    }
    prices <- do.call(cbind, prices)
    prices <- na.locf(prices)
    returns <- na.omit(Return.calculate(prices))
    
    returns$zeroes <- 0
    weights <- c(weights, 1-sum(weights))
    stratReturns <- Return.portfolio(R = returns, weights = weights, rebalance_on = rebalance_on)
    
    if(displayStats) {
        stats <- rbind(table.AnnualizedReturns(stratReturns), maxDrawdown(stratReturns), CalmarRatio(stratReturns))
        rownames(stats)[4] <- "Max Drawdown"
        print(stats)
        charts.PerformanceSummary(stratReturns)
    }
    
    if(outputReturns) {
        return(stratReturns)
    }
} 

LongSeeker(c("XLP", "TLT"), c(.8, .6))
