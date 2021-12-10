equity.history <- function(symbol, from=NULL, to=Sys.Date(), source='yahoo', period='days') {
    ## function returns a dataframe of adjusted prices
    ## period = 'days' (default), 'weeks', 'months', 'quarters', or 'years' 

    ## e.g., equity.history(c("SPY","EFA", "IJS", "EEM","AGG"), from='2005-01-01')
    ##       equity.history(c("SPY","EFA", "IJS", "EEM","AGG"), from='2005-01-01')

    ## install.packages('quantmod')

    ## Download historical info into dataframes with each symbol name.
    ## The last column from yahoo is the adjusted price.
    ## "While closing price merely refers to the cost of shares at the end of the day, 
    ##  the adjusted closing price considers other factors like dividends, stock splits, 
    ##  and new stock offerings. Since the adjusted closing price begins where the 
    ##  closing price ends, it can be called a more accurate measure of stocks' value"
    if (is.null(to)) to <- Sys.Date()

    ## get XTS object for symbol with historical closing price and adjusted price
    ## quantmod::getSymbols('SPY', src='yahoo')
    quantmod::getSymbols(symbol, 
                         src = source, 
                         auto.assign = TRUE, 
                         warnings = FALSE)
    
    ## copy XTS OHLCV format data for 1st symbol to variable asset
    asset <- get(symbol[1])

    if (period != 'days') {
        ## convert OHLCV format data to some other period
        asset <- xts::to.period(asset, period=period)
    }
    
    ## find column numbers with closing and adjusted prices
    colclose <- which(grepl('Close',    names(asset)))
    coladj   <- which(grepl('Adjusted', names(asset)))
    
    ## copy 1st asset information into XTS objects for closing and adjusted prices
    closeprice <- asset[, colclose]
    adjprice   <- asset[, coladj]
    
    ## merge adjusted prices for additional symbols, if any
    if (length(symbol) > 1) {
        for (i in 2:length(symbol)) {
            asset <- get(symbol[i])
            if (period != 'days') {
                ## convert OHLCV format data to some other period
                asset <- xts::to.period(asset, period=period)
            }
            closeprice <- merge(closeprice, asset[, colclose])
            adjprice   <- merge(adjprice  , asset[, coladj])
        }
    }

    ## fix names and return
    names(closeprice) <- symbol
    names(adjprice)   <- symbol

    ## approximate any missing prices
    closeprice <- zoo::na.approx(closeprice)
    adjprice   <- zoo::na.approx(adjprice)
    
    ## calculate incremental TWR
    twri  <- adjprice / xts::lag.xts(adjprice, 1) - 1
    ## remove 1st row since NA
    twri <- twri[-1,]
    
    ## ## the following does the same with matrices
    ## ## calculate return
    ## ## convert to matrix until maybe someday if I learn xts
    ## adjpricem <- as.matrix(adjprice)
    ## nrows <- nrow(adjpricem)
    ## twrim <- adjpricem[2:nrows,] / adjpricem[1:(nrows-1),] - 1
    ## ## convert back to xts
    ## ## twri <- xts::as.xts(twrim)

    ## extract dates as a vector of dates
    dates <- as.Date( zoo::index( twri ) )
    if (is.null(from)) from  <- dates[1]
     
    ## restrict to duration
    ## xtsrange <- paste('"', noquote(duration[1]), '/', noquote(duration[2]), '"', sep='')
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
    xtsrange
    twri <- twri[xtsrange]

    ## figure out how to divide xts object by a vector
    ## ## create test xts1 object
    ## a <- rbind(0.1, 0.2, 0.3)
    ## b <- rbind(0.01, 0.02, 0.03)
    ## xts1 <- xts::xts(cbind(a,b), order.by=Sys.Date()-3:1)
    ## ## calculate cumulative TWR
    ## twrcum_test <- t(t(cumprod(xts1+1)) / as.vector(xts1[1,]+1) - 1)
    ## class(twrcum_test)
    ## convert back to xts
    ## twrcum_test <- xts::as.xts( twrcum_test )

    ## calculate twrcum and standard deviation
    ## 1st date should have twrcum = 0
    twrcum <- xts::as.xts( t(t(cumprod(twri+1)) / as.vector(twri[1,]+1) - 1) )
    std    <- apply(twri[2:nrow(twri),], 2, sd, na.rm=TRUE)
    
    return(list(close = closeprice, adjprice = adjprice, twri=twri, twrcum=twrcum, std=std))
}

## out  <- equity.history(c('SPY', 'IWM', 'EFA', 'AGG', 'SHV'), from='1995-01-01', period='years')
## twri <- out$twri
