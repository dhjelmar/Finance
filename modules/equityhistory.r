equityhistory <- function(symbol, from=NULL, to=Sys.Date(), source='yahoo', period='days') {
    ## function returns a dataframe of adjusted prices
    ## period = 'days' (default), 'weeks', 'months', 'quarters', or 'years' 

    ## e.g., equityhistory(c("SPY","EFA", "IJS", "EEM","AGG"), from='2005-01-01')
    ##       equityhistory(c("SPY","EFA", "IJS", "EEM","AGG"), from='2005-01-01')

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

    ## extract dates as a vector of dates
    dates <- as.Date( zoo::index( asset ) )
    if (is.null(from)) from  <- dates[1]
    
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
    
    ## calculate TWR
    twr  <- adjprice / xts::lag.xts(adjprice, 1) - 1
    ## remove 1st row since NA
    twr <- twr[-1,]
    
    ## ## the following does the same with matrices
    ## ## calculate return
    ## ## convert to matrix until maybe someday if I learn xts
    ## adjpricem <- as.matrix(adjprice)
    ## nrows <- nrow(adjpricem)
    ## twrm <- adjpricem[2:nrows,] / adjpricem[1:(nrows-1),] - 1
    ## ## convert back to xts
    ## ## twr <- xts::as.xts(twrm)
     
    ## restrict to duration
    ## xtsrange <- paste('"', noquote(duration[1]), '/', noquote(duration[2]), '"', sep='')
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
    xtsrange
    twr <- twr[xtsrange]
   
    ## calculate twrcum and standard deviation
    twrcum <- cumprod(twr+1) / (twr[1,]+1) - 1
    std    <- apply(twr[2:nrow(twr),], 2, sd, na.rm=TRUE)
    
    return(list(close = closeprice, adjprice = adjprice, twr=twr, twrcum=twrcum, std=std))
}

## out <- equityhistory(c('SPY', 'IWM', 'EFA', 'AGG', 'SHV'), from='1995-01-01', period='years')
## twr <- out$twr
