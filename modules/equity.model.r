equity.model <- function(symbol, period='months', from=NULL, to=NULL,
                        duration = c('1 year', '3 years', '5 years')) {

    ## get history and omit any NA
    out <- equity.history(symbol, period=period, from = from)  # 50 works, 60 does not
    twr <- na.omit( out$twr )

    ## set plotspace to fill by columns first
    par(mfcol=c(4, length(duration)))

    ## strip out 1-year, 3-year, and 5-year histories, if available
    alpha    <- NA
    beta     <- NA
    twrcum   <- NA
    stdev    <- NA
    sharpe   <- NA
    
    i        <- 0
    for (durationi in duration) {
        i <- i + 1
      
        ## extract vectors for alapha and beta
        twrx <- xts::last(twr, durationi)
        twrx <- as.numeric( twrx[, 1] )
        
        ## calculate cumulative returns for durationi
        twrcum[i]   <- prod(twrx + 1) - 1

        ## calculate standard deviation and sharpe ratio for durationi
        stdev[i]       <- sd(twrx)
        sharpe[i]      <- twrcum[i]   / stdev[i]

        ## ## plot incremental and cumulative returns for input duration
        ## print( plotxts(twrx, main="Incremental TWR" ) )    # oddly "print" is needed in a loop
        ## xtscum <- cumprod(twrx+1)-1

        ## plot histogram
        out <- hist_nwj(twrx+1, type='nwj', upperbound=FALSE,
                        main=paste("Incremental Returns for", symbol, sep=' '))
        abline(v=mean(twrx+1), col='red', lwd=1)

        ## plot qqplots
        out <- qqplot_nwj(twrx+1, type='n', main="for TWR+1")
        out <- qqplot_nwj(twrx+1, type='w', main="for TWR+1")
        out <- qqplot_nwj(twrx+1, type='j', main="for TWR+1")
            
    }
    df  <- data.frame(duration, twrcum, stdev, sharpe)
    return(df)
}
