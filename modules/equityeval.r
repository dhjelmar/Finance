equityeval <- function(symbol, bench, period='months') {

    ## get history
    out <- equityhistory(symbol, period=period)  # 50 works, 60 does not
    twr <- out$twr
    benchtwr <- equityhistory(bench, period=period)$twr
    
    ## combine twr and benchmarks to line up dates and remove NA
    both <- na.omit( cbind(twr, benchtwr) )

       
    ## plot incremental and cumulative returns for input duration
    plotspace(2,1)
    print( plotxts(both, main="Incremental TWR", ) )    # oddly "print" is needed in a loop
    xtscum <- cumprod(both+1)-1
    print( plotxts(xtscum, main="Cumulative TWR") )

    ## set plotspace to fill by columns first
    par(mfcol=c(4,3))

    ## strip out 1-year, 3-year, and 5-year histories, if available
    alpha    <- NA
    beta     <- NA
    twrcum   <- NA
    benchcum <- NA
    stdev    <- NA
    benchstdev <- NA
    sharpe   <- NA
    benchsharpe <- NA
    
    i        <- 0
    duration <- c('1 year', '3 years', '5 years')
    for (durationi in duration) {
        i <- i + 1
      
        ## extract vectors for alapha and beta
        bothx <- xts::last(both, durationi)
        twrx       <- as.numeric( bothx[, 1] )
        benchtwrx  <- as.numeric( bothx[, 2] )
   
        ## determine alpha and beta for symbol i
        out <- alpha_beta(twrx, benchtwrx, 
                          plot = TRUE, 
                          xlabel = paste('Incremental TWR for', bench, sep=' '),
                          ylabel = paste('Incremental TWR for', symbol, sep=' '),
                          range  = range(twrx, benchtwrx, na.rm = TRUE),
                          main   = durationi)
        alpha[i] <- out$alpha
        beta[i]  <- out$beta
        
        ## calculate cumulative returns for durationi
        twrcum[i]   <- prod(twrx + 1) - 1
        benchcum[i] <- prod(benchtwrx + 1) - 1

        ## calculate standard deviation and sharpe ratio for durationi
        stdev[i]       <- sd(twrx)
        benchstdev[i]  <- sd(benchtwrx)
        sharpe[i]      <- twrcum[i]   / stdev[i]
        benchsharpe[i] <- benchcum[i] / benchstdev[i]

        ## add cumulative TWR to alpha/beta plot
        mtext(paste('TWR Cum = ', signif(twrcum[i],4)*100, '%;',
                    'Benchmark Cum = ', signif(benchcum[i], 4)*100, '%',
                    sep=''), 
              side=3, line=0, cex=0.75)
        
        ## plot histogram of alpha and beta for symbol 1
        out <- hist_nwj(twrx, type='nj', upperbound=FALSE,
                        main="Histogram of Incremental Returns")
        abline(v=mean(twrx), col='red', lwd=1)
        out <- qqplot_nwj(twrx, type='n')
        out <- qqplot_nwj(twrx, type='j')
    }
    df  <- data.frame(duration_years=c(1,3,5), twrcum, alpha, beta, stdev, sharpe,
                      benchcum, benchstdev, benchsharpe)
    return(df)
}
