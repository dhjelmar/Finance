equity.eval <- function(symbol, bench, period='months', from=NULL, to=NULL,
                       duration = c('1 year', '3 years', '5 years'),
                       makeplots = TRUE) {

    ## given a single symbol and benchmark, creates:
    ##     dataframe containing duration, twrcum, alpha, beta, stdev, sharpe, benchcum, benchstdev, benchsharpe
    ##     set of plots of: increemntal TWR, 
    ##                      cumulative twr (twrcum), 
    ##                      incremental twr for symbol vs. benchmark,
    ##                      cumulative twr vs. standard deviation for symbol and benchmark
  
    ## get history
    out <- equity.history(symbol, period=period, from=from, to=to)  # 50 works, 60 does not
    twr <- out$twr
    benchtwr <- equity.history(bench, period=period)$twr
    
    ## combine twr and benchmarks to line up dates and remove NA
    both <- na.omit( cbind(twr, benchtwr) )

    ## set plotspace to fill by columns first
    par(mfcol=c(3, length(duration)))

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
    for (durationi in duration) {
        i <- i + 1
      
        ## extract vectors for alapha and beta
        bothx <- xts::last(both, durationi)
        twrx       <- as.numeric( bothx[, 1] )
        benchtwrx  <- as.numeric( bothx[, 2] )
        
        ## calculate cumulative returns for durationi
        twrcum[i]   <- prod(twrx + 1) - 1
        benchcum[i] <- prod(benchtwrx + 1) - 1

        ## calculate standard deviation and sharpe ratio for durationi
        stdev[i]       <- sd(twrx)
        benchstdev[i]  <- sd(benchtwrx)
        sharpe[i]      <- twrcum[i]   / stdev[i]
        benchsharpe[i] <- benchcum[i] / benchstdev[i]

        ## plot incremental and cumulative returns for input duration
        xtscum <- cumprod(bothx+1)-1
        if (isTRUE(makeplots)) {
            print( plotxts(bothx, main="Incremental TWR" ) )    # oddly "print" is needed in a loop
            print( plotxts(xtscum, main="Cumulative TWR") )
            
            ## add cumulative TWR to plot
            mtext(paste('TWR Cum = ', signif(twrcum[i],4)*100, '%;',
                        'Benchmark Cum = ', signif(benchcum[i], 4)*100, '%',
                        sep=''), 
                  side=3, line=1, cex=0.75)
        }

        ## determine alpha and beta for symbol i and create plot
        out <- alpha.beta(twrx, benchtwrx, 
                          plot = makeplots, 
                          xlabel = paste('Incremental TWR for', bench, sep=' '),
                          ylabel = paste('Incremental TWR for', symbol, sep=' '),
                          range  = range(twrx, benchtwrx, na.rm = TRUE),
                          main   = durationi)
        alpha[i] <- out$alpha
        beta[i]  <- out$beta
        
    }
    df  <- data.frame(duration, twrcum, alpha, beta, stdev, sharpe,
                      benchcum, benchstdev, benchsharpe)
    return(df)
}
