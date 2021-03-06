equity.eval <- function(symbol, bench, period='months', from=NULL, to=NULL,
                       duration = c('12 months', '36 months', '60 months'),
                       makeplots = TRUE,
                       twri = NULL, twrib = NULL) {

    ## given a single symbol and benchmark, creates:
    ##     dataframe containing duration, twrcum, alpha, beta, stdev, sharpe, twrcumb, stdevb, sharpeb
    ##     set of plots of: incremental TWR, 
    ##                      cumulative twr (twrcum), 
    ##                      incremental twr for symbol vs. benchmark,
    ##                      cumulative twr vs. standard deviation for symbol and benchmark

    ## options: symbol and bench names are taken (and overwritten) from twri and twrib if provided
    ##          from and to are used to define the maximum date range
    ##          duration = c('12 months', '36 months', '60 months') default makes plots for last 1, 3, and 5 yrs
    ##                   = c('1 year', '3 years', '5 years') makes plots for current year, last 3, and last 5
    ##                     Note: can specify any number of duration options
    ##                   = number instead returns the last this number of time steps
    ##                     Use this option with from/to to set a specific date range, e.g.:
    ##                         from='2020-10-28', to='2020-12-31', period='days', duration=99999 

    ## extract names of symbol and bench from twri and twrib if they were not input
    if (missing(symbol)) symbol <- names(twri)
    if (missing(bench))  bench  <- names(twrib)

    ## test that there is only one symbol or twri column
    ##                and only one bench or twrib column
    if ( (class(symbol) == 'character' & length(symbol) > 1) |
         (length(names(twri))  > 1) |
         (length(names(twrib)) > 1)
        ) {
        cat('/nError: Multiple symbols passed into equity.eval/n')
        cat(  '       through "symbol", "bench", "twri", or "twrib./n')
        return()
    }
    
    ## get history
    if (is.null(twri)) {
        ## twri not passed in so obtain
        out  <- equity.history(symbol, period=period, from=from, to=to)
        twri <- out$twri
    }
    
    if (is.null(twrib)) {
        ## twrib not passed in so obtain (no need to worry about dates since will line up later)
        twrib <- equity.history(bench, period=period)$twri
    }

    ## if from and/or to not defined, extract xtsrange from twri
    if (is.null(from)) from <- zoo::index(twri[1,])
    if (is.null(to))   to   <- zoo::index(twri[nrow(twri),])
    xtsrange <- paste(from, '/', to, sep='')
    twri  <- twri[xtsrange]
    twrib <- twrib[xtsrange]

    ## combine twri and benchmarks to line up dates and remove NA
    ## first make sure the same dates will be considered identical
    zoo::index(twri)  <- as.Date( zoo::index(twri) )  # align/fix xts dates before cbind merge
    zoo::index(twrib) <- as.Date( zoo::index(twrib) ) # align/fix xts dates before cbind merge
    both <- na.omit( cbind(twri, twrib) )

    ## set plotspace to fill by columns first
    par(mfcol=c(3, length(duration)))

    ## strip out 1-year, 3-year, and 5-year histories, if available
    alpha    <- NA
    beta     <- NA
    twrcum   <- NA
    twrcumb <- NA
    stdev    <- NA
    stdevb <- NA
    sharpe   <- NA
    sharpeb <- NA
    
    i        <- 0
    for (durationi in duration) {
        i <- i + 1
      
        ## extract vectors for alpha and beta
        bothx <- xts::last(both, durationi)
        twrix       <- as.numeric( bothx[, 1] )
        twribx  <- as.numeric( bothx[, 2] )
        
        ## calculate cumulative returns for durationi
        twrcum[i]   <- prod(twrix + 1) - 1
        twrcumb[i] <- prod(twribx + 1) - 1

        ## calculate standard deviation and sharpe ratio for durationi
        stdev[i]       <- sd(twrix)
        stdevb[i]  <- sd(twribx)
        sharpe[i]      <- twrcum[i]   / stdev[i]
        sharpeb[i] <- twrcumb[i] / stdevb[i]

        ## plot incremental and cumulative returns for input duration
        xtscum <- cumprod(bothx+1)-1
        if (isTRUE(makeplots)) {
            print( plotxts(bothx, main="Inc" ) )    # oddly "print" is needed in a loop
            print( plotxts(xtscum, main="Cum") )
            
            ## add cumulative TWR to plot
            mtext(paste('TWR Cum = ', signif(twrcum[i],4)*100, '%;',
                        'Benchmark Cum = ', signif(twrcumb[i], 4)*100, '%',
                        sep=''), 
                  side=3, line=0.75, cex=0.75)
        }

        ## determine alpha and beta for symbol i and create plot
        out <- alpha.beta(twrix, twribx, 
                          plot = makeplots, 
                          xlabel = paste('Incremental TWR for', bench, sep=' '),
                          ylabel = paste('Incremental TWR for', symbol, sep=' '),
                          range  = range(twrix, twribx, na.rm = TRUE),
                          main   = paste(durationi, '; period=', period, sep=''))
        alpha[i] <- out$alpha
        beta[i]  <- out$beta
        
    }
    df  <- data.frame(duration, twrcum, alpha, beta, stdev, sharpe,
                      twrcumb, stdevb, sharpeb)
    return(df)
}
## out <- equity.eval('AAPL', 'SPY', from='2020-10-11', to='2020-12-15',
##                   period='days', duration=99999 )
