portfolio.summary <- function(twri, value, twrib) {

    ## Minimum input
    ## Given:     twri    = incremental TWR (time weighted return) for holdings
    ##            value   = xts object with value of each holding over time
    ##            twrib   = twri for benchmark
    ##            twri.ef = twri for efficient frontier

    ## Output:    twrc.ytd = twrc (cumulative TWR) for year to date
    ##            twrcb    = twrc for benchmark

    ## create dataframe for output
    df <- data.frame(description = c(names(twri), 'portfolio', names(twrib)))

    ## check that twri and value have the same dates (needed for portfolio weighting over time)
    if (!identical(zoo::index(value), zoo::index(twri))) {
        ## problem with input value and twri not being consistent
        cat('\n\n#######################################################\n')
        cat(    '#    WARNING: VALUE AND TWRI DATES NOT CONSISTENT      \n')
        cat(    '#######################################################\n\n')
        return()
    }

    ## determine portfolio value and weights for holdings
    value0      <- value / (twri + 1)                       # values if back out holding twri
    value.port  <- apply(value , 1, sum)                    # sum rows
    value0.port <- apply(value0, 1, sum)                    # sum rows
    twri.port   <- value.port / value0.port - 1
    weight      <- as.numeric( value[nrow(value),] / sum(value[nrow(value),]) )

    ## add weights and values to df
    weight <- c(weight, 1, rep(NA, ncol(twrib)))
    value  <- c(as.numeric(value[nrow(value),]), 
                sum(value[nrow(value),]), 
                rep(NA, ncol(twrib)))
    df$value  <- value
    df$weight <- weight
    
    ## turn twri.port back into xts object
    portfolio <- xts::as.xts( zoo::as.zoo( as.matrix(twri.port), zoo::index(twri)))
    colnames(portfolio) <- 'portfolio'
    
    ## combine into single xts object
    twriall <- cbind(twri, portfolio, twrib)
    
    ## set date ranges to evaluate
    dates <- zoo::index(twri)
    ##               YTD       1 year       3 years      5 years
    date.range <- c('1 year', '12 months', '36 months', '60 months')

    i <- 0
    twrc <- NA
    df.names <- names(df)
    ## add twrc values for various date ranges to df
    for (date.eval in date.range) {

        i <- i + 1
        
        ## extract timeframe for date.eval
        twri.eval <- xts::last(twriall, date.eval)
        
        ## identify corresponding xtsrange
        from <- zoo::index(twri.eval)[1]
        to   <- zoo::index(twri.eval)[nrow(twri.eval)]
        xtsrange <- paste(from, '/', to, sep='')

        ## above xtsrange words easiest for TWRC
        ## but need the time zero from date to later convert to twrc.annual
        from0.index <- grep(from, dates) - 1
        from0 <- zoo::index(twri)[from0.index]
        xtsrange0 <- paste(from0, '/', to, sep='')
        
        ## calculate twrc
        twrc  <- cumprod(twri.eval+1) - 1
        twrc.l <- t( xts::last(twrc) )    # last row as a column matrix

        ## annualize twrc
        ## average annual return
        ## days.held <- as.numeric( sum( diff(zoo::index(twri.eval)) ) )
        days.held <- as.numeric( sum( diff(zoo::index(twri[xtsrange0])) ) )
        if (days.held > 366) {
            twrc.ann  <- (1 + twrc.l)^(365.25 / days.held) - 1
        } else {
            twrc.ann  <- twrc.l
        }

        df <- cbind(df, twrc.ann)

    }

    row.names(df) <- seq(1, nrow(df))
    names(df) <- c(df.names, 'TWRC YTD', 'TWRC 1-yr', 'TWRC 3-yr', 'TWRC 5-yr')
    df.cols <- ncol(df)
    perf <- df
    perf$date <- zoo::index(twri.eval)[nrow(twri.eval)]
    
    ## change twrc, std, twrc.ann, and std.ann to character and apply % sign
    df$value  <- scales::dollar(df$value)
    df$weight <- round(df$weight, 2)
    df[4:df.cols] <- sapply(df[4:df.cols], function(x) scales::percent(x, accuracy=0.01))
    cat('\nEvaluation as of', as.character(zoo::index(twri.eval)[nrow(twri.eval)]), '\n')
    cat(  'TWRC for YTD is not annualized. Longer durations are annualized.\n')
    print(df)

    return(perf)
    
}
