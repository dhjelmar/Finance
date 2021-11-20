portfolio <- function(symbol,
                      weight, 
                      twri=NULL,
                      twribench,
                      duration=NULL,
                      period='months') {
    ## given: symbol   = vector of 1 or more symbols of holdings in portfolio
    ##        weight   = vector of weights for each symbol (needs to sum to 1)
    ##        duration = ??
    ##        twri     = xts object with output from equitytwr if provided for symbols
    ##        twribench = xts object with output from equitytwr if provided for bench
    ## create plots of: risk/reward for portfolio, symbols, and benchmark
    ##                  performance history by year (bar chart?)
    ##                  alpha/beta for portfolio, symbols, and benchmark

    ## get equity history
    if (is.null(twri))      twri      <- equitytwr(symbol)

    ## change NA to 0
    twri[is.na(twri)] <- 0

    ## create portfolio twri
    twriport <- twri %*% as.matrix(weight)              # matrix
    ## turn back into xts
    twriport <- xts::as.xts( zoo::as.zoo( twriport, zoo::index(twri)))
    
    ## combine into single xts object
    twriall <- cbind(twri, twriport, twribench)

    ## restrict to duration
    ## xtsrange <- paste('"', noquote(duration[1]), '/', noquote(duration[2]), '"', sep='')
    xtsrange <- paste(noquote(duration[1]), '/', noquote(duration[2]), sep='')
    xtsrange
    twriall <- twriall[xtsrange]

    ## extract names
    symbols <- names(twriall)

    ## cumulative twr and standard deviation
    twrcum  <- cumprod(twriall + 1) - 1
    twrcuml <- t( xts::last(twrcum) )
    std     <- as.matrix( apply(twriall, 2, sd, na.rm=TRUE) )

    ## risk/reward plot
    rr <- as.data.frame( cbind(twrcuml, std) )
    names(rr) <- c('twrcum', 'std')
    rr$key <- rownames(rr)
    with(rr, plotfit(std, twrcum, key, interval='noline'))

    ## add efficient frontier lines to plot
    

    a <- 1
    a <- 1
    a <- 1
    a <- 1
    a <- 1
    
    return(twriall)
}
## twribench <- equitytwr('SPY')
## symbol   <- c('AAPL', 'IFED')
## weight   <- c(0.2, 0.8)
## duration <- c("2021-09-01","2021-10-30")
## portfolio(symbol, weight, twribench=twribench, duration=duration)

