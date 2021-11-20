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
    
    return(twriall)
}
twribench <- equitytwr('SPY')
portfolio(c('AAPL', 'IFED'), c(0.2, 0.8), twribench=twribench, duration=c("2021-08-31","2021-10-30"))
portfolio(c('AAPL', 'IFED'), c(0.2, 0.8), twribench=twribench, duration=c("2021-09-01","2021-10-30"))

