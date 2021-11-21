portfolio_test <- function() {
    symbol    <- c('AAPL', 'IFED')
    weight    <- c(0.2, 0.8)
    twribench <- equitytwr('SPY')
    from      <- '2021-06-01'
    to        <- '2021-10-30'
    portfolio(symbol, weight, twribench=twribench,
              from=from, to=to,
              plottype='r')
}

portfolio <- function(symbol,
                      weight, 
                      twri=NULL,
                      twribench,
                      from,
                      to  ,
                      period='months',
                      plottype='r') {
    ## given: symbol   = vector of 1 or more symbols of holdings in portfolio
    ##        weight   = vector of weights for each symbol (needs to sum to 1)
    ##        from     = start date
    ##        to       = end date
    ##        twri     = xts object with output from equitytwr if provided for symbols
    ##        twribench = xts object with output from equitytwr if provided for bench
    ## create plots of: risk/reward for portfolio, symbols, and benchmark
    ##                  performance history by year (bar chart?)
    ##                  alpha/beta for portfolio, symbols, and benchmark

    ## get equity history
    if (is.null(twri))      twri      <- equitytwr(symbol, period=period)

    ## change NA to 0
    twri[is.na(twri)] <- 0

    ## create portfolio twri
    portfolio <- twri %*% as.matrix(weight)              # matrix
    ## turn back into xts
    portfolio <- xts::as.xts( zoo::as.zoo( portfolio, zoo::index(twri)))
    
    ## combine into single xts object and call the benchmark "benchmark"
    colnames(twribench) <- 'benchmark'
    twriall <- cbind(twri, portfolio, twribench)

    ## restrict to duration
    ## xtsrange <- paste('"', noquote(duration[1]), '/', noquote(duration[2]), '"', sep='')
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
    xtsrange
    twriall <- twriall[xtsrange]

    ## extract names
    symbols <- names(twriall)

    ## cumulative twr and standard deviation
    twrcum  <- cumprod(twriall + 1) - 1
    twrcuml <- t( xts::last(twrcum) )
    std     <- as.matrix( apply(twriall, 2, sd, na.rm=TRUE) )

    ## risk/reward plot
    key <- data.frame(colnames(twriall))
    rr  <- as.data.frame( cbind(key, twrcuml, std) )
    names(rr) <- c('key', 'twrcum', 'std')
    if (grepl('r', plottype)) {

        ## simple option with color but benchmark and portfolio scattered throughout
        ## with(rr, plotfit(std, twrcum, key, interval='noline'))
        
        ## determine reange
        xlim <- c(min(rr$std),
                  max(rr$std) + 0.2*(max(rr$std) - min(rr$std)))
        ylim <- range(rr$twrcum)

        ## add holdings
        holdings <- rr[rr$key != 'portfolio' & rr$key != 'benchmark',]
        plot(holdings$std, holdings$twrcum,
             xlab = 'Standard Deviation',
             ylab = 'Cumulative TWR',
             col  = 'black', pch=1,
             xlim=xlim, ylim=ylim)
        ## add portfolio
        rrport <- rr[rr$key == 'portfolio',]
        points(rrport$std, rrport$twrcum, col='red', pch=16)
        ## add benchmark
        rrbench <- rr[rr$key == 'benchmark',]
        points(rrbench$std, rrbench$twrcum, col='magenta1', pch=17)

        ## add efficient frontier line
        out <- ef(model='Schwab', from=from, to=to, addline=TRUE, col='black', lty=1, pch=3)
        out <- ef(model='simple', from=from, to=to, addline=TRUE, col='black', lty=2, pch=4)

        ## add legend
        legend('topright', title=NULL,
               col    = c('black',   'red',       'magenta1',  'black',     'black'),
               legend = c('holding', 'portfolio', 'benchmark', 'Schwab EF', 'S&P 500 / AGG'),
               lty    = c(NA,        NA,          NA,           1,           2),
               pch    = c(1,         16,          17,           3,           4))
    }
    
    ## add efficient frontier lines to plot
    

    a <- 1
    a <- 1
    a <- 1
    a <- 1
    a <- 1
    
    return(list(twri = twriall, summary=rr))
}

