portfolio_eval_test <- function(twri=NULL, twrib=NULL) {
    holding <- c('SPLV', 'FAMEX', 'EFA', 'AGG', 'SHV')
    twri    <- equitytwr(holding)
    twrib   <- equitytwr('SPY', period='months')
    out <- portfolio(holding,
                     c(0.3, 0.2,   0.1,   0.3,  0.1),
                     twri  = twri,
                     twrib = twrib,
                     from = '2015-12-31',
                     to   = '2021-11-30',
                     plottype = 'cria',
                     label = 'symbol')
}

portfolio_eval <- function(holding,
                           weight, 
                           twri=NULL,
                           twrib,
                           from,
                           to  ,
                           period='months',
                           plottype='icra',
                           label = 'symbol',
                           portfolioname=NULL) {
    ## given: holding   = vector of 1 or more symbols of holdings in portfolio
    ##        weight   = vector of weights for each holding (needs to sum to 1)
    ##        from     = start date
    ##        to       = end date
    ##        twri     = xts object with output from equitytwr, if provided, for holdings
    ##        twrib    = xts object with output from equitytwr for bench
    ##        label    = 'symbol' uses holding symbols on risk/return and beta/alpha plots
    ##                 = 'simple' collapses all holdings to "holding"
    ## create plots of: risk/reward for portfolio, holdings, and benchmark
    ##                  performance history by year (bar chart?)
    ##                  alpha/beta for portfolio, holdings, and benchmark

    ## get equity history
    if (is.null(twri))      twri      <- equitytwr(holding, period=period)

    ## change NA to 0
    twri[is.na(twri)] <- 0

    ## create portfolio twri
    portfolio <- twri %*% as.matrix(weight)              # matrix
    ## turn back into xts
    portfolio <- xts::as.xts( zoo::as.zoo( portfolio, zoo::index(twri)))
    
    ## combine into single xts object and call the benchmark "benchmark"
    colnames(twrib) <- 'benchmark'
    twriall <- cbind(twri, portfolio, twrib)
    
    ## restrict to duration
    ## xtsrange <- paste('"', noquote(duration[1]), '/', noquote(duration[2]), '"', sep='')
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
    xtsrange
    twriall <- twriall[xtsrange]

    ## generate efficient frontier module data
    efdata <- ef(model='Schwab', from=from, to=to, addline=FALSE, col='black', lty=1, pch=3)
    nameseftwri <- names(efdata$twri)
    ## efdata will have a result for today while mutual funds will not if prior to close
    ## combine ef twri with others, remove NA, then split apart again
    temp <- na.omit( cbind(twriall, efdata$twri) )
    twriall     <- temp[, 1:ncol(twriall)]
    efdata$twri <- temp[, (ncol(twriall)+1):ncol(temp)]
    ## fix efdata$twri names in case they got renamed in cbind (i.e., if duplicates)
    names(efdata$twri) <- nameseftwri
    ## recall ef to fix other list parameters in efdata using new twri range
    efdata <- ef(model='Schwab', efdata=efdata, addline=FALSE, col='black', lty=1, pch=3)    

    ## extract names
    holdings <- names(twriall)

    ## cumulative twr and standard deviation
    twrcum  <- cumprod(twriall + 1) - 1
    twrcuml <- t( xts::last(twrcum) )
    std     <- as.matrix( apply(twriall, 2, sd, na.rm=TRUE) )

    ## reserve plotspace if plottype > 1
    if (nchar(plottype) == 2) {
        plotspace(1,2)
    } else if (nchar(plottype) == 3) {
        plotspace(1,3)
    } else if (nchar(plottype) == 4) {
        plotspace(2,2)
    }

    
    ##-----------------------------------------------------------------------------
    ## plot cumulative TWR
    if (grepl('c', plottype)) plot( plotxts(twrcum,
                                            main=portfolioname) )


    ##-----------------------------------------------------------------------------
    ## create dataframe of holdings, 'portfolio', and 'benchmark'
    out       <- data.frame(colnames(twriall))
    colnames(out) <- 'Holding'
    if (label == 'symbol') {
        ## use holding symbols on risk/return and beta/alpha plots
        out$lable <- out$Holding
    } else {
        ## use labels of 'holding', 'portfolio', or 'benchmark' on plot
        out$label <- 'holding'
        out[out$Holding == 'portfolio',]$label <- 'portfolio'
        out[out$Holding == 'benchmark',]$label <- 'benchmark'
    }
    rr  <- cbind(out, twrcuml, std)
    names(rr) <- c('Holding', 'label', 'twrcum', 'std')


    ##-----------------------------------------------------------------------------
    ## add alpha and beta to rr
    rr$alpha <- NA
    rr$beta  <- NA
    benchi <- as.numeric(twriall[, ncol(twriall)])
    for (i in 1:ncol(twriall)) {
        out <- alpha_beta(as.numeric(twriall[,i]), benchi, plot=FALSE)
        rr[i,]$alpha <- out$alpha
        rr[i,]$beta  <- out$beta
    }

    ## separate into 3 dataframes
    rrhold  <- rr[rr$Holding != 'portfolio' & rr$Holding != 'benchmark',]
    rrport  <- rr[rr$Holding == 'portfolio',]
    rrbench <- rr[rr$Holding == 'benchmark',]

    
    ##-----------------------------------------------------------------------------
    ## risk/reward plot

    if (grepl('r', plottype)) {

        ## separate into 3 dataframes
        rrhold  <- rr[rr$Holding != 'portfolio' & rr$Holding != 'benchmark',]
        rrport  <- rr[rr$Holding == 'portfolio',]
        rrbench <- rr[rr$Holding == 'benchmark',]
        
        ## determine reange making room for legend
        xrange <- range(rr$std, efdata$ef$efstd)
        xlim   <- c(min(xrange),
                    max(xrange) + 0.25*(max(xrange) - min(xrange)))
        ylim   <- range(rr$twrcum, efdata$ef$eftwrcum)

        ## plot holdings
        with(rrhold, plotfit(std, twrcum, label, interval='noline',
                             xlimspec=xlim, ylimspec=ylim,
                             xlabel = 'Standard Deviation',
                             ylabel = 'Cumulative TWR'))
        ## add portfolio
        points(rrport$std, rrport$twrcum, col='red', pch=16)
        ## add benchmark
        points(rrbench$std, rrbench$twrcum, col='blue', pch=17)
        ## add efficient frontier line
        out <- ef(model='Schwab', efdata=efdata, addline=TRUE, col='black', lty=1, pch=3)
        out <- ef(model='simple', efdata=efdata, addline=TRUE, col='black', lty=2, pch=4)
        mtext('(portfolio = solid red circle; benchmark = solid blue triangle)',
              side=3, line=1, cex=1)
        mtext('(Schwab EF = solid line; S&P 500 / AGG EF = dotted line)',
              side=3, line=0, cex=1)

            
        ## ## add holdings
        ## plot(rrhold$std, rrhold$twrcum,
        ##      xlab = 'Standard Deviation',
        ##      ylab = 'Cumulative TWR',
        ##      col  = 'black', pch=1,
        ##      xlim=xlim, ylim=ylim)
        ## grid(col='gray70')
        ## ## add portfolio
        ## points(rrport$std, rrport$twrcum, col='red', pch=16)
        ## ## add benchmark
        ## points(rrbench$std, rrbench$twrcum, col='magenta1', pch=17)
        ## 
        ## ## add efficient frontier line
        ## out <- ef(model='Schwab', efdata=efdata, addline=TRUE, col='black', lty=1, pch=3)
        ## out <- ef(model='simple', efdata=efdata, addline=TRUE, col='black', lty=2, pch=4)
        ## 
        ## ## add legend
        ## legend('bottomright', title=NULL,
        ##        col    = c('black',   'red',       'magenta1',  'black',     'black'),
        ##        legend = c('holding', 'portfolio', 'benchmark', 'Schwab EF', 'S&P 500 / AGG'),
        ##        lty    = c(NA,        NA,          NA,           1,           2),
        ##        pch    = c(1,         16,          17,           3,           4))
    }

    
    ##-----------------------------------------------------------------------------
    ## plot incremental TWR
    if (grepl('i', plottype)) plot( plotxts(twriall,
                                            main=portfolioname) )


    ##-----------------------------------------------------------------------------
    ## alpha/beta plot

    if (grepl('a', plottype) | grepl('b', plottype)) {
        
        ## create plot
        xrange <- range(rr$beta)
        xlim   <- c(min(xrange),
                    max(xrange) + 0.25*(max(xrange) - min(xrange)))
        ylim   <- range(rr$alpha)
        with(rrhold, plotfit(beta, alpha, label, interval='noline',
                             xlimspec=xlim, ylimspec=ylim,
                             xlabel = 'beta',
                             ylabel = 'alpha'))
        ## add portfolio
        points(rrport$beta, rrport$alpha, col='red', pch=16)
        ## add benchmark
        points(rrbench$beta, rrbench$alpha, col='blue', pch=17)
        ## add subtitle text
        mtext('(portfolio = solid red circle; benchmark = solid blue triangle)',
              side=3, line=1, cex=1)
        
    }

    ## add weights to rr
    rr$weight <- c(weight, NA, NA)

    ## any correlation
    stats <- select(rr, twrcum, std, alpha, beta, weight)
    pairsdf(stats)

    return(list(twri = twriall, summary=rr))
}
