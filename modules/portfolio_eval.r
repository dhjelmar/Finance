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
                     plottype = c('twrc', 'rr', 'twri', 'ab', 'pe', 'pairs'),
                     label = 'symbol')
}

portfolio_eval <- function(holding,
                           weight, 
                           twri=NULL,
                           twrib,
                           from,
                           to  ,
                           period='months',
                           plottype=c('twrc', 'rr', 'twri', 'ab', 'pe', 'pairs'),
                           label = 'symbol',
                           main=NULL) {
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
    ##                  p/e ratio vs reward?
    
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
    if (length(plottype) == 2) {
        plotspace(1,2)
    } else if (length(plottype) == 3) {
        plotspace(1,3)
    } else if (length(plottype) == 4) {
        plotspace(2,2)
    } else if (length(plottype) == 5) {
        plotspace(2,3)
    }

    
    ##-----------------------------------------------------------------------------
    ## plot cumulative TWR
    if (sum(grepl('twrc', plottype) >= 1)) plot( plotxts(twrcum,
                                                         main=main) )
    
    
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
    perf  <- cbind(out, twrcuml, std)
    names(perf) <- c('Holding', 'label', 'twrcum', 'std')


    ##-----------------------------------------------------------------------------
    ## add alpha and beta to perf
    perf$alpha <- NA
    perf$beta  <- NA
    benchi <- as.numeric(twriall[, ncol(twriall)])
    for (i in 1:ncol(twriall)) {
        out <- alpha_beta(as.numeric(twriall[,i]), benchi, plot=FALSE)
        perf[i,]$alpha <- out$alpha
        perf[i,]$beta  <- out$beta
    }


    ##-----------------------------------------------------------------------------
    ## add p/e ratio and other stats to perf
    out <- quantmod::getQuote(holdings, src='yahoo',
                              what = quantmod::yahooQF(c('Previous Close',
                                                         'P/E Ratio',
                                                         'Price/EPS Estimate Next Year',
                                                         'Price/Book',
                                                         'EPS',
                                                         '50-day Moving Average',
                                                         '200-day Moving Average')))
    out$'Trade Time' <- NULL
    outname <- names(out)
    names(out) <- c('close', outname[2:ncol(out)])
    perf <- cbind(perf, out)

    
    ##-----------------------------------------------------------------------------

    ## separate into 3 dataframes
    perfhold  <- perf[perf$Holding != 'portfolio' & perf$Holding != 'benchmark',]
    perfport  <- perf[perf$Holding == 'portfolio',]
    perfbench <- perf[perf$Holding == 'benchmark',]

    
    ##-----------------------------------------------------------------------------
    ## risk/reward plot

    if (sum(grepl('rr', plottype) >= 1)) {
        
        ## separate into 3 dataframes
        perfhold  <- perf[perf$Holding != 'portfolio' & perf$Holding != 'benchmark',]
        perfport  <- perf[perf$Holding == 'portfolio',]
        perfbench <- perf[perf$Holding == 'benchmark',]
        
        ## determine reange making room for legend
        xrange <- range(perf$std, efdata$ef$efstd)
        xlim   <- c(min(xrange),
                    max(xrange) + 0.25*(max(xrange) - min(xrange)))
        ylim   <- range(perf$twrcum, efdata$ef$eftwrcum)

        ## plot holdings
        with(perfhold, plotfit(std, twrcum, label, interval='noline',
                             xlimspec=xlim, ylimspec=ylim,
                             xlabel = 'Standard Deviation',
                             ylabel = 'Cumulative TWR',
                             main   = main))
        ## add portfolio
        points(perfport$std, perfport$twrcum, col='red', pch=16)
        ## add benchmark
        points(perfbench$std, perfbench$twrcum, col='blue', pch=17)
        ## add efficient frontier line
        out <- ef(model='Schwab', efdata=efdata, addline=TRUE, col='black', lty=1, pch=3)
        out <- ef(model='simple', efdata=efdata, addline=TRUE, col='black', lty=2, pch=4)
        mtext('(portfolio = solid red circle; benchmark = solid blue triangle)',
              side=3, line=0.8, cex=1)
        mtext('(Schwab EF = solid line; S&P 500 / AGG EF = dotted line)',
              side=3, line=0, cex=1)

            
        ## ## add holdings
        ## plot(perfhold$std, perfhold$twrcum,
        ##      xlab = 'Standard Deviation',
        ##      ylab = 'Cumulative TWR',
        ##      col  = 'black', pch=1,
        ##      xlim=xlim, ylim=ylim)
        ## grid(col='gray70')
        ## ## add portfolio
        ## points(perfport$std, perfport$twrcum, col='red', pch=16)
        ## ## add benchmark
        ## points(perfbench$std, perfbench$twrcum, col='magenta1', pch=17)
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
    if (sum(grepl('twri', plottype) >= 1)) plot( plotxts(twriall,
                                                         main=main) )
    
    
    ##-----------------------------------------------------------------------------
    ## alpha/beta plot

    if (sum(grepl('ab', plottype) >= 1)) {
        
        ## create plot
        xrange <- range(perf$beta)
        xlim   <- c(min(xrange),
                    max(xrange) + 0.25*(max(xrange) - min(xrange)))
        ylim   <- range(perf$alpha)
        with(perfhold, plotfit(beta, alpha, label, interval='noline',
                             xlimspec=xlim, ylimspec=ylim,
                             xlabel = 'beta',
                             ylabel = 'alpha',
                             main   = main))
        ## add portfolio
        points(perfport$beta, perfport$alpha, col='red', pch=16)
        ## add benchmark
        points(perfbench$beta, perfbench$alpha, col='blue', pch=17)
        ## add subtitle text
        mtext('(portfolio = solid red circle; benchmark = solid blue triangle)',
              side=3, line=0.75, cex=1)
        
    }


    ##-----------------------------------------------------------------------------
    ## P/E plot
    if (sum(grepl('pe', plottype) >= 1)) {
        
        
    }

    ##-----------------------------------------------------------------------------
    
    ## add weights to perf and drop label parameter
    perf$weight <- c(weight, NA, NA)
    perf$label  <- NULL

    ## any correlation?
    if (sum(grepl('pairs', plottype) >= 1)) {
        ## identify performance info to output and use in correlation plot
        pairplot <- select(perf, twrcum, std, alpha, beta, 'P/E Ratio', 'Price/Book', weight)
        pairsdf(pairplot)
    }
    
    return(list(twri = twriall, performance=perf))
}

