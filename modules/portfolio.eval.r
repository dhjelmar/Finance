## next:
## - xtsrange rather than from and to

portfolio.eval_test <- function(twri=NULL, twrib=NULL) {

    holding <- c('SWPPX', 'AGG') 
    from <- '2016-12-30'   # 2016-12-31 is a Saturday
    to   <- '2021-11-30'
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')

    ##-----------------------------------------------------------------------------
    ## verification 1
    ## test case where look up holding and baseline twri from Yahoo
    ## the following should return:
    ##    (1) holdings at end of "S&P 500 / AGG EF" line
    ##    (2) portfolio on halfway pointbetween 3rd and 4th point on that line
    twrib   <- 'SPY'
    period  <- 'months'
    out1 <- portfolio.eval(holding,
                           weight = rep(1/length(holding), length(holding)),
                           twrib = twrib,
                           from = from,
                           to   = to,
                           plottype = c('twrc', 'rr', 'twri', 'ab', 'pe', 'pairs'),
                           ## plottype = c('twrc', 'rr', 'twri', 'ab'),
                           label = 'symbol',
                           main  = 'verification 1')

    ##-----------------------------------------------------------------------------
    ## verification 2
    ## where supply twri xts objects
    ## need to see get the same results as the first case
    twri    <- equity.twri(holding, period=period)
    twrib   <- equity.twri('SPY'  , period=period)
    out2 <- portfolio.eval(holding,
                           weight = rep(1/length(holding), length(holding)),
                           twri  = twri,
                           twrib = twrib,
                           from = from,
                           to   = to,
                           plottype = c('twrc', 'rr', 'twri', 'ab'),
                           ## plottype = c('twrc', 'rr', 'twri', 'ab'),
                           label = 'symbol',
                           main  = 'verification 2')

    ##-----------------------------------------------------------------------------
    ## verification 3
    ## supply twri but short by the starting date
    twri <- out2$twri     # twri starts from 2016-12-30
    out3 <- portfolio.eval(holding,
                           weight = rep(1/length(holding), length(holding)),
                           twri  = twri,
                           twrib = twrib,
                           from  = from,
                           to    = to, 
                           plottype = c('twrc', 'rr', 'twri', 'ab'),
                           ## plottype = c('twrc', 'rr', 'twri', 'ab'),
                           label = 'symbol',
                           main  = 'verification 3')

    ##-----------------------------------------------------------------------------
    ## verification 4
    ## use Schwab 70/30 model as benchmark
    twri   <- equity.twri(holding)[xtsrange]
    from   <- zoo::index(twri[1,])
    to     <- zoo::index(twri[nrow(twri),])
    efdata <- ef(model='Schwab', from=from, to=to, addline=FALSE)
    twrib  <- (efdata$eftwri$schwab_60_40 + efdata$eftwri$schwab_80_20) / 2
    names(twrib) <- 'Schwab_70_30'
    ## twrib  <- efdata$eftwri$schwab_80_20
    out4   <- portfolio.eval(holding,
                             weight = rep(1/length(holding), length(holding)),
                             twri  = twri,
                             twrib = twrib,
                             efdata = efdata,
                             from = from,
                             to   = to,
                             plottype = c('twrc', 'rr', 'twri', 'ab'),
                             ## plottype = c('rr'),
                             ## plottype = c('twrc', 'rr', 'twri', 'ab'),
                             label = 'symbol',
                             main  = 'verification 4; benchmark = Schwab 70/30')
    ## lines(efdata$ef$efstd, efdata$ef$eftwrcum, type='b', col='red', lty=3, pch=1)
    ## twrib.std  <- sd(twrib[2:nrow(twrib)])
    ## twrib.twrc <- twrc.calc(twrib, zero.from=TRUE)
    ## points(twrib.std, xts::last(twrib.twrc))
}

portfolio.eval <- function(holding,
                           weight    = NA, 
                           rebalance = 'period',
                           value     = NA, 
                           twri      = NULL,
                           twrib     = 'SPY',
                           efdata    = NULL,
                           from,
                           to  ,
                           period='months',
                           na = 'omit',
                           plottype=c('twrc', 'rr', 'twri', 'ab'),
                           arrange = TRUE,   # TRUE figures out best layout
                           label = 'symbol',
                           main=NULL) {

    ## given: holding   = vector of 1 or more symbols of holdings in portfolio
    ##        weight    = vector of weights to use for rebalancing
    ##        rebalance = 'period' (default) to rebalance every period to same weight
    ##                  = 'years' to rebalance at end of each year
    ##                  = 'no' to let assets grow without rebalancing
    ##        value     = matrix of values for each holding for each period
    ##        twri      = xts object with output from equity.twri, if provided, for holdings
    #3                    xts object can contain more than what is in holding
    ##        twrib     = 'SPY' (default) uses SPY ETF as benchmark
    ##                  = some other stock symbol for use as benchmark
    ##                  = xts object with output from equity.twri for benchmark
    ##        from      = start date
    ##        to        = end date
    ##        period    = 'months' (default) for each increment in time (overruled if twri provided)
    ##                  = 'days'
    ##                  = 'years'
    ##        na        = option to handle holdings with no twri
    ##                  = 'omit' (default) restrict twri for all holdings to dates where all exist;
    ##                    this option may result in a crash if too short a history for some holding
    ##                  = 'zero' changes twri from NA to zero for any holding where not defined
    ##        plottype  = 'twrc' for cumulative TWR
    ##                  = 'rr' for risk/reward
    ##                  = 'twri' for incremental TWR
    ##                  = 'ab' for alpha/beta
    ##        arrange   = TRUE (default) lets R figure out the best arrangement
    ##                  = FALSE uses plot arrangement set up prior to function call
    ##        label     = 'symbol' uses holding symbols on risk/return and beta/alpha plots
    ##                  = 'simple' collapses all holdings to "holding"
    ##        main      = title for plots

    ## 
    
    ## create plots of: risk/reward for portfolio, holdings, and benchmark
    ##                  performance history by year (bar chart?)
    ##                  alpha/beta for portfolio, holdings, and benchmark
    ##                  p/e ratio vs reward?
    
    ## get equity history
    twri.cpi <- NULL
    if (is.null(twri)) {
        twri_provided <- FALSE
        twri  <- equity.twri(holding, period=period)
    } else {
        ## twri provided but may need to strip out the holding columns
        twri_provided <- TRUE
        twri_orig <- twri
        twri <- twri[, (colnames(twri) %in% tidyselect::all_of(holding))]
        if (length(names(twri)) != length(holding)) {
            ## all requested holdings were not in input twri so grab them
            twri  <- equity.twri(holding, period=period)
        }
    }
    
    if (class(twrib)[1] != 'xts') {
        ## symbol provided rather than XTS object
        twrib <- equity.twri(twrib  , period=period)        
    } else {
        ## xts object provided for twrib
        twrib_provided <- TRUE
        if (!identical(zoo::index(twri), zoo::index(twrib))) {
            cat('\n############################################################\n')
            cat('  # Error: Provided twri and twrib do not have the same dates\n')
            cat('  ############################################################\n')
            return()
        }
    }

    ## The following would be incorrect for intermediate missing dates
    ## but that should not genearlly be a problem. Not a problem for
    ## missing dates all at the beginning.
    if (na == 'omit') {
        ## restrict twri for all holdings to dates where all exist
        twri <- na.omit(twri)
    } else {
        ## change NA to 0  
        twri[is.na(twri)] <- 0
    }

    ## check that twri and twrib have the same dates; extract new twrib if needed
    ## note the correction only works if twrib can be found on Yahoo
    twri.dates  <- zoo::index(twri)
    twrib.dates <- zoo::index(twrib)
    ## find twri in twrib
    first.index <- which(grepl(twri.dates[1], twrib.dates)) - 1
    first       <- twrib.dates[first.index]
    ## use date from next earlier twri as the adjusted date for equity.twri
    adjdates    <- c(first, twri.dates)
    if (!identical(twri.dates, twrib.dates)) {
        ## redo twrib to match the twri index
        twrib <- equity.twri(names(twrib), adjdates = adjdates)
    }
    
    ## create portfolio twri
    ##        weight    = vector of weights to use for rebalancing
    ##        rebalance = 'period' (default) to rebalance every period to same weight
    ##                  = 'years' to rebalance at end of each year
    ##                  = 'no' to let assets grow without rebalancing
    ##        value     = xts object with values for each holding for each period
    if (class(value)[1] == 'xts') {
        ## determine twri for portfolio from provided matrix of values
        value       <- value[,names(value) %in% holding]   # values only for holdings
        ## value       <- value[xtsrange]                          # work only with values in range
       if (!identical(zoo::index(value), zoo::index(twri))) {
            ## problem with input value and twri not being consistent
            cat('\n\n#######################################################\n')
            cat(    '#    WARNING: INPUT VALUE AND TWRI NOT CONSISTENT      \n')
            cat(    '#######################################################\n\n')
            return()
        }
        value0      <- value / (twri + 1)                       # values if back out holding twri
        value.port  <- apply(value , 1, sum)                    # sum rows
        value0.port <- apply(value0, 1, sum)                    # sum rows
        twri.port   <- value.port / value0.port - 1
        weight      <- as.numeric( value[nrow(value),] / sum(value[nrow(value),]) )
    } else if (class(weight) == 'numeric') {
        if (rebalance == 'no') {
            ## let assets grow without rebalancing (i.e., determine new weight after every period)
            ## need to determine weight as a function of time
            weight0 <- weight
            value0.port <- 1   # initialize value of portfolio at time t-1
            twri.port   <- NA  # initialize vector for portfolio twri
            for (i in 1:nrow(twri)) {
                value.hold   <- (twri[1,] + 1) * weight * value0.port     # element-wise multiplication
                value.port   <- sum(value.hold)
                twri.port[i] <- value.port / value0.port
                weight       <- value.hold / value.port
                value0.port  <- value.port
            }
        } else if (rebalance == 'years') {
            ## rebalance at the end of every calendar year (dlh future option)
            cat('\nError: Option "rebalance = \'years\'" not programmed yet\n')
            return()
        } else {
            ## rebalance = 'period' (default)
            ## hold weight constant throughout the evaluation (i.e., rebalance every period)
            twri.port <- twri %*% as.matrix(weight)  # %*% specifies matrix multiplication
        }
    } else {
        cat('\nError: input "value" is not an xts object or input "weight" is not a numeric vector\n')
        return()
    }        
    
    ## turn back into xts
    portfolio <- xts::as.xts( zoo::as.zoo( as.matrix(twri.port), zoo::index(twri)))
    
    ## combine into single xts object and call the benchmark "benchmark"
    colnames(twrib) <- 'benchmark'
    twriall <- cbind(twri, portfolio, twrib)
    
    ## restrict to duration
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
    twriall <- twriall[xtsrange]

    if (is.null(efdata)) {
        ## generate efficient frontier module data for same dates as twriall
        symbol      <- c('SPY', 'IWM', 'EFA', 'AGG', 'SHV')
        efdata      <- list()  # declares efdata as a list
        efdata$twri <- equity.twri(symbol, adjdates = adjdates)
        efdata$twri <- efdata$twri[xtsrange]
    }
        
    ## establish other parameters in efdata using the defined efdata$twri
    efdata <- ef(model='Schwab', efdata=efdata, addline=FALSE, col='black', lty=1, pch=3)
    efdata.Schwab <- efdata
    efdata.simple <- NA

    ## extract names
    holdings <- names(twriall)

    ## cumulative twr and standard deviation
    ## 1st date should have twrcum = 0
    twrcum  <- xts::as.xts( t(t(cumprod(twriall+1)) / as.vector(twriall[1,]+1) - 1) )
    twrcuml <- t( xts::last(twrcum) )
    std     <- as.matrix( apply(twriall[2:nrow(twriall),], 2, sd, na.rm=TRUE) )
    
    if (isTRUE(arrange)) {
        ## reserve plotspace if plottype > 1
        if (length(plottype) == 2) {
            plotspace(1,2)
        } else if (length(plottype) == 3) {
            plotspace(1,3)
        } else if (length(plottype) >= 4) {
            plotspace(2,2)
        }
    }

    
    ##-----------------------------------------------------------------------------
    if (which(grepl('CPI', names(twri_orig))) > 0) {
        ## twri for CPI (consumer price index) provided for plotting
        twri.cpi <- twri_orig$CPI[xtsrange]
        twrc.cpi <- twrc.calc(twri.cpi, zero.from=TRUE)
        if (period == 'days') {
            divisor <- 52*5
        } else if (period == 'weeks') {
            divisor <- 52
        } else if (period == 'months') {
            divisor <- 12
        } else if (period == 'years') {
            divisor <- 1
        }
        twri.cpip5 <- twri.cpi + 0.05 / divisor
        twrc.cpip5 <- twrc.calc(twri.cpip5, zero.from=TRUE)
    }

    
    ##-----------------------------------------------------------------------------
    ## plot cumulative TWR
    if (sum(grepl('twrc', plottype) >= 1)) {
        ## plot( plotxts(twrcum, main=main) )
        xts <- twrcum
        pp <- xts::plot.xts(xts[,1:(ncol(xts)-2)], ylab='Cumulative TWR', main=main,
                            ylim=range(xts))
        pp <- xts::addSeries(xts$portfolio, on=1, col='red'    , lwd=2, lty=2)
        pp <- xts::addSeries(xts$benchmark, on=1, col='black'  , lwd=2, lty=2)
        legend.names <- names(xts)
        if (which(grepl('CPI', names(twri_orig))) > 0) {
            pp <- xts::addSeries(twrc.cpip5, on=1, col='cyan', lwd=2, lty=3)
            legend.names <- c(names(xts), 'CPI+5%')
        }
        pp <- xts::addLegend("topleft",
                             legend.names = legend.names, 
                             lty=c(rep(1, ncol(xts)-2), 2, 2, 3),
                             col=c(    1:(ncol(xts)-2), 'red', 'black', 'cyan'))
        plot(pp)
    }
    
    ##-----------------------------------------------------------------------------
    ## create dataframe of holdings, 'portfolio', and 'benchmark'
    out       <- data.frame(colnames(twriall))
    colnames(out) <- 'Holding'
    if (label == 'symbol') {
        ## use holding symbols on risk/return and beta/alpha plots
        out$label <- out$Holding
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
        out <- alpha.beta(as.numeric(twriall[2:(nrow(twriall)-1),i]),
                          benchi[2:(nrow(twriall)-1)],
                          plot=FALSE)
        perf[i,]$alpha <- out$alpha
        perf[i,]$beta  <- out$beta
    }


    ##-----------------------------------------------------------------------------
    ## add p/e ratio and other stats to perf
    if (isFALSE(twri_provided)) {
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
    } else {
        ## do not look for holdings (they may not exist if twri was passed in)
    }

    
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
        efdata.Schwab <- ef(model='Schwab', efdata=efdata, addline=TRUE, col='black', lty=1, pch=3)
        efdata.simple <- ef(model='simple', efdata=efdata, addline=TRUE, col='black', lty=2, pch=4)
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
    if (sum(grepl('twri', plottype) >= 1)) {
        ## plot( plotxts(twriall, main=main) )
        xts <- twriall
        pp <- xts::plot.xts(xts[,1:(ncol(xts)-2)], ylab='Incremental TWR', main=main,
                            ylim=range(xts))
        pp <- xts::addSeries(xts$portfolio, on=1, col='red'    , lwd=2, lty=2)
        pp <- xts::addSeries(xts$benchmark, on=1, col='black'  , lwd=2, lty=2)
        legend.names <- names(xts)
        if (which(grepl('CPI', names(twri_orig))) > 0) {
            pp <- xts::addSeries(twri.cpip5, on=1, col='cyan', lwd=2, lty=3)
            legend.names <- c(names(xts), 'CPI+5%')
        }
        pp <- xts::addLegend("topleft",
                             legend.names = legend.names, 
                             lty=c(rep(1, ncol(xts)-2), 2, 2, 3),
                             col=c(    1:(ncol(xts)-2), 'red', 'black', 'cyan'))
        ## pp <- xts::addLegend("topleft",
        ##                      legend.names = names(xts), 
        ##                      lty=c(rep(1, ncol(xts)-2), 2, 2),
        ##                      col=c(    1:(ncol(xts)-2), 'red', 'black'))
        plot(pp)
    }
    
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
        ## pairplot <- select(perf, twrcum, std, alpha, beta, 'P/E Ratio', 'Price/Book', weight)
        ## pairsdf(pairplot)
        pairsdf(as.data.frame(twriall))
    }
    
    return(list(twri = twriall, performance=perf,
                efdata.Schwab=efdata.Schwab,
                efdata.simple=efdata.simple))
}

