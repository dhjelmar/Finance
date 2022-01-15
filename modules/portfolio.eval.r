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
    ## lines(efdata$ef$efstd, efdata$ef$eftwrc, type='b', col='red', lty=3, pch=1)
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
    ##        rebalance = 'no' to let assets grow without rebalancing
    ##                  = 'period' (default) to rebalance every period to same weight
    ##                    (note: this is what ef.r currently assumes)
    ##                  = 'years' to rebalance at end of each year
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
    ##                  = 'rr' for risk/reward (terc vs. sd)
    ##                    or 'rra' for annualized risk/rewrad (ann. twrc vs. sd * sqrt(12)) 
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
    
    ## if (twrib == 'SPY') browser()
    
    ## understand requested xts range of dates
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
    
    ## get equity history
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
        twrib <- twrib[xtsrange]
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
        ## attempt to redo twrib to match the twri index
        ## note this will fail if twri symbol is not a real symbol
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
        ## value over time is not defined, so use initial weight instead
        
        if (rebalance == 'no') {
            ## let assets grow without rebalancing (i.e., determine new weight after every period)
            ## need to determine weight as a function of time
            ## first restrict twri to xtsrange
            twri <- twri[xtsrange]
            ## create empty xts object for value
            ## value.port <- 1
            value      <- xts.create(datevec=zoo::index(twri), value=NA, names=names(twri))
            value.port <- xts.create(datevec=zoo::index(twri), value=NA, names='portfolio')
            value.port[1] <- 1
            value[1,]  <- weight * value.port[[1]]
            twri.port  <- xts.create(datevec=zoo::index(twri), value=0, names='portfolio')
            for (i in 2:nrow(twri)) {
                value[i,]     <- (twri[i,] + 1) * weight * value.port[[i-1]]  # new value for each holding
                value.port[i] <- sum(value[i,])                               # portfolio value
                twri.port[i]  <- value.port[[i]] / value.port[[i-1]] - 1      # portfolio twri
                weight        <- as.vector( value[i,] / value.port[[i]] )     # new weights
            }                

        } else if (rebalance == 'years') {
            ## rebalance at the end of every calendar year (dlh future option)
            cat('\nError: Option "rebalance = \'years\'" not programmed yet\n')
            return()
            
        } else {
            ## rebalance = 'period' (default)
            ## rebalance every period (i.e., hold weight constant throughout the evaluation)
            twri.port <- twri %*% as.matrix(weight)  # %*% specifies matrix multiplication
        }
        
    } else {
        cat('\nError: input "value" is not an xts object or input "weight" is not a numeric vector\n')
        return()
    }        
    
    ## turn back into xts
    portfolio <- xts::as.xts( zoo::as.zoo( as.matrix(twri.port), zoo::index(twri)))
    colnames(portfolio) <- 'portfolio'
    
    ## combine into single xts object and call the benchmark "benchmark"
    ## colnames(twrib) <- 'benchmark'
    twriall <- cbind(twri, portfolio, twrib)
    colnames(twriall) <- c(colnames(twri), colnames(portfolio), colnames(twrib))
    twri.col        <- ncol(twri)
    twrib.col       <- ncol(twrib)
    twrib.col.start <- twri.col + 1 + 1
    twrib.col.end   <- twri.col + 1 + twrib.col
    
    ## restrict to duration
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
    ## 1st date should have twrc = 0
    twrc  <- xts::as.xts( t(t(cumprod(twriall+1)) / as.vector(twriall[1,]+1) - 1) )
    twrcl <- t( xts::last(twrc) )
    std   <- as.matrix( apply(twriall[2:nrow(twriall),], 2, sd, na.rm=TRUE) )

    ## calculate risk from standard deviation
    ##    std(xi) = sqrt ( sum(xi-xbar)^2 / N ) for population which seems to be what finance world uses
    ## If have x = monthly TWR and want std for y = yearly TWR
    ## then set F * std(x) = std(y) and solve for F.
    ## If assume yi = 12*xi and give credit for the fact that 1 yearly entry is from 12 measurments, 
    ## then can use Ny = 12*Nm and F = sqrt(12).
    ## std( TWR_monthly - avg_TWR_monthly )
    std.ann  <- std * 12^0.5  

    ## average annual return
    days.held <- as.numeric(as.Date(to) - as.Date(from))
    twrc.ann  <- (1 + twrcl)^(365.25 / days.held) - 1

    
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
    ## plot cumulative TWR
    if (sum(grepl('twrc', plottype) >= 1)) {
        ## plot( plotxts(twrc, main=main) )
        xts <- twrc
        pp <- xts::plot.xts(xts[, 1:twri.col], ylab='Cumulative TWR', main=main,
                            ylim=range(xts, na.rm=TRUE))
        pp <- xts::addSeries(xts$portfolio,
                             on=1, col='red'    , lwd=2, lty=2)
        pp <- xts::addSeries(xts[, twrib.col.start:twrib.col.end],
                             on=1, col=c('black', 'cyan')  , lwd=2, lty=2)
        legend.names <- names(xts)
        pp <- xts::addLegend("topleft",
                             legend.names = legend.names, 
                             lty=c(rep(1, twri.col), 2, 2, 3),
                             col=c(    1:(1 + twrib.col), 'red', 'black', 'cyan'))
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
    perf  <- cbind(out, twrcl, std, twrc.ann, std.ann)
    names(perf) <- c('Holding', 'label', 'twrc', 'std', 'twrc.ann', 'std.ann')


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

    ## separate into 3 dataframes
    perfhold  <- perf[1:twri.col,]
    perfport  <- perf[perf$Holding == 'portfolio',]
    perfbench <- perf[twrib.col.start,]   # first benchmark only
    
    if (sum(grepl('rr$', plottype) >= 1)) {
        
        ## determine range making room for legend
        xrange <- range(perf$std, efdata$ef$efstd)
        xlim   <- c(min(xrange),
                    max(xrange) + 0.25*(max(xrange) - min(xrange)))
        ylim   <- range(perf$twrc, efdata$ef$eftwrc)

        ## plot holdings
        with(perfhold, plotfit(std, twrc, label, interval='noline',
                               xlimspec=xlim, ylimspec=ylim,
                               xlabel = 'Standard Deviation',
                               ylabel = 'Cumulative TWR',
                               main   = main))
        ## add portfolio
        points(perfport$std, perfport$twrc, col='red', pch=16)
        ## add benchmark
        points(perfbench$std, perfbench$twrc, col='blue', pch=17)
        ## add efficient frontier line
        efdata.Schwab <- ef(model='Schwab', efdata=efdata, annualize=FALSE, addline=TRUE, col='black', lty=1, pch=3)
        efdata.simple <- ef(model='simple', efdata=efdata, annualize=FALSE, addline=TRUE, col='black', lty=2, pch=4)
        mtext('(portfolio = solid red circle; benchmark = solid blue triangle)',
              side=3, line=0.8, cex=1)
        mtext('(Schwab EF = solid line; S&P 500 / AGG EF = dotted line)',
              side=3, line=0, cex=1)

    }
    
    if (sum(grepl('rra', plottype) >= 1)) {
        
        ## determine range making room for legend
        xrange <- range(perf$std.ann, efdata$ef$efstd)
        xlim   <- c(min(xrange),
                    max(xrange) + 0.25*(max(xrange) - min(xrange)))
        ylim   <- range(perf$twrc.ann, efdata$ef$eftwrc)

        ## plot holdings
        with(perfhold, plotfit(std.ann, twrc.ann, label, interval='noline',
                               xlimspec=xlim, ylimspec=ylim,
                               xlabel = 'Standard Deviation * sqrt(12)',
                               ylabel = 'Annualized Cumulative TWR',
                               main   = main))
        ## add portfolio
        points(perfport$std.ann, perfport$twrc.ann, col='red', pch=16)
        ## add benchmark
        points(perfbench$std.ann, perfbench$twrc.ann, col='blue', pch=17)
        ## add efficient frontier line
        efdata.Schwab <- ef(model='Schwab', efdata=efdata, annualize=TRUE, addline=TRUE, col='black', lty=1, pch=3)
        efdata.simple <- ef(model='simple', efdata=efdata, annualize=TRUE, addline=TRUE, col='black', lty=2, pch=4)
        mtext('(portfolio = solid red circle; benchmark = solid blue triangle)',
              side=3, line=0.8, cex=1)
        mtext('(Schwab EF = solid line; S&P 500 / AGG EF = dotted line)',
              side=3, line=0, cex=1)

    }

    
    ##-----------------------------------------------------------------------------
    ## plot incremental TWR
    if (sum(grepl('twri', plottype) >= 1)) {
        ## plot( plotxts(twriall, main=main) )
        xts <- twriall
        pp <- xts::plot.xts(xts[, 1:twri.col], ylab='Incremental TWR', main=main,
                            ylim=range(xts, na.rm = TRUE))
        pp <- xts::addSeries(xts$portfolio,
                             on=1, col='red'    , lwd=2, lty=2)
        pp <- xts::addSeries(xts[, twrib.col.start:twrib.col.end],
                             on=1, col=c('black', 'cyan')  , lwd=2, lty=2)
        legend.names <- names(xts)
        pp <- xts::addLegend("topleft",
                             legend.names = legend.names, 
                             lty=c(rep(1, twri.col), 2, 2, 3),
                             col=c(    1:(1 + twrib.col), 'red', 'black', 'cyan'))
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
    perf$weight <- c(weight, rep(NA, 1+twrib.col))
    perf$label  <- NULL

    ## any correlation?
    if (sum(grepl('pairs', plottype) >= 1)) {
        ## identify performance info to output and use in correlation plot
        ## pairplot <- select(perf, twrc, std, alpha, beta, 'P/E Ratio', 'Price/Book', weight)
        ## pairsdf(pairplot)
        pairsdf(as.data.frame(twriall))
    }
    
    return(list(twri = twriall, performance=perf,
                efdata.Schwab=efdata.Schwab,
                efdata.simple=efdata.simple))
}

