backtest <- function(holding, weight='equal', bench='SPY', xtsrange=5, period='months',
                     twri, twrib, twri.ef,
                     plottype=c('twri', 'ab', 'twrc', 'rr', 'portfolio'),
                     standard.layout=TRUE, main=NULL) {

    ## simple input:
    ##     holding = vector of holdings        (e.g., c('SPY', 'IWM', 'EFA', 'AGG', 'SHV'))
    ##               if not provided, assume holding = names(twri)
    ##     weight  = vector of holding weights (e.g., c( 0.4,   0.2,   0.1,   0.3,   0.1))
    ##             = 'equal' sets holding weights to 1/(# of holdings)
    ##     bench   = benchmark                 (e.g., default is 'SPY')
    ##               if not provided, assume bench = names(twrib)[1] if provided
    ##     period  = 'days' (default), 'weeks', 'months', 'years'
    ##     xtsrange = requested date range to evaluate twri over (e.g., '2020-12-31/2021')
    ##              = 5 (default) for last 5 years
    ##              = # for last # years
    ##              = 0 for YTD

    ## alternate input options:
    ##     twri     = xts object with twri for holdings
    ##                if holding is also provided, will pull twri for holdings from twri
    ##                if not provided, will be obtained
    ##     twrib    = xts object with twri for benchmark 
    ##                if bench is also provided, will pull twri for bench from twrib
    ##                if not provided, will be obtained
    ##     twri.ef  = xts object with twri for effective frontier symbols
    ##                if not provided, will be obtained
    ##     main     = title for some of the plots (default=NULL)
    ##     plottype = default plots everything
    ##              = twri  = incremental TWR
    ##              = twrc  = cumulative TWR
    ##              = ab    = alpha vs. beta
    ##              = rr    = risk/return (TWR vs. standard deviation)
    ##                        actual TWR if < 1 yr; cumulative TWR if > 1 yr
    ##              = portfolio = portfolio plots
    ##     standard.layout = TRUE (default) uses 2x2 plot space for i, c, ab, and rr
    ##                     = FALSE uses existing plot setup
    
    ## if not input, get twri for holdings, benchmark, and efficient frontier
    if (missing(holding)) holding <- names(twri)
    if (missing(twri))    twri  <- equity.twri(holding, refresh=TRUE, file=NA, period=period)
    if (missing(twrib))   twrib <- equity.twri(bench  , refresh=TRUE, file=NA, period=period)
    if (missing(twri.ef)) twri.ef <- ef(period=period, addline=FALSE)$twri

    ## check twri dates within xtsrange and covert everything to that if they do not match
    if (xtsrange == 0) {
        ## set to YTD
        start <- format(Sys.Date(), "%Y")
        end   <- Sys.Date()
        xtsrange <- paste(start, '/', end, sep='')
    } else if (is.number(xtsrange)) {
        ## set to requested number of years
        start <- Sys.Date() - 365.25 * xtsrange
        end   <- Sys.Date()
        xtsrange <- paste(start, '/', end, sep='')
    }        
    twri <- twri[xtsrange]
    dates <- zoo::index(twri)
    twrib   <- twri.adjust(twrib  , d2m=FALSE, dates.new=dates, twri.input=TRUE)
    twri.ef <- twri.adjust(twri.ef, d2m=FALSE, dates.new=dates, twri.input=TRUE)

    ## check for NA in twri, twrib, and twri.ef
    twri_all <- cbind(twri, twrib, twri.ef)
    twri_all <- na.omit(twri_all)
    dates.all <- zoo::index(twri_all)
    mismatch <- grep('FALSE', dates == dates.all)
    if (length(mismatch) > 1) {
        cat('\nFatal error: Select new date range. Some holdings have twri=NA.\n')
        return(twri_all)
    }
    
    ## calculate portfolio performance
    if (weight == 'equal') {
        nhold <- length(holding)
        weight <- rep(1/nhold, nhold)
    }
    port <- portfolio.calc(twri[xtsrange], weight=weight, twrib=twrib[xtsrange])

    ## plot portfolio performance
    if (isTRUE(standard.layout)) plotspace(2,2)
    duration <- as.numeric(diff.Date(range(dates)))
    if (!is.null(main)) main <- paste(main, '; ', sep='')
    if (grepl('twri', plottype) | grepl('ab', plottype) |
        grepl('twrc', plottype) | grepl('rr', plottype)) {
        ## at least one plot for individual holdings is requestedf
        if (duration > 365.25) {
            ## print annualized risk/return plot
            ## if requested plottype includes rr (risk/return), replace with rra (annualized risk/return)
            plottype <- str_replace(plottype, 'rr', 'rra')
            out <- portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, 
                                  twri.ef=twri.ef[xtsrange],
                                  plottype=plottype, pch.hold = 16,
                                  main=paste(main, 'Benchmark = ', names(twrib)[1], sep=''))
        } else {
            ## print non-annualized risk/return plot
            out <- portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, 
                                  twri.ef=twri.ef[xtsrange],
                                  plottype=plottype, pch.hold = 16,
                                  main=paste(main, 'Benchmark = ', names(twrib)[1], sep=''))
        }        
    } else {
        ## no individual holding plots are requested
        out <- NA
    }
    
    if (grepl('portfolio', plottype)) {
        ## evaluate portfolio as if it were a mutural fund
        out.eq <- equity.eval(twri = port$twri$portfolio, twrib = twrib, period=period)
        ##                    bench='schwab_80_20', twrib = out$efdata.Schwab$eftwri$schwab_80_20)
    }
    
    if (is.na(out)) {
        return(list(port=port, efdata.Schwab=NA, efdata.simple=NA))
    } else {        
        return(list(port=port, efdata.Schwab=out$efdata.Schwab, efdata.simple=out$efdata.simple))
    }
}
## xtsrange1 <- '2020-12-31/2021-12'
## xtsrange3 <- '2018-12-31/2021-12'
## xtsrange5 <- '2016-12-31/2021-12'

## ## holding <- c('SPY', 'IWM', 'EFA', 'AGG', 'SHV')
## holding <- c('SWPPX', 'PONAX')
## weight  <- c( 0.6,   0.4)
## bench   <- 'SPY'
## period  <- 'days'
## xtsrange <- xtsrange5
## out <- backtest(holding=holding, weight=weight, bench=bench, 
##                 period=period, xtsrange=xtsrange)

## ## following not working right for some reason
## twri  <- equity.twri(holding, refresh=TRUE, file=NA, period=period)
## twrib <- equity.twri(bench  , refresh=TRUE, file=NA, period=period)
## twri.ef <- ef(period=period, addline=FALSE)$twri
## out <- backtest(twri=twri, weight=weight, twrib=twrib, twri.ef=twri.ef, xtsrange)
