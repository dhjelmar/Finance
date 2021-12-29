performance.plot <- function(portfolio, valuesheet, twrsheet, twrib, xtsrange, period,
                             portfolioname = NULL, file = NULL) {
    
    
    ## extract value for portfolio holdings
    value          <- valuesheet[,names(valuesheet[xtsrange]) %in% portfolio]
    value.current  <- as.numeric( tail(value, 1) )

    if (!is.null(file)) pdf(file = file, onefile = TRUE,          # creates a multi-page PDF file
                            ## file = "performance%03d.pdf", onefile = FALSE,  # creates multiple PDF files
                            width = 9,  # The width of the plot in inches
                            height = 7) # The height of the plot in inches


    ## create value plot
    plotspace(3,1)
    plot( plotxts(value[xtsrange]) )

    ## extract actual from and to to be used in plots based on applying xtsrange
    xts  <- twrsheet[xtsrange]
    from <- zoo::index(xts[1,])
    to   <- zoo::index(xts[nrow(xts),])
    duration <- paste(from, 'to', to, sep=' ')
    if (is.null(portfolioname)) portfolioname <- 'Portfolio'
    main <- paste(portfolioname, ': ', duration, '; period = ', period, sep='')
    cat('\n', main, '\n\n')

    ## create cumulative and incremental TWR plots
    ## period = days used in the following so there is a unique value for every date read from Excel
    ## all date entries are consisered equally in evaluation of standard deviation, alpha, and beta
    out1 <- portfolio.eval(portfolio, twri=twrsheet, twrib='SPY', value=valuesheet,
                           plottype = c('twrc', 'twri'), arrange=FALSE,
                           from=from, to=to, period=period,
                           main = main)

    ## create risk/return plot
    out2 <- portfolio.eval(portfolio, twri=twrsheet, twrib='SPY', value=valuesheet,
                           plottype = c('rr', 'ab'),
                           from=from, to=to, period=period,
                           main = main)
    ## above can be made more efficient to not look everything up again
    ## following not working yet though
    ## twri  <- out1$twri
    ## twrib <- out1$twri$benchmark
    ## out2 <- portfolio.eval(portfolio, twri=twri, twrib=twrib, value=valuesheet,
    ##                        plottype = c('rr', 'ab'),
    ##                        from=from, to=to, period=period,
    ##                        main = main)
    out2$performance$value <- c(value.current, sum(value.current), NA)
    
    ## evaluate portfolio as if it was a mutual fund
    twri <- twrsheet[,names(twrsheet) %in% portfolio]
    ## out <- equity.eval(portfolioname, 'SPY', twri=out$twri$portfolio, period=period)
    out3 <- equity.eval(portfolioname, 'SPY', twri=twri, period=period)


    if (!is.null(file)) dev.off() # close external pdf (or jpg) file

    return(list(out.xtsrange=out2, out.125yrs=out3))

}
