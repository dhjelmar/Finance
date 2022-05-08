portfolio.plot <- function(twri=NA, twrc=NA, perf=NA, twri.ef=NA,
                           plottype=c('twri', 'twrc', 'rra', 'ab'), pch.hold=21,
                           main=NULL, label='symbol') {

    ## input: twri = incremental TWR (Time Weighted Return)
    ##               xts object with holdings, portfolio, and benchmarks listed in that order
    ##               'portfolio' is used to identify the breakpoint
    ##               portfolio.calc() creates output in this format
    ##               easy to create otherwise with cbind()
    ##        twrc = cumulative TWR xts object (same format as twri)
    ##        twri.ef = xts object with twri for SPY, IWM, EFA, AGG, SHV
    ##                  for use in adding efficient frontier line to risk/reward plot
    ##                  can get by running ef(addline=FALSE)
    ##                  make sure dates correspond to those for twri
    ##        perf = dataframe with rows in same order as twri columns for entire period
    ##               perf and columns only required if request rr or ab plots
    ##               columns define: holding
    ##                               twrc = ending twrc (should match twrc)
    ##                               std  = standard deviation
    ##                               twrc.ann = annualized TWRC if duration > 1 year
    ##                               std.ann  = annualized standard deviation (std*sqrt(12))
    ##                               alpha
    ##                               beta
    ##        label     = 'symbol' uses holding symbols on risk/return and beta/alpha plots
    ##                  = 'simple' collapses all holdings to "holding"
    
    ## identify number of holdings and benchmarks
    port.col <- grep('portfolio', names(twri))  # get column number for portfolio
    nhold  <- port.col - 1
    nbench <- ncol(twri) - port.col

    ## initialize output parameters
    efdata.Schwab = NA
    efdata.simple = NA
        
    for (requested in plottype) {
        
        ##-----------------------------------------------------------------------------
        ## plot incremental TWR
        if (requested == 'twri') {
            ## plot( plotxts(twri, main=main) )
            xts <- twri
            pp <- xts::plot.xts(xts[, 1:nhold], ylab='Incremental TWR', main="",
                                col=c(1:nhold),
                                ylim=range(xts, na.rm = TRUE))
            pp <- xts::addSeries(xts$portfolio,
                                 on=1, col='red'    , lwd=2, lty=2)
            pp <- xts::addSeries(xts[, (port.col+1):ncol(twri)],
                                 on=1, col=c('black', 'cyan')  , lwd=2, lty=2)
            legend.names <- names(xts)
            pp <- xts::addLegend("topleft",
                                 legend.names = legend.names, 
                                 lty=c(rep(1, nhold), 2, 2, 3),
                                 col=c(    1: nhold, 'red', 'black', 'cyan'))
            plot(pp)
        }

        ##-----------------------------------------------------------------------------
        ## plot cumulative TWR
        if (requested == 'twrc') {
            ## plot( plotxts(twrc, main=main) )
            xts <- twrc
            pp <- xts::plot.xts(xts[, 1:nhold], ylab='Cumulative TWR', main="",
                                col=c(1:nhold),
                                ylim=range(xts, na.rm=TRUE))
            pp <- xts::addSeries(xts$portfolio,
                                 on=1, col='red'    , lwd=2, lty=2)
            pp <- xts::addSeries(xts[, (port.col+1):ncol(twri)],
                                 on=1, col=c('black', 'cyan')  , lwd=2, lty=2)
            legend.names <- names(xts)
            pp <- xts::addLegend("topleft",
                                 legend.names = legend.names, 
                                 lty=c(rep(1, nhold), 2, 2, 3),
                                 col=c(    1: nhold, 'red', 'black', 'cyan'))
            plot(pp)
        }

        ##-----------------------------------------------------------------------------
        ## add label for plot legend to perf
        if (label == 'symbol') {
            ## use holding symbols on risk/return and beta/alpha plots
            perf$label <- perf$holding
        } else {
            ## use labels of 'holding', 'portfolio', or 'benchmark' on plot
            perf$label <- 'holding'
            perf[perf$holding == 'portfolio',]$label <- 'portfolio'
            perf[(port.col+1):nrow(perf),]$label <- 'benchmark'
        }        

        
        ##-----------------------------------------------------------------------------
        ## separate performance table into 3 dataframes for risk/reward and alpha/beta plots
        perfhold  <- perf[1:nhold,]
        perfport  <- perf[perf$holding == 'portfolio',]
        perfbench <- perf[(port.col+1),]
        perfbench1 <- perfbench[1,]        # first benchmark only
        
        
        ##-----------------------------------------------------------------------------
        ## plot risk/reward

        if (requested == 'rr') {
            ## risk   = standard deviation
            ## reward = twrc

            if (nrow(twri) > 2) {
                ## need at least 3 rows for sd() (1st row is not used)
                
                ## determine range making room for legend
                efdata.schwab <- ef(model='Schwab', efdata=twri.ef, annualize=FALSE, addline=FALSE)
                efdata.simple <- ef(model='simple', efdata=twri.ef, annualize=FALSE, addline=FALSE)
                xrange <- range(perf$std, efdata.schwab$ef$efstd, efdata.simple$ef$efstd)
                xlim   <- c(min(xrange, na.rm = TRUE),
                            max(xrange, na.rm = TRUE) + 
                            0.25*(max(xrange, na.rm = TRUE) - min(xrange, na.rm = TRUE)))
                ylim   <- range(perf$twrc, efdata.schwab$ef$eftwrc, efdata.simple$ef$eftwrc,
                                na.rm = TRUE)

                ## plot holdings
                with(perfhold, plotfit(std, twrc, label, interval='noline',
                                       xlimspec=xlim, ylimspec=ylim, pch=pch.hold,
                                       xlabel = 'Standard Deviation',
                                       ylabel = 'Cumulative TWR',
                                       main   = main))
                ## add portfolio
                points(perfport$std, perfport$twrc, col='red', pch=1)
                ## add benchmark (only the 1st)
                points(perfbench1$std, perfbench1$twrc, col='blue', pch=2)
                ## add efficient frontier line
                efdata.Schwab <- ef(model='Schwab', efdata=twri.ef, annualize=FALSE,
                                    addline=TRUE, col='black', lty=1, pch=3)
                efdata.simple <- ef(model='simple', efdata=twri.ef, annualize=FALSE,
                                    addline=TRUE, col='black', lty=2, pch=4)
                mtext('(portfolio = open red circle; benchmark = open blue triangle)',
                      side=3, line=0.8, cex=1)
                mtext('(Schwab EF = solid line; S&P 500 / AGG EF = dotted line)',
                      side=3, line=0, cex=1)
            } else {
                cat('WARNING: Insufficient number of rows in twri for requested rr plot.\n')
            }
                
        }
        
        if (requested == 'rra') {
            ## risk   = standard deviation * sqrt(12)
            ## reward = annualized twrc

            if (nrow(twri) > 2) {
                ## need at least 3 rows for sd() (1st row is not used)
                
                ## determine range making room for legend
                ## first run ef() to get range of effective frontier plots
                efdata.schwab <- ef(model='Schwab', efdata=twri.ef, annualize=TRUE, addline=FALSE)
                efdata.simple <- ef(model='simple', efdata=twri.ef, annualize=TRUE, addline=FALSE)
                xrange <- range(perf$std.ann, efdata.schwab$ef$efstd, efdata.simple$ef$efstd)
                xlim   <- c(min(xrange, na.rm = TRUE),
                            max(xrange, na.rm = TRUE) + 
                            0.25*(max(xrange, na.rm = TRUE) - min(xrange, na.rm = TRUE)))
                ylim   <- range(perf$twrc.ann, efdata.schwab$ef$eftwrc, efdata.simple$ef$eftwrc,
                                na.rm = TRUE)

                ## plot holdings
                with(perfhold, plotfit(std.ann, twrc.ann, label, interval='noline',
                                       xlimspec=xlim, ylimspec=ylim, pch=pch.hold,
                                       xlabel = 'Standard Deviation * sqrt(12)',
                                       ylabel = 'Annualized Cumulative TWR',
                                       main   = main))
                ## add portfolio
                points(perfport$std.ann, perfport$twrc.ann, col='red', pch=1)
                ## add benchmark
                points(perfbench1$std.ann, perfbench1$twrc.ann, col='blue', pch=2)
                ## add efficient frontier line
                efdata.Schwab <- ef(model='Schwab', efdata=twri.ef,
                                    annualize=TRUE, addline=TRUE, col='black', lty=1, pch=3)
                efdata.simple <- ef(model='simple', efdata=twri.ef,
                                    annualize=TRUE, addline=TRUE, col='black', lty=2, pch=4)
                mtext('(portfolio = open red circle; benchmark = open blue triangle)',
                      side=3, line=0.8, cex=1)
                mtext('(Schwab EF = solid line; S&P 500 / AGG EF = dotted line)',
                      side=3, line=0, cex=1)
            } else {
                cat('WARNING: Insufficient number of rows in twri for requested rra plot.\n')
            }

        }

        
        ##-----------------------------------------------------------------------------
        ## alpha/beta plot
        if (requested == 'ab') {
            ## create plot
            xrange <- range(perf[1:(nhold+2),]$beta)
            xlim   <- c(min(xrange, na.rm = TRUE),
                        max(xrange, na.rm = TRUE) + 
                            0.25*(max(xrange, na.rm = TRUE) - min(xrange, na.rm = TRUE)))
            ylim   <- range(perf[1:(nhold+2),]$alpha, na.rm = TRUE)
            with(perfhold, plotfit(beta, alpha, label, interval='noline',
                                   xlimspec=xlim, ylimspec=ylim, pch=pch.hold,
                                   xlabel = 'beta',
                                   ylabel = 'alpha',
                                   main   = main))
            ## add portfolio
            points(perfport$beta, perfport$alpha, col='red', pch=1)
            ## add benchmark
            points(perfbench1$beta, perfbench1$alpha, col='blue', pch=2)
            ## add subtitle text
            mtext('(portfolio = open red circle; benchmark = open blue triangle)',
                  side=3, line=0.75, cex=1)   
            abline(v=1)
            abline(h=0)
        }


        ## any correlation?
        if (requested == 'pairs') {
            ## identify performance info to output and use in correlation plot
            ## pairplot <- select(perf, twrc, std, alpha, beta, 'P/E Ratio', 'Price/Book', weight)
            ## pairsdf(pairplot)
            pairsdf(as.data.frame(twri))
        }
    }

    return(list(perf  = perf,
                efdata.Schwab = efdata.Schwab,
                efdata.simple = efdata.simple))
    
}
