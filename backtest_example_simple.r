##-----------------------------------------------------------------------------
## setup
os <- .Platform$OS.type
if (os == 'windows') {
    ## load generic modules
    source("F:\\Documents\\01_Dave's Stuff\\Programs\\GitHub_home\\R-setup\\setup.r")
    ## identify working folder
    path <- c("f:/Documents/01_Dave's Stuff/Programs/GitHub_home/Finance/")
} else {
    ## os == unix
    source('~/GitHub_repos/R-setup/setup.r')
    path <- c('~/GitHub_repos/Finance/')
}
## set working folder
setwd(path)
## load local modules
r_files <- list.files(paste(path, 'modules/', sep=''), pattern="*.[rR]$", full.names=TRUE)
for (f in r_files) {
    ## cat("f =",f,"\n")
    source(f)
}

##-----------------------------------------------------------------------------
## simplest application
out <- backtest(holding=c('VNQ', 'RQI'))
out <- backtest(holding=c('VNQ', 'RQI'), weight='equal', bench='SPY', xtsrange=0, period='months')


##-----------------------------------------------------------------------------
## a little more efficient application if looking to run multiple times with different holdings
## first get benchmarch and efficient frontier TWR objects
twrib <- equity.twri('SPY'  , refresh=TRUE, file=NA, period='days')
twrib <- equity.twri('AGG'  , refresh=TRUE, file=NA, period='days')
twri.ef <- ef(period='days', addline=FALSE)$twri


##-----------------------------------------------------------------------------
## now look at performance of holdings relative to above
out <- backtest(holding=c('VNQ', 'RQI'), twrib=twrib, twri.ef=twri.ef)
out <- backtest(holding=c('VNQ', 'RQI'), twrib=twrib, twri.ef=twri.ef, period='days')
out <- backtest(holding=c('VNQ', 'NRZ',           # REITs
                          'AMT', 'SBAC', 'CCI'),  # tower companies
                twrib=twrib, 
                twri.ef=twri.ef, 
                xtsrange=3, 
                plottype=c('twri', 'ab', 'twrc', 'rr'),
                main='REIT')
## sort output by TWRC
out$port$perf[order(out$port$perf$twrc, decreasing=TRUE),]

out <- backtest(holding=c('CVSIX',    # market neutral
                          'CTFAX',    # global macro
                          'SMRSX',    # short bond
                          'MWFSX',    # global bond
                          'GIOAX'),   # credit?
                twrib=twrib, twri.ef=twri.ef, xtsrange=0, 
                plottype=c('twri', 'ab', 'twrc', 'rr'),
                main='Alt + Fixed')
out$port$perf[order(out$port$perf$twrc, decreasing=TRUE),]

## group 1
holding.large <- c('AKREX', 'DSENX', 'JACTX', 'VYM')
holding.small <- c('CDW', 'MCHP', 'UMBMX')   # ZM
holding.inter <- c('NWFFX', 'RYIPX')
holding.alt   <- c('BRIFX', 'SBAC', 'DBC', 'AMT')
holding.alt   <- c('SBAC', 'DBC', 'AMT', 'IAU')
holding.fixed <- c('AGG', 'LSBDX', 'PONAX')
## group 2
holding.large <- c('AKREX', 'EGFIX', 'FSRPX', 'ROGSX', 'SPLV', 'TRBCX', 'TWEIX', 'SWKS')
holding.small <- c('FAMEX', 'JSCVX', 'UMBMX')
holding.inter <- c('NWFFX', 'RYIPX')
holding.alt   <- c('BRIFX', 'SBAC', 'DBC', 'AMT')
holding.alt   <- c('SBAC', 'DBC', 'AMT', 'IAU')
holding.fixed <- c('AGG', 'LSBDX', 'PONAX')
## type
holding.large.g <- c('AKREX', 'JACTX', 'EGFIX', 'FSRPX', 'ROGSX', 'TRBCX')
holding.large.b <- c('DSENX', 'SPY')
holding.large.v <- c('VYM', 'SPLV', 'TWEIX')
holding.small.g <- c()
holding.small.b <- c('FAMEX', 'UMBMX')
holding.small.v <- c('JSCVX')
holding.fixed   <- c('AGG', 'FRFZX', 'JPST', 'LALDX', 'PONAX', 'TIYRX')
holding.stock <- c('NVDA', 'OKTA', 'SBAC', 'AMT', 'CDW', 'SPLV')
holding.reit <- c('SBAC', 'AMT', 'VNQ')
out <- backtest(holding=holding.fixed,
                twrib=twrib,
                ##    1     twrib='AGG',
                twri.ef=twri.ef,
                ## xtsrange=0, period='days',
                xtsrange=20,
                ## xtsrange='2020-01/2020-03', period='days',
                plottype=c('twri', 'ab', 'twrc', 'rr'),
                main='fixed')
out$port$perf[order(out$port$perf$twrc, decreasing=TRUE),]

out.ef <- out$efdata.simple$ef
shinyplot(as.data.frame(out$port$perf), 'std.ann' , 'twrc.ann', xline=out.ef$efstd, yline=out.ef$eftwrc)
shinyplot(as.data.frame(out$port$perf), 'beta', 'alpha')

##-----------------------------------------------------------------------------
## identify holdings and weights that comprise portfolio
holding       <- c('SPY', 'IWM', 'EFA', 'AGG', 'SHV')
weight        <- rep(1/length(holding), length(holding))
port1 <- data.frame(holding=holding, weight=weight)
class       port1$port <- 'Portfolio 1'
US          
Inter       ## another way to enter the portfolio holdings
Alt         ## only need holding and weight vectors to be defined
Alt         port <- '
Alt         class       holding   weight     # comments
Bond_US     US_L_G      APGAX      8
Bond_Inter  US_L_G      LCGFX      8         # $50 fee to buy
Cash        US_L_V      SCHD      16
US_M_G      BMDSX      7
US_M_V      JNVSX      7
Global      GWPAX      9
Inter       SPDW      10
Emerg       IEMG       5
Alt         CVSIX      3
Alt         CTFAX      2
Bond_short  SMRSX     20
Bond_global MWFSX      5
'
port.ms  <- readall(port)
port.ms$weight <- port.ms$weight / 100
port.ms$port <- 'MorganStanley'

## personal capital balanced
port <- '
class       holding   weight
US          VTI       48.7
Inter       VEU       21
Alt         VNQ        3.4
Alt         IAU        3.3
Alt         DBC        3.3
Bond_US     AGG       16.6
Bond_Inter  IGOV       2.9
Cash        Cash       0.8
'
port.pc <- readall(port)
port.pc$weight <- port.pc$weight / 100
sum(port.pc$weight)
port.pc$port <- 'PersonalCapital - Balanced'

## choose one of the above sample portfolios
portfolio     <- port3
portfolioname <- port3$name
holding  <- as.character( portfolio$holding )
weight   <- portfolio$weight
if (sum(weight) == 0) cat('WARNING: Portfolio holding weight sum =', sum(weight), 'but should sum to 1\n')

## define a benchmark to use in the evaluation
bench <- 'SPY'

##-----------------------------------------------------------------------------
## define a timeframe and period for incremental TWR
period   <- 'months'
xtsrange1 <- '2021-12-31/2022-12'
xtsrange3 <- '2019-12-31/2022-12'
xtsrange5 <- '2017-12-31/2022-12'

##-----------------------------------------------------------------------------
## get twri for holdings, benchmark, and efficient frontier
twri  <- equity.twri(holding, refresh=TRUE, file=NA, period=period)
twrib <- equity.twri(bench  , refresh=TRUE, file=NA, period=period)
twri.ef <- ef(period=period, addline=FALSE)$twri

## calculate and plot performance
backtest.here <- function(twri, weight, twrib, twri.ef, xtsrange) {
    port <- portfolio.calc(twri[xtsrange], weight=weight, twrib=twrib[xtsrange])
    plotspace(2,2)
    out <- portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, 
                          twri.ef=twri.ef[xtsrange],
                          plottype=c('twri', 'ab', 'twrc', 'rra'), pch.hold = 16,
                          main=paste('Benchmark = ', names(twrib)[1], sep=''))
    return(list(port=port, efdata.Schwab=out$efdata.Schwab, efdata.simple=out$efdata.simple))
}
out <- backtest.here(twri, weight, twrib, twri.ef, xtsrange1)
out <- backtest(holding, weight, bench, xtsrange1, period=period,
                twri, twrib, twri.ef)
port <- out$port

## without pre-calculating twri, twrib, twri.ef
out <- backtest(holding, weight, bench, xtsrange5, period=period)
port <- out$port

## efficient frontier line is set based on last call to risk/reward plot in portfolio.plot ('rr' or 'rra')
## need to make sure requested shiny x,y are consistent with efficient frontier
## so efficient fontier line is right on shinyplot:
##     need to make sure plot std     and twrc     if portfolio.plot used 'rr'
##     need to make sure plot std.ann and twrc.ann if portfolio.plot used 'rra'
out.ef <- out$efdata.simple$ef
shinyplot(as.data.frame(out$port$perf), 'std.ann' , 'twrc.ann', xline=out.ef$efstd, yline=out.ef$eftwrc)
shinyplot(as.data.frame(out$port$perf), 'beta', 'alpha')

## look at correlation between investments
pairsdf(as.data.frame(port$twri))
plotspace(1,1)
GGally::ggpairs(as.data.frame(port$twri))

##-----------------------------------------------------------------------------
## how about this last year?
## Note: making use of prior xts object for twri
##       this avoids again looking up the holding performance
##       it only works here because the duration is the same or shorter than what is twri
xtsrange <- '2020-10-31/2021-11-30'
port <- portfolio.calc(twri[xtsrange], weight=weight, twrib=twrib[xtsrange])
plotspace(2,2)
out <- portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, 
                      twri.ef=twri.ef[xtsrange],
                      plottype=c('twri', 'ab', 'twrc', 'rra'), pch.hold = 16,
                      main=paste('Benchmark = ', names(twrib)[1], sep=''))
pairsdf(as.data.frame(port$twri))

##-----------------------------------------------------------------------------
## DEEP DIVE INTO SPECIFIC HOLDING
out <- equity.eval('BKI', 'SPY', duration='3 years')
out <- equity.eval('IFED', 'SPY', period='days', duration='3 years')
out
