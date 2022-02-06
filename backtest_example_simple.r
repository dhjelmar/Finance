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
## identify holdings and weights that comprise portfolio
portfolioname <- 'Portfolio 1'
holding       <- c('SPY', 'IWM', 'EFA', 'AGG', 'SHV')
weight        <- rep(1/length(holding), length(holding))

## another way to enter the portfolio holdings
portfolioname <- 'MS'
portfolio <- '
class       holding   weight     # comments
US_L_G      APGAX      8
US_L_G      LCGFX      8         # $50 fee to buy
US_L_V      SCHD      16
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
port_in  <- readall(portfolio)
holding  <- as.character( port_in$holding )
weight   <- port_in$weight / 100


## define a benchmark to use in the evaluation
bench <- 'SPY'

##-----------------------------------------------------------------------------
## define a timeframe and period for incremental TWR
period   <- 'months'
xtsrange1 <- '2020-12-31/2021-12'
xtsrange3 <- '2018-12-31/2021-12'
xtsrange5 <- '2016-12-31/2021-12'

##-----------------------------------------------------------------------------
## get twri for holdings, benchmark, and efficient frontier
twri  <- equity.twri(holding, refresh=TRUE, file=NA, period=period)
twrib <- equity.twri(bench  , refresh=TRUE, file=NA, period=period)
twri.ef <- ef(period=period, addline=FALSE)$twri

## calculate and plot performance
backtest <- function(twri, weight, twrib, twri.ef, xtsrange) {
    port <- portfolio.calc(twri[xtsrange], weight=weight, twrib=twrib[xtsrange])
    plotspace(2,2)
    out <- portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, 
                          twri.ef=twri.ef[xtsrange],
                          plottype=c('twri', 'ab', 'twrc', 'rra'), pch.hold = 16,
                          main=paste('Benchmark = ', names(twrib)[1], sep=''))
    return(list(port=port, efdata.Schwab=out$efdata.Schwab, efdata.simple=out$efdata.simple))
}
out <- backtest(twri, weight, twrib, twri.ef, xtsrange)
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
