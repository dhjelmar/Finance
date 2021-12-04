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
portfolioname <- 'My Portfolio'
holding       <- c('PTNQ', 'SPYG', 'VOOG', 'JPST', 'LALDX', 'PONAX', 'SPLV')
holding       <- c('SPY', 'IWM', 'EFA', 'AGG', 'SHV')
weight        <- rep(1/length(holding), length(holding))

## define a benchmark to use in the evaluation
twrib   <- 'SPY'

## define a timeframe and period for incremental TWR
period   <- 'months'
from     <- '2018-10-31'
to       <- '2021-11-30'
duration <- paste(from, 'to', to, sep=' ')

## evaluate portfolio
out <- portfolio.eval(holding, weight=weight, twrib=twrib,
                      plottype = c('twrc', 'rr', 'twri', 'ab'),
                      from=from, to=to, period=period,
                      main = paste(portfolioname, '; duration =', duration, sep=' '))

## alternage evalaution providing xts object for twri
## this avoid again looking up the holding performance
out <- portfolio.eval(holding, weight=weight, twri=out$twri, twrib=twrib,
                      plottype = c('twrc', 'rr', 'twri', 'ab'),
                      from=from, to=to, period=period,
                      main = paste(portfolioname, '; duration =', duration, sep=' '))

## look at correlation between investments
pairsdf(as.data.frame(out$twri))

##-----------------------------------------------------------------------------
## DEEP DIVE INTO SPECIFIC HOLDING
out <- equity.eval('BKI', 'SPY', duration='3 years')
out
