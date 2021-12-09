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
weight   <- holdings$weight / 100


## define a benchmark to use in the evaluation
twrib   <- 'SPY'

##-----------------------------------------------------------------------------
## define a timeframe and period for incremental TWR
period   <- 'months'
from     <- '2018-12-31'
to       <- '2021-11-30'
duration <- paste(from, 'to', to, sep=' ')

## evaluate portfolio
out <- portfolio.eval(holding, weight=weight, twrib=twrib,
                      plottype = c('twrc', 'rr', 'twri', 'ab'),
                      from=from, to=to, period=period,
                      main = paste(portfolioname, '; duration =', duration, sep=' '))

efdata <- out$efdata.Schwab$ef
## ef <- out$efdata.simple$ef
shinyplot(as.data.frame(out$performance), 'std', 'twrcum', xline=efdata$efstd, yline=efdata$eftwrcum)
shinyplot(as.data.frame(out$performance), 'beta', 'alpha')

## look at correlation between investments
pairsdf(as.data.frame(out$twri))

##-----------------------------------------------------------------------------
## how about this last year?
## Note: making use of prior xts object for twri
##       this avoids again looking up the holding performance
##       it only works here because the duration is the same or shorter than what is twri
from     <- '2020-10-31'
to       <- '2021-11-30'
duration <- paste(from, 'to', to, sep=' ')
out <- portfolio.eval(holding, weight=weight, twri=out$twri, twrib=twrib,
                      plottype = c('twrc', 'rr', 'twri', 'ab'),
                      from=from, to=to, period=period,
                      main = paste(portfolioname, '; duration =', duration, sep=' '))
pairsdf(as.data.frame(out$twri))

##-----------------------------------------------------------------------------
## DEEP DIVE INTO SPECIFIC HOLDING
out <- equity.eval('BKI', 'SPY', duration='3 years')
out <- equity.eval('IFED', 'SPY', period='days', duration='3 years')
out
