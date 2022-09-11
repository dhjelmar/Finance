## Setup

## look into these
## library(RollingWindow) # try RollingCompound function <-- failed
## library(RcppRoll)      # try roll_prod                <-- failed

##-----------------------------------------------------------------------------
## setup
os <- .Platform$OS.type
if (os == 'windows') {
    ## load generic modules
    source("F:\\Documents\\01_Dave\\Programs\\GitHub_home\\R-setup\\setup.r")
    ## identify working folder
    path <- c("f:/Documents/01_Dave/Programs/GitHub_home/Finance/")
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
## READ DATA and CONVERT TO XTS
out        <- performance.read()
valuesheet <- out$valuesheet
twrsheet   <- out$twrsheet


##-----------------------------------------------------------------------------
## define portfolios created from combining accounts
accounts <- names(twrsheet)
print(accounts)
church <- accounts[1:8]
endow  <- accounts[grepl('End', accounts)]
de     <- accounts[grepl('^D |^E |^DE', accounts)]
p      <- accounts[grepl('^P'         , accounts)]


##-----------------------------------------------------------------------------
## EVALUATION  SETTINGS
## select a portfolio and timeframe for the evaluation
portfolio     <- de
portfolioname <- 'DE'

portfolio     <-  p
portfolioname <- 'p'

portfolio     <-  church
portfolioname <- 'Church'

portfolio     <-  endow
portfolioname <- 'Church Endowment Fund'

## define vector of xtsranges to evaluate
period        <- 'months'
ytd       <- '2021-12/2022'  # ytd
xtsrange1 <- '2021-12/2022'  # 1 year
xtsrange3 <- '2019-12/2022'  # 3 year
xtsrange5 <- '2017-12/2022'  # 5 year
xtsrange.vec <- c(ytd, xtsrange1, xtsrange3, xtsrange5)
xtsrange.vec <- '2020-12/2022-01'
xtsrange.vec <- ytd
## xtsrange.vec <- '2021-12/2022-01' # YTD

## identify name of pdf filename to create (NULL plots to screen)
filename <- 'church.pdf'
filename <- NULL


##-----------------------------------------------------------------------------
## evalaute portfolio

##----------------------
## prepare XTS objects for evaluation
out    <- prep(portfolio)
twri   <- out$twri
twrib  <- out$twrib
efdata <- out$efdata
value  <- out$value

##----------------------
## create summary table
port.summary <- portfolio.summary(twri, value, twrib)
## port.summary <- portfolio.summary(twri, value, twrib, periods=list( 8, '1 year'), period.names='extract')
## port.summary <- portfolio.summary(twri, value, twrib, periods=list(12, '12 months'), period.names='extract')
## port.summary <- portfolio.summary(twri, value, twrib, periods=list(12, '1 year'), period.names=list('TWRC 12 periods', 'TWRC 1 year'))

##----------------------
## create performance plots
if (!is.null(filename)) {
    ## create PDF of plots
    pdf(file = filename, onefile = TRUE,          # creates a multi-page PDF file
        ## file = "performance%03d.pdf", onefile = FALSE,  # creates multiple PDF files
        width = 11,  # The width of the plot in inches
        height = 8.5) # The height of the plot in inches
}



for (xtsrange in xtsrange.vec) {

    ## set 2x2 plot space filling by columns first
    par(mfcol=c(2,2))

    ## plot portfolio performance with selected benchmark
        ## treat missing twri as zero
    port <- portfolio.calc(twri[xtsrange], value=value[xtsrange], twrib=twrib[xtsrange], na.value=0)
    portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, 
                   twri.ef=efdata$twri[xtsrange],
                   plottype=c('twri', 'twrc', 'ab', 'rra'), pch.hold = 16,
                   main=paste('Benchmark = ', names(twrib)[1], sep=''))

    ## repeat above with SPY as baseline and converting everything to monthly returns
    value.m <- twri.adjust(value, d2m=TRUE, twri.input=FALSE)
    twri.m  <- twri.adjust(twri , d2m=TRUE)
    twrib.m  <- equity.twri('SPY', period='months')
    xtsrange <- range(zoo::index(twri.m[xtsrange]))
    xtsrange <- paste(xtsrange[1], '/', xtsrange[length(xtsrange)], sep='')
    port.m  <- portfolio.calc(twri.m[xtsrange], value=value.m[xtsrange], twrib=twrib.m[xtsrange], na.value=0)
    efdata.m <- ef(model='Schwab', period='months', addline=FALSE)
    portfolio.plot(twri=port.m$twri, twrc=port.m$twrc, perf=port.m$perf, 
                   twri.ef=efdata.m$twri[xtsrange],
                   plottype=c('twri', 'twrc', 'ab', 'rra'), pch.hold = 16,
                   main=paste('Baseline = ', names(twrib.m)[1], sep=''))

    ## print summary output from calls to portfolio.calc
    cat('\n\nxtsrange =', xtsrange, '\n\n')
    cat('Using twri directly and requested benchmark\n')
    print( portfolio.calc.print(port)   )
    cat('\n')
    cat('converting twri to month ends and using SPY as benchmark\n')
    print( portfolio.calc.print(port.m) )
    
}

## evaluate portfolio as if it was a mutual fund
port.m.all  <- portfolio.calc(twri.m, value=value.m, twrib=twrib.m)
port.twri <- port.m.all$twri$portfolio
port.twri <- na.omit(port.twri)
out <- equity.eval(portfolioname, bench='SPY', twri=port.twri, period='months')

if (!is.null(filename)) dev.off() # close external pdf (or jpg) file


## interactive plots
xtsrange <- '2018-12-31/2021-11-30'
port.m   <- portfolio.calc(twri.m[xtsrange], value=value.m[xtsrange], twrib=twrib.m[xtsrange])
efdata.m <- ef(model='Schwab', efdata=efdata.m$twri[xtsrange], annualize=TRUE, addline=FALSE)
shinyplot(port.m$perf, 'beta'  , 'alpha')
shinyplot(port.m$perf, 'std.ann', 'twrc.ann', xline=efdata.m$ef$efstd, yline=efdata.m$ef$eftwrc)
