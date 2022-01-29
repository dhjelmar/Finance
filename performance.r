## Setup

## look into these
## library(RollingWindow) # try RollingCompound function <-- failed
## library(RcppRoll)      # try roll_prod                <-- failed

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
## READ DATA and CONVERT TO XTS
out        <- performance.read()
valuesheet <- out$valuesheet
twrsheet   <- out$twrsheet

##-----------------------------------------------------------------------------
## define portfolios created from combining accounts
accounts <- names(twrsheet)
print(accounts)
church <- accounts[1:5]
de     <- accounts[grepl('^D |^E |^DE', accounts)]
p      <- accounts[grepl('^P'         , accounts)]

##-----------------------------------------------------------------------------
## select a portfolio and timeframe for the evaluation
portfolio     <- de
portfolioname <- 'DE'

portfolio     <-  church
portfolioname <- 'Church'

portfolio     <-  p
portfolioname <- 'p'

period        <- 'months'
xtsrange1 <- '2020-12/2021'  # 1 year
xtsrange3 <- '2018-12/2021'  # 3 year
xtsrange5 <- '2016-12/2021'  # 5 year
xtsrange  <- xtsrange3

##-----------------------------------------------------------------------------
## pull twri from twrsheet
twri <- twrsheet[, (colnames(twrsheet) %in% tidyselect::all_of(portfolio))]
## adjust twri to market days
twri <- twri.adjust(twri)

##-----------------------------------------------------------------------------
## get effective frontier data
efdata <- ef(model='Schwab', period='days', addline=FALSE)
## adjust dates to same as defined in twri
efdata$twri <- twri.adjust(efdata$twri, dates.new = zoo::index(twri))

##-----------------------------------------------------------------------------
## define benchmarks
twrib  <- (efdata$eftwri$schwab_60_40 + efdata$eftwri$schwab_80_20) / 2
names(twrib) <- 'Schwab_70_30'
twrib <- twri.adjust(twrib, dates = zoo::index(twri))

## add CPI+5% as a benchmark if exists inside twrsheet
if (which(grepl('CPI', names(twrsheet))) > 0) {
    ## twri for CPI (consumer price index) provided for plotting
    cpi          <- twri.adjust( twrsheet$CPI )
    twrib        <- cbind(twrib, cpi)
    p5           <- 0
    twrib$CPI.p5 <- 0
    date <- as.numeric( zoo::index(twrib) )
    for (i in 2:nrow(twrib)) {
        ## add 5% to CPI
        elapsed <- date[i] - date[i-1]
        p5[i]   <- 0.05 / (365.25 / elapsed)
        twrib$CPI.p5[i] <- twrib$CPI[i] * 0 + p5[i]
        ## twrc.cpi <- twrc.calc(twri.cpi, zero.from=TRUE)
    }
    ## assume 1st p5 entry is same as the 2nd
    twrib$CPI.p5[1] <- twrib$CPI[1] + p5[2]
    ## eliminate the CPI column
    twrib$CPI <- NULL
    ## rename 'CPI.p5' to 'CPI+5%'
    ## the following works but the complex name gets messed up and replaced down the line
    ## names(twrib) <- stringr::str_replace(names(twrib), 'CPI.p5', 'CPI+5%')
}


##-----------------------------------------------------------------------------
## pull value from valuesheet
value <- valuesheet[, (colnames(valuesheet) %in% tidyselect::all_of(portfolio))]
## adjust twri to market days
value <- twri.adjust(value, twri.input = FALSE)


##-----------------------------------------------------------------------------
## evaluate portfolio
## look at available date range
print('Available dates:')
print(zoo::index(twri))


file <- NULL
## file <- 'de.pdf'
if (!is.null(file)) {
    ## create PDF of plots
    pdf(file = file, onefile = TRUE,          # creates a multi-page PDF file
        ## file = "performance%03d.pdf", onefile = FALSE,  # creates multiple PDF files
        width = 8,  # The width of the plot in inches
        height = 10) # The height of the plot in inches
}

for (xtsrange in c(xtsrange1, xtsrange3, xtsrange5)) {

    ## set 2x2 plot space filling by columns first
    par(mfcol=c(2,2))

    ## plot portfolio performance with selected benchmark
    port <- portfolio.calc(twri[xtsrange], value=value[xtsrange], twrib=twrib[xtsrange])
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
    port.m  <- portfolio.calc(twri.m[xtsrange], value=value.m[xtsrange], twrib=twrib.m[xtsrange])
    efdata.m <- ef(model='Schwab', period='months', addline=FALSE)
    portfolio.plot(twri=port.m$twri, twrc=port.m$twrc, perf=port.m$perf, 
                   twri.ef=efdata.m$twri[xtsrange],
                   plottype=c('twri', 'ab', 'twrc', 'rra'), pch.hold = 16,
                   main=paste('Baseline = ', names(twrib.m)[1], sep=''))

    ## print summary output from calls to portfolio.calc
    portfolio.calc.print(port)
    portfolio.calc.print(port.m)
    
}

## evaluate portfolio as if it was a mutual fund
port.twri <- port.m$twri$portfolio
out <- equity.eval(portfolioname, bench='SPY', twri=port.twri, period='months')

if (!is.null(file)) dev.off() # close external pdf (or jpg) file


## interactive plots
xtsrange <- xtsrange3
port.m   <- portfolio.calc(twri.m[xtsrange], value=value.m[xtsrange], twrib=twrib.m[xtsrange])
efdata.m <- ef(model='Schwab', efdata=efdata.m$twri[xtsrange], annualize=TRUE, addline=FALSE)
shinyplot(port.m$perf, 'beta'  , 'alpha')
shinyplot(port.m$perf, 'std.ann', 'twrc.ann', xline=efdata.m$ef$efstd, yline=efdata.m$ef$eftwrc)
