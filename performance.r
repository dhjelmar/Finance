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

period        <- 'months'
xtsrange      <- '2016-12/2021-12'

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

## define xtsrange
xtsrange <- '2016-12/2021'  # 5 year
xtsrange <- '2018-12/2021'  # 3 year
xtsrange <- '2020-12/2021'  # 1 year

port <- portfolio.calc(twri[xtsrange], value=value[xtsrange], twrib=twrib[xtsrange])

plotspace(2,2)
portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, twri.ef=efdata$twri,
               plottype=c('twri', 'twrc', 'rra', 'ab'))



out <- performance.plot(portfolio, valuesheet, twrsheet, twrib, xtsrange, period,
                        portfolioname)
perf <- as_tibble( out$out.xtsrange$performance )  # tibble conversion strips the rownames
perf

df <- perf
## change twrc, std, twrc.ann, and std.ann to character and apply % sign
df[2:5]   <- sapply(df[2:5], function(x) scales::percent(x, accuracy=0.01))
df$alpha   <- round(df$alpha , 3)
df$beta    <- round(df$beta  , 3)
df$weight  <- round(df$weight, 2)
df$value   <- scales::dollar(df$value)
## print as data frame rather than tibble so right justifies
df <- as.data.frame(df)
df
         
## same as above but creates pdf file with plot output
out <- performance.plot(portfolio, valuesheet, twrsheet, twrib, xtsrange, period,
                        portfolioname, 
                        file = paste('performance.', portfolioname, '.pdf', sep=''))
