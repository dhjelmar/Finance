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
xtsrange      <- '2020-12/2021-12'

##-----------------------------------------------------------------------------
## evaluate portfolio
out <- performance.plot(portfolio, valuesheet, twrsheet, twrib, xtsrange, period,
                        portfolioname)
perf <- as_tibble( out$out.xtsrange$performance )  # tibble conversion strips the rownames
perf

df <- perf
df[2:4]   <- sapply(df[2:4], function(x) scales::percent(x, accuracy=0.01))
df$beta   <- round(df$beta  , 3)
df$weight <- round(df$weight, 2)
df$value  <- scales::dollar(df$value)
## print as data frame rather than tibble so right justifies
print( as.data.frame(df) )

         
## same as above but creates pdf file with plot output
out <- performance.plot(portfolio, valuesheet, twrsheet, twrib, xtsrange,
                        portfolioname, 
                        file = paste('performance.', portfolioname, '.pdf', sep=''))
