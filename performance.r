## Setup

## look into these
## library(RollingWindow) # try RollingCompound function <-- failed
## library(RcppRoll)      # try roll_prod                <-- failed

##-----------------------------------------------------------------------------
## setup
path <- getwd()
source(paste( strsplit( getwd(), 'Finance' ), '/R-setup/setup.r', sep='' ) )
r_files <- list.files(paste(path, '/ modules/', sep=''), pattern="*.[rR]$", full.names=TRUE)
for (f in r_files) {
    ## cat("f =",f,"\n")
    source(f)
}

##-----------------------------------------------------------------------------
## READ DATA and CONVERT TO XTS
source('performance.read.r')
out        <- performance.read()
map        <- out$map
valuesheet <- out$valuesheet
twrsheet   <- out$twrsheet

##-----------------------------------------------------------------------------
## define portfolios created from combining accounts
accounts <- names(twrsheet)
print(accounts)
church <- c('1111', '3-33-333')
church <- accounts[1:5]
de     <- accounts[grepl('^D |^E |^DE', accounts)]
p      <- accounts[grepl('^P'         , accounts)]

## select a portfolio and timeframe for the evaluation
portfolio     <-  church
portfolioname <- 'Church'

portfolio     <- de
portfolioname <- 'DE'

portfolio     <- verify
portfolioname <- 'verify01'


from          <- '2020-12'
to            <- '2021-12'
period        <- 'months'
xtsrange      <- paste(from, '/' , to, sep='')

out <- performance.plot(portfolio, valuesheet, twrsheet, twrib, xtsrange,
                        portfolioname)
performance <- as_tibble( out$out.xtsrange$performance )
performance


out <- performance.plot(portfolio, valuesheet, twrsheet, twrib, xtsrange,
                        portfolioname, 
                        file = paste('performance.', portfolioname, '.pdf', sep=''))
