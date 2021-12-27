## Setup

## look into these
## library(RollingWindow) # try RollingCompound function <-- failed
## library(RcppRoll)      # try roll_prod                <-- failed

##-----------------------------------------------------------------------------
## setup
path <- getwd()
source(paste( strsplit( getwd(), 'Finance' ), '/R-setup/setup.r', sep='' ) )
r_files <- list.files(paste(path, '/modules/', sep=''), pattern="*.[rR]$", full.names=TRUE)
for (f in r_files) {
    ## cat("f =",f,"\n")
    source(f)
}

##-----------------------------------------------------------------------------
## READ DATA and CONVERT TO XTS
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

##-----------------------------------------------------------------------------
## select a portfolio and timeframe for the evaluation
portfolio     <-  church
portfolioname <- 'Church'

portfolio     <- de
portfolioname <- 'DE'

portfolio     <- verify
portfolioname <- 'verify01'

period        <- 'months'
xtsrange <- '2020-12/2021-12'

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

dlh
something is not correct:
  given: xtsrange  <- '2020-12/2021-12'
         period    <- 'months'
         portfolio <- church
  portfolio has correct twrcum for accounts but not portfolio
         14.96% in consolidated report
         14.97% in Octave
         15.09% in R

         
## same as above but creates pdf file with plot output
out <- performance.plot(portfolio, valuesheet, twrsheet, twrib, xtsrange,
                        portfolioname, 
                        file = paste('performance.', portfolioname, '.pdf', sep=''))
