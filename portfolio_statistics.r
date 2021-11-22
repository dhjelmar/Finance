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
## Import portfolio information
## Only required info out of this section is to have a dataframe with:
##       Symbol
##       Weight
## In this example, additional info is saved
account_info <- readall("account_info.xlsx")
account_info <- readall("F:\\Documents\\01_Dave's Stuff\\Finances\\allocation.xlsx",
                       sheet="all assets", header.row = 2, data.start.row = 4)
account_info$Account_Name <- account_info$Account
account_info <- na.omit(account_info)

## yahoo uses "-" instead of "." or "/" in symbol names so convert
account_info$Symbol <- gsub("\\.|\\/", "-", account_info$Symbol)

## consider moneymarkets as cash since yahoo does not have info for them
account_info[(account_info$Symbol == 'SWVXX' | account_info$Symbol == 'SWYXX'),]$Symbol <- 'Cash'

## do same with individual bonds
account_info[nchar(account_info$Symbol) > 8,]$Symbol <- 'Cash'

## maybe better to consider those things SHV (3-month treasury)
account_info[(account_info$Symbol == 'Cash'),]$Symbol <- 'SHV'

## if too much was read in, strip to only what is needed to identify unique accounts
account_info <- select(account_info, c('Account_Name', 'Owner', 'Account_Type', 'Symbol', 'Quantity', 'Weight'))

##-------------------------------------------------------------------------
## SELECT ACCOUNT
account <- split(account_info, account_info$Account_Name)
names(account)
df <- account$`DE Invest`

## EVALUATE
holding <- df$Symbol
twri    <- equitytwr(holding)
twrib   <- equitytwr('SPY', period='months')
out <- portfolio(holding,
                 df$Weight,
                 twri  = twri,
                 twrib = twrib,
                 from = '2020-10-30',
                 to   = '2021-10-30',
                 plottype = 'cria')


out <- portfolio(holding,
                 df$Weight,
                 twri  = twri,
                 twrib = twrib,
                 from = '2018-10-30',
                 to   = '2021-10-30',
                 plottype = 'ra')

out$summary[order(out$summary$twrcum),]


##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------

## Below is additional but probably now broken fanciness to update weights
## based on quantities of each holding. Need to pull that into a separate function.

##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
## ADD PRICE FOR EACH SECURITY TO account_info

## identify securities
securitylist <- unique(account_info$Symbol)

## drop money markets and individual bonds (long symbol names) since yahoo does not deal with them
securitylist <- securitylist[! securitylist %in% c('SWVXX', 'SWYXX')]
securitylist <- securitylist[nchar(securitylist) < 8]

## obtain recent closing prices
out1 <- equityprice(securitylist, refresh=TRUE, file='out1.csv')
security <- data.frame(Symbol=securitylist, name=out1$Name, close=out1$Close)

## Add recent price data to account_info
account_info$Close <- security$close[match(account_info$Symbol, security$Symbol)]

## change NA closing prices to 1.0 (i.e., bonds and money markets)
temp <- account_info$Close
temp[is.na(temp)] <- 1
account_info$Close <- temp

##-----------------------------------------------------------------------------
## Get twr values for account securities
alltwr <- equitytwr(securitylist, refresh=FALSE, file='out2.csv')

## Get twr values for benchmark
benchtwr <- equitytwr('SPY', refresh=FALSE, file='out3.csv')


##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
## Select unique account or combine accounts for evaluation
unique(account_info$Account_Type)
## split account info by Acount_Type into a list of dataframes
account <- split(account_info, account_info$Account_Type)
names(account)

## select one to work with
i <- 2
dfname <- names(account)[i]
print(dfname)
df <- account[[i]]

## ## alternately could have used subset to pull out a single account or combine accounts
## invest <- subset(account_info, account_info$Account_Type == "Investment")
## ira    <- subset(account_info, account_info$Account_Type == "IRA - Traditional")
## dfname <- 'ira'
## df     <- ira

##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
## combine duplicate symbol entries if needed and drop columns except for Symbol and Quantity
df <- aggregate(df$Quantity, by=list(df$Symbol, df$Close), FUN=sum)
names(df) <- c('Symbol', 'Close', 'Quantity')

## ## convert data to vectors
## asset   <- as.character(df$Symbol)
## shares  <- df$Quantity


##-----------------------------------------------------------------------------
## GIVEN DATAFRAME WITH SYMBOL, CLOSING VALUE, AND QUANTITY OF THAT SYMBOL
## EVALUTATE THE SYBMOLS AND PORTFOLIO

stopped here

for (asset in df$Symbol) {
  out <- equityeval(asset, 'SPY')
  duration <- out$duration
alpha <- out$alpha
beta  <- out$beta
twrcum <- out$twrcum
benchcum <- out$benchcum
}

out <- equitymodel('AAPL', 'SPY')
out <- equityeval('AAPL', 'SPY')
DT::datatable(signif(out,4))


out <- equityeval('IFED', 'SPY', period='days', duration='1 year')
out <- equityeval('IFED', 'SPY', period='days', duration='1 year', from='2021-10-11')


outzoo <- zoo::zoo(out)
outzoo$duration_years <- NULL
zoo::index(outzoo) <- c(1,3,5)
zoo::plot.zoo(outzoo)




df <- xts::last(df, '5 years')



## df = dataframe containing Symbol, Close, and Quantity
## alltwr = XTS object with historical TWR for all symbols in df and possibly more
## benchtwr = XTS object with historical TWR for benchmark

## first determine weights of each asset
df$Value <- df$Close * df$Quantity
totalvalue <- sum(df$Value)
df$weight <- df$Value / totalvalue





stats <- data.frame(symbol,
                    name = symbolname,
                    shares = as.numeric(shares), 
                    value=as.numeric(value), 
                    twr  = twri,
                    beta, 
                    alpha)
## sort from low to high twr
stats <- stats[order(stats$twr),]


## calculate portfolio beta
beta_portfolio  <- sum( weight * beta  )
alpha_portfolio <- sum( weight * alpha )
twr_portfolio   <- sum( weight * twri  )
portfolio <- data.frame(symbol  = dfname, 
                        name   = 'portfolio',
                        shares = NA, 
                        value  = totalvalue, 
                        twr    = twr_portfolio,
                        beta   = beta_portfolio,
                        alpha  = alpha_portfolio)
stats <- rbind(stats, portfolio)
print(stats)

## plot portfolio
plotspace(1,2)
out <- plotfit(stats$alpha, stats$beta, stats$symbol, nofit=TRUE)
## xx <- stats[nrow(stats),]$beta
## yy <- stats[nrow(stats),]$alpha
## color <- as.character(out$legend[nrow(out$legend),]$color)
## points(xx, yy, pch=16, col=color)
out <- plotfit(stats$alpha, stats$twr, stats$symbol, nofit=TRUE)

## any correlation between alpha, beta, and twr?
abtwr <- select(stats, twr, alpha, beta)
pairsdf(abtwr)

## ## plot interactive
## if (os == 'windows') {
##     ## following uses plotly which does not work on Chromebook
##     plot_interactive(stats, 'beta', 'alpha')
## }

library(shiny)
shinyplot(as.data.frame(stats), 'beta', 'alpha')
}


