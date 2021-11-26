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
## eventually need: account_info = dataframe with "Account_Name", "Owner", "Account_Type",
##                                                "Holding", "Quantity"
##                                 other values can be included, but are not needed

option <- 2

if (os == 'windows') {
    account_info <- readall("F:\\Documents\\01_Dave's Stuff\\Finances\\allocation.xlsx",
                           sheet="all assets", header.row = 2, data.start.row = 4)
    account_info$Account_Name <- account_info$Account

} else if (os != 'windows' & option == 1) {
    account_info <- readall("account_info.xlsx")
    account_info$Holding <- account_info$Symbol

} else {
    account_info <- readall("Edelman.xlsx")
    account_info$Symbol   <- account_info$Holding
    account_info$Quantity <- account_info$Shares
    account_info$Account_Name <- paste(account_info$Owner, account_info$Account_Type, sep=' ')
    account_info <- account_info[account_info$Holding != 'SCGE1' &
                                 account_info$Holding != 'SCGI1' &
                                 account_info$Holding != 'SCGL1' &
                                 account_info$Holding != 'SCGN1' &
                                 account_info$Holding != 'SCGS1' &
                                 account_info$Holding != 'SCII1'   ,]
}
    
## strip out any lines with incomplete info
account_info <- na.omit(account_info)

## yahoo uses "-" instead of "." or "/" in symbol names so convert
account_info$Holding <- gsub("\\.|\\/", "-", account_info$Holding)

## consider moneymarkets and individual bonds (long symbols) as 3-month treasury
## since yahoo does not have info for them
account_info[(account_info$Holding == 'SWVXX' | account_info$Holding == 'SWYXX'),]$Holding <- 'SHV'
account_info[nchar(account_info$Holding) > 8,]$Holding <- 'SHV'

## yahoo does not have cash either, but added option to give 'Cash' a negligible return
account_info[(account_info$Holding == 'Cash & Cash Equivalents'),]$Holding <- 'Cash'

## strip to only what is needed to identify unique accounts
account_info <- select(account_info, c('Account_Name', 'Owner', 'Account_Type', 'Holding', 'Quantity'))


##-------------------------------------------------------------------------
## GET TWR FOR EVERY HOLDING and BENCHMARK

## ## set period for twri (days, months, years)
period     <- 'months'

## holdings
holdingall <- unique(account_info$Holding)
twriall    <- equitytwr(holdingall, period='month')

## benchmark options
efdata   <- ef('Schwab',        from='1900-01-01', to=Sys.Date(), addline=FALSE)
efdata2  <- ef('S&P 500 / AGG', from='1900-01-01', to=Sys.Date(), addline=FALSE)
eftwri   <- cbind(efdata$twri, efdata$eftwri, efdata2$eftwri)

##-------------------------------------------------------------------------
## CREATE PORTFOLIO TO BE EVALUTED BY SELECTING ACCOUNT (or COMBINED ACCOUNT)
## create dataframe called "portfolio" with columns labeled "Holding" and "Quantity"

## this will create a separte dataframe for each Account_Name
account <- split(account_info, account_info$Account_Name)
names(account)
portfolioname <- 'DE Invest'
portfolio <- account$`DE Invest`

##---------------

## list account names, owners, and types
unique(account_info$Account_Name)
unique(account_info$Owner)
unique(account_info$Account_Type)

##---------------

## select accounts to create portfolio and give it a name
portfolioname <- 'All Holdings'
portfolio     <- account_info

##---------------

portfolioname <- 'Investment Account'
portfolio <- subset(account_info, Account_Type == 'invest')

##---------------

portfolioname <- 'Dave IRA - Traditional'
portfolio <- subset(account_info, Account_Name == 'Dave IRA - Traditional')

##---------------

## strip out only Holding and Quantitiy
portfolio <- select(portfolio, c('Holding', 'Quantity'))

##-------------------------------------------------------------------------
## COLLAPSE IDENTICAL HOLDINGS (ESPECIALLY FOR COMBINED ACCOUNTS) AND DETERMINE WEIGHTS
portfolio <- weights(portfolio, portfolioname)

##-------------------------------------------------------------------------
## GET TWRI FOR PORTFOLIO FROM TWRIALL
twri <- porttwri(twriall, portfolio$Holding)

##-------------------------------------------------------------------------
## EVALUATE PORTFOLIO
eftwri$schwab_70_30 = (eftwri$schwab_60_40 + eftwri$schwab_80_20)/2
twrib <- eftwri$schwab_70_30
out <- portfolio_eval(portfolio$Holding,
                      portfolio$Weight,
                      twri  = twri,
                      twrib = twrib,
                      from = '2018-10-30',
                      to   = '2021-10-30',
                      plottype = 'cria',
                      portfolioname = portfolioname)

performance <- out$performance
performance[order(performance$twrcum),]

performance$Holding <- rownames(performance)
portfolio <- merge(portfolio, performance, by='Holding')
portfolio$weight <- NULL
printdf(portfolio, 99)

## ## plot interactive
## if (os == 'windows') {
##     ## following uses plotly which does not work on Chromebook
##     plot_interactive(rr, 'std', 'twrcum')
##     plot_interactive(rr, 'beta', 'alpha')
## }

shinyplot(as.data.frame(portfolio), 'std', 'twrcum')
shinyplot(as.data.frame(portfolio), 'beta', 'alpha')


-##-----------------------------------------------------------------------------
## DEEP DIVE INTO SPECIFIC HOLDING
out <- equityeval('BKI', 'SPY')
out <- equityeval('BKI', 'SPY', duration='3 years')

quantmod::getQuote(c('BKI'), src='yahoo', what = quantmod::yahooQF(c('P/E Ratio', 'PEG Ratio')))

quantmod::getQuote(c('BKI','SPY'), src='yahoo', what = quantmod::yahooQF(c('Previous Close',
                                                                           'P/E Ratio', 
                                                                           'Earnings/Share', 
                                                                           'EPS Forward',
                                                                           'Price/Book', 
                                                                           '52-week Low',
                                                                           '52-week High',
                                                                           '200-day Moving Average',
                                                                           'Dividend Yield',
                                                                           'Dividend/Share')))
