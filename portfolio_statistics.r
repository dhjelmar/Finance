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
account_info <- readall("account_info.xlsx")

## account_info <- readall("F:\\Documents\\01_Dave's Stuff\\Finances\\allocation.xlsx",
##                        sheet="all assets", header.row = 2, data.start.row = 4)
## account_info$Account_Name <- account_info$Account

## strip out any lines with incomplete info
account_info <- na.omit(account_info)

## yahoo uses "-" instead of "." or "/" in symbol names so convert
account_info$Symbol <- gsub("\\.|\\/", "-", account_info$Symbol)

## consider moneymarkets and individual bonds (long symbols) as 3-month treasury
## since yahoo does not have info for them
account_info[(account_info$Symbol == 'SWVXX' | account_info$Symbol == 'SWYXX'),]$Symbol <- 'SHV'
account_info[nchar(account_info$Symbol) > 8,]$Symbol <- 'SHV'

## yahoo does not have cash either, but added option to give 'Cash' a negligible return
account_info[(account_info$Symbol == 'Cash & Cash Equivalents'),]$Symbol <- 'Cash'

## strip to only what is needed to identify unique accounts
account_info <- select(account_info, c('Account_Name', 'Owner', 'Account_Type', 'Symbol', 'Quantity'))


##-------------------------------------------------------------------------
## GET TWR AND CURRENT PRICES FOR EVERY HOLDING and TWR FOR BENCHMARK

## ## set period for twri (days, months, years)
period     <- 'months'

## holdings
holdingall <- unique(account_info$Symbol)
closeall   <- equityprice(holdingall)
twriall    <- equitytwr(holdingall, period='month')

## benchmark options
efdata   <- ef('Schwab',        from='1900-01-01', to=Sys.Date(), addline=FALSE)
efdata2  <- ef('S&P 500 / AGG', from='1900-01-01', to=Sys.Date(), addline=FALSE)
eftwri   <- cbind(efdata$twri, efdata$eftwri, efdata2$eftwri)



###########################################################################
###########################################################################

## restart from here to create a new portfolio for evaluation

###########################################################################
###########################################################################

##-------------------------------------------------------------------------
## CREATE PORTFOLIO TO BE EVALUTED BY SELECTING ACCOUNT (or COMBINED ACCOUNT)
## list account names, owners, and types
unique(account_info$Account_Name)
unique(account_info$Owner)
unique(account_info$Account_Type)

## ## this will create a list of all accounts
## account <- split(account_info, account_info$Account_Name)
## names(account)
## portfolio <- account$`DE Invest`

## select accounts to create portfolio and give it a name
portfolioname <- 'Investment Account'
portfolio <- subset(account_info, Account_Type == 'invest')
portfolio <- select(portfolio, c('Symbol', 'Quantity'))
names(portfolio) <- c('Holding', 'Quantity')

## ## print portfolio to be evaluated
## print(portfolio, 9999)


##-------------------------------------------------------------------------
## COLLAPSE IDENTICAL HOLDINGS (ESPECIALLY FOR COMBINED ACCOUNTS)

## combine duplicate holdings
portfolio <- aggregate(portfolio$Quantity, by=list(portfolio$Holding), FUN=sum)
names(portfolio) <- c('Holding', 'Quantity')


##-------------------------------------------------------------------------
## DETERMINE WEIGHT OF EACH HOLDING

## add closing price data to portfolio
closeall$Holding <- rownames(closeall)
portfolio <- merge(portfolio, closeall, by='Holding')
portfolio <- select(portfolio, c('Holding', 'Name', 'Quantity', 'Close'))

## determine weight
portfolio$Market_Value <- portfolio$Quantity * portfolio$Close
totalvalue             <- sum(portfolio$Market_Value)
portfolio$Weight       <- portfolio$Market_Value / totalvalue

## print(portfolio)

##-------------------------------------------------------------------------
## GET TWRI FOR PORTFOLIO FROM TWRIALL
twri <- twriall[, (colnames(twriall) %in% portfolio$Holding)]
times <- zoo::index(twri)

## reorder to match order in portfolio dataframe
## first convert to dataframe
twri <- as.data.frame(twri)
## reorder to match portfolio
twri <- select(twri, portfolio$Holding)
## convert back to xts
twri <- xts::as.xts(twri)
## above did not recover the exact same timestamp so replacing
zoo::index(twri) <- times

##-------------------------------------------------------------------------
## EVALUATE PORTFOLIO
eftwri$schwab_70_30 = (eftwri$schwab_60_40 + eftwri$schwab_80_20)/2
twrib <- eftwri$schwab_70_30
out <- portfolio_eval(portfolio$Holding,
                      portfolio$Weight,
                      twri  = twri,
                      twrib = twrib,
                      from = '2020-10-30',
                      to   = '2021-10-30',
                      plottype = 'cria',
                      portfolioname = portfolioname)

summary <- out$summary
summary[order(summary$twrcum),]

portfolio <- merge(portfolio, summary, by='Holding')
portfolio$weight <- NULL
portfolio$label  <- NULL
printdf(portfolio, 99)

## ## plot interactive
## if (os == 'windows') {
##     ## following uses plotly which does not work on Chromebook
##     plot_interactive(rr, 'std', 'twrcum')
##     plot_interactive(rr, 'beta', 'alpha')
## }

library(shiny)
shinyplot(as.data.frame(summary), 'std', 'twrcum')
shinyplot(as.data.frame(summary), 'beta', 'alpha')

