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

option <- 3

if (os == 'windows') {
    account_info <- readall("F:\\Documents\\01_Dave's Stuff\\Finances\\allocation.xlsx",
                           sheet="all assets", header.row = 2, data.start.row = 4)
    account_info$Account_Name <- account_info$Account

} else if (os != 'windows' & option == 1) {
    account_info <- readall("account_info.xlsx")
    account_info$Holding <- account_info$Symbol

} else if (os != 'windows' & option == 2) {
    account_info <- readall("Edelman.xlsx")
    account_info$Quantity <- account_info$Shares
    account_info$Account_Name <- paste(account_info$Owner, account_info$Account_Type, sep=' ')
    account_info <- account_info[account_info$Holding != 'SCGE1' &
                                 account_info$Holding != 'SCGI1' &
                                 account_info$Holding != 'SCGL1' &
                                 account_info$Holding != 'SCGN1' &
                                 account_info$Holding != 'SCGS1' &
                                 account_info$Holding != 'SCII1'   ,]

} else if (os != 'windows' & option == 3) {
    account_info <- readall("allocation_stripped.xlsx", sheet='all assets', 
                            header.row=2, data.start.row=4)
    account_info$Holding <- account_info$Symbol
    ## holdings in charity account are not in yahoo
    account_info <- account_info[account_info$Account_Type != 'Charity',]
    account_info$Account_Type[account_info$Account_Type == 'ira'] <- 'IRA'
    account_info$Account_Type[grepl('FAM', account_info$Account_Name)] <- 'FAM'
    ## eliminate tiny amount in KD which is an 11/4/21 IBM spinnoff (short history can crash functions)
    account_info <- account_info[account_info$Holding != 'KD',]
}
    
## strip out any lines with incomplete info
account_info <- na.omit(account_info)

## yahoo uses "-" instead of "." or "/" in symbol names so convert
account_info$Holding <- gsub("\\.|\\/", "-", account_info$Holding)

## yahoo does not have cash, but added option to give 'Cash' a negligible return
## do the same for moneymarkets and individual bonds (long symbols) since also not in yahoo
account_info[(account_info$Holding == 'Cash & Cash Investments'),]$Holding <- 'Cash'
account_info[(account_info$Holding == 'SWVXX' | account_info$Holding == 'SWYXX'),]$Holding <- 'Cash'
account_info[nchar(account_info$Holding) > 8,]$Holding <- 'Cash'

## strip to only what is needed to identify unique accounts
account_info <- select(account_info, c('Account_Name', 'Owner', 'Account_Type', 'Holding', 'Quantity'))

debug.flag <- FALSE
if (isTRUE(debug.flag)) {
    account_info_all <- account_info
    account_info <- account_info_all[136:145,]
}

##-------------------------------------------------------------------------
## GET TWR FOR EVERY HOLDING and BENCHMARK

## ## set period for twri (days, months, years)
period     <- 'months'

## holdings
holdingall <- unique(account_info$Holding)
twriall    <- equity.twri(holdingall, period='month') # this works as long as 1st holding is not "Cash"

## benchmark options
efdata   <- ef('Schwab',        from='1900-01-01', to=Sys.Date(), addline=FALSE)
efdata2  <- ef('S&P 500 / AGG', from='1900-01-01', to=Sys.Date(), addline=FALSE)
eftwri   <- cbind(efdata$twri, efdata$eftwri, efdata2$eftwri)

##-------------------------------------------------------------------------
## CREATE PORTFOLIO TO BE EVALUTED BY SELECTING ACCOUNT (or COMBINED ACCOUNT)
## create dataframe called "portfolio" with columns labeled "Holding" and "Quantity"

## this will create a separte dataframe for each Account_Name (or Account_Type)
accountc <- account_info[account_info$Owner == 'C',]
accountp <- account_info[account_info$Owner == 'P',]
accountd <- account_info[(account_info$Owner == 'D' |
                          account_info$Owner == 'DE' |
                          account_info$Owner == 'E' ),]

account <- accountd
if (isTRUE(debug.flag)) account <- account_info
account_list <- split(account, account$Account_Type)
account_list <- split(account, account$Account_Name)
names(account_list)

naccounts <- length(names(account_list))
perf_all <- NA
mv       <- NA
portfolio_twri <- NA
plotspace(2,2)
for (i in 1:(naccounts+1)) {

    if (i == naccounts + 1) {
        portfolio <- account
        portfolioname <- 'All Combined'
        
    } else {
        ## select portfolio
        portfolio     <- account_list[[i]]
        portfolioname <- names(account_list[i])
    }

    cat('portfolio =', i, 'of', naccounts+1, 
        '; portfolioname =', portfolioname, '\n')

    ## strip out only Holding and Quantitiy
    portfolio <- select(portfolio, c('Holding', 'Quantity'))
    
    ## COLLAPSE IDENTICAL HOLDINGS (ESPECIALLY FOR COMBINED ACCOUNTS) AND DETERMINE WEIGHTS
    portfolio <- weights(portfolio, portfolioname)

    ## market value for portfolio
    mv[i] <- sum(portfolio$Market_Value)

    ## GET TWRI FOR PORTFOLIO FROM TWRIALL
    twri <- portfolio.twri(twriall, portfolio$Holding)

    ## EVALUATE PORTFOLIO
    eftwri$schwab_70_30 = (eftwri$schwab_60_40 + eftwri$schwab_80_20)/2
    twrib <- eftwri$schwab_70_30
    twrib <- eftwri$SPY
    from = '2018-12-31'
    to   = '2021-11-30'
    xtsrange <- paste(from, '/', to, sep='')
    duration <- paste(from, 'to', to, sep=' ')
    ## na <- 'omit'
    ## if (portfolioname == 'IRA' | portfolioname == 'All Combined') na <- 'zero'  # IFED too short duration for evaluation
    na <- 'zero'  # this will force all of the portfolios to be on the same duration

    ## replace portfolio.eval with portfolio.calc and portfolio.plot
    ## out <- portfolio.eval(portfolio$Holding,
    ##                       portfolio$Weight,
    ##                       twri  = twri,
    ##                       twrib = twrib,
    ##                       from  = from,
    ##                       to    = to,
    ##                       na    = na,
    ##                       plottype = c('twrc', 'rr', 'ab', 'twri'),
    ##                       main = paste(portfolioname, '; duration =', duration, sep=' '))

    port <- portfolio.calc(twri[xtsrange], weight=portfolio$Weight, twrib=twrib[xtsrange])
    portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, 
                   twri.ef=efdata$twri[xtsrange],
                   plottype=c('twri', 'ab', 'twrc', 'rra'), pch.hold = 16,
                   main=paste('Benchmark = ', names(twrib)[1], sep=''))
    
    ## add value to output
    port$perf$value <- c(portfolio$Market_Value,
                         sum(portfolio$Market_Value, na.rm = TRUE),
                         rep(NA, ncol(twrib)))
    performance <- port$perf

    ## collect twri in a list
    portfolio_twri[i] <- list(port$twri)
    ## performance[order(performance$twrcum),]
    performance$portfolioname <- portfolioname
    performance$duration      <- duration

    ## collect results
    perf_all <- rbind(perf_all, performance)
    
}
## remove 1st row
perf_all <- perf_all[-1,]
names(portfolio_twri) <- c(names(account_list), 'All Combined')

## look at correlation betweeen each investment in an account
## pairsdf(as.data.frame(portfolio_twri$Roth))

## pull out and combine portfolio totals and benchmark stats
pp <- perf_all
pp_type  <- pp[pp$holding == 'portfolio',]
pp_bench <- pp[nrow(pp),][1,]
pp_bench$portfolioname <- pp_bench$holding
perf_summary <- rbind(pp_type, pp_bench)
perf_summary$holding <- NULL
## perf_summary$Market_Value <- c(mv, NA)   # NA is for the benchmark
totalvalue <- sum(perf_summary$value, na.rm=TRUE) / 2  # divide by 2 because "all combined" is in list
perf_summary$weight       <- perf_summary$value / totalvalue
rownames(perf_summary) <- 1:nrow(perf_summary)
perf_summary <- as_tibble(perf_summary)
printdf(perf_summary, 999)

## create plots
plotspace(2,1)
## risk/return plot for all accounts in portfolio
with(perf_summary, plotfit(std, twrc, portfolioname,
                           xlab = 'Standard Deviation',
                           ylab = 'Cumulative TWR', 
                           bg   = 'grey80',
                           interval = 'noline',
                           suppress = 'yes',
                           xlimspec = range(std   , out$efdata$std),
                           ylimspec = range(twrc, out$efdata$twrc)))
## add efficient fontier lines
efdata.Schwab <- ef(model='Schwab', efdata=efdata, from=from, to=to, annualize=FALSE, addline=TRUE, col='black', lty=1, pch=3)
efdata.simple <- ef(model='simple', efdata=efdata, from=from, to=to, annualize=FALSE, addline=TRUE, col='black', lty=2, pch=3)
## alpha/beta plot for all accounts in portfolio
with(perf_summary, plotfit(beta, alpha, portfolioname,
                           bg   = 'grey80',
                           interval = 'noline',
                           suppress = 'yes'))
abline(v=1)
abline(h=0)

shinyplot(as.data.frame(perf_summary), 'std', 'twrc')
shinyplot(as.data.frame(perf_summary), 'beta', 'alpha')

## select portfolio for shiny plot
## account_list <- split(account, account$Account_Type)
portfolio_list <- split(perf_all, perf_all$portfolioname)
names(portfolio_list)
portfolio <- as_tibble(portfolio_list$'D Roth')
printdf(portfolio, 999)

## Schwab Invest      = 80/20
##        IRA w/o FAM = 60/40


## ## plot interactive
## if (os == 'windows') {
##     ## following uses plotly which does not work on Chromebook
##     plot_interactive(portfolio, 'std', 'twrcum')
##     plot_interactive(portfolio, 'beta', 'alpha')
## }
portfolio <- perf_all[perf_all$portfolioname == 'D Roth',]
efline <- efdata.Schwab$ef
shinyplot(as.data.frame(portfolio), 'std', 'twrc', xline = efline$efstd, yline=efline$eftwrc)
shinyplot(as.data.frame(portfolio), 'beta', 'alpha')

## biggest holdings
df <- perf_all[perf_all$holding != 'portfolio',]
head( df[order(df$value, decreasing = TRUE),], 20 )
shinyplot(perf_all[perf_all$holding != 'portfolio',], 'value', 'twrc')

##-----------------------------------------------------------------------------
## DEEP DIVE INTO SPECIFIC HOLDING
out <- equity.eval('BKI', 'SPY')
out <- equity.eval('BKI', 'SPY', duration='3 years')

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
