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
    ## load performance project modules
    path <- c("f:/Documents/01_Dave's Stuff/Programs/GitHub_home/Finance/")
} else {
    ## os == unix
    source('~/GitHub_repos/R-setup/setup.r')
    path <- c('~/GitHub_repos/Finance/')
}
## set working folder
setwd(path)

r_files <- list.files(paste(path, 'modules/', sep=''), pattern="*.[rR]$", full.names=TRUE)
for (f in r_files) {
    ## cat("f =",f,"\n")
    source(f)
}

##-----------------------------------------------------------------------------
## READ DATA
filename <- 'performance_data_example.xlsx'
map  <- readall(filename, sheet = 'Map',    header.row=3)
## account <- map[, c('Account_Number', 'Owner', 'Owner_Group', 'Account_Name')]
valuesheet <- readall(filename, sheet = 'value', header.row=6, rename=FALSE)
twrsheet   <- readall(filename, sheet = 'TWR',   header.row=6, rename=FALSE)

##-----------------------------------------------------------------------------
## CONVERT tibble TO XTS
## first change to dateframe so can set rownames as dates
## could convert directly to XTS but then, since "Date" is character, 
## all matrix columns would be character
valuesheet           <- as.data.frame(valuesheet)
rownames(valuesheet) <- valuesheet$Date
valuesheet$Date      <- NULL
valuesheet           <- xts::as.xts(valuesheet, order.by=as.Date(rownames(valuesheet)))

twrsheet           <- as.data.frame(twrsheet)
rownames(twrsheet) <- twrsheet$Date
twrsheet$Date      <- NULL
twrsheet           <- xts::as.xts(twrsheet, order.by=as.Date(rownames(twrsheet)))

##-----------------------------------------------------------------------------
## adjust any month end dates to dates the market is open
out <- equity.history('SPY', period='days')
closedates <- as.Date( zoo::index(out$close) )

valuedates <- as.Date( zoo::index(valuesheet) )
twrdates   <- zoo::index(twrsheet)

closest <- function(x, range=closedates) {
    ## closedates[findInterval(as.Date('2021-01-31'), as.Date(closedates))]
    closedates[findInterval(as.Date(x), as.Date(closedates))]
}
## closest('2021-01-31')

adjust <- unlist( purrr::pmap(list(x = valuedates), function(x) closest(x)) )
zoo::index(valuesheet) <- as.Date(adjust, origin='1970-01-01')

adjust <- unlist( purrr::pmap(list(x = twrdates), function(x) closest(x)) )
dates  <- as.Date(adjust, origin='1970-01-01')
zoo::index(twrsheet) <- dates

##-----------------------------------------------------------------------------
## define portfolios created from combining accounts
accounts <- names(twrsheet)
print(accounts)
church <- c('1111', '3-33-333')

## select a portfolio and timeframe for the evaluation
portfolio     <-  church
portfolioname <- 'Church'
from          <- '2016-12-31'
to            <- '2021-12-31'
period        <- 'months'
duration      <- paste(from, 'to', to, sep=' ')

## determine weight of each portion of portfolio
value         <- valuesheet[,names(valuesheet) %in% church]
current.value <- as.numeric( tail(value, 1) )
weight        <- current.value / sum(current.value)

## create value plot
plotspace(3,1)
plotxts(value)
## period = days used in the following so there is a unique value for every date read from Excel
## all date entries are consisered equally in evaluation of standard deviation, alpha, and beta
out <- portfolio.eval(portfolio, weight=weight, twri=twrsheet, twrib='SPY',
                      plottype = c('twrc', 'twri'), arrange=FALSE,
                      from=from, to=to, period=period,
                      main = paste(portfolioname, ': ', duration, 
                                   '; period = ', period, sep=''))

## create risk/return plot
out <- portfolio.eval(portfolio, weight=weight, twri=twrsheet, twrib='SPY',
                      plottype = c('rr', 'ab'),
                      from=from, to=to, period=period,
                      main = paste(portfolioname, ': ', duration, 
                                   '; period = ', period, sep=''))
twri <- out$twri

## evaluate portfolio as if it was a mutual fund
out <- equity.eval(portfolioname, 'SPY', twri=twri$portfolio, period=period)




###############################################################################
###############################################################################
## 
##
## DROP THE REST BELOW?
##
##
###############################################################################
###############################################################################


notusing <- function() {
    ## CREATE ACCOUNT DATAFRAME
    account <- data.frame(number      = map$Account_Number,
                          owner       = map$Owner,
                          owner_group = map$Owner_Group,
                          name        = map$Account_Name)
    ## initialize value and twr
    account$date  <- 0
    account$value <- 0
    account$twr   <- 0
    for (i in 1:nrow(map)) {
        ## if (i == 9) browser()
        
        ## assign date field
        account$date[i]  <- list( zoo::index(valuesheet) )
        
        valuecol         <- which( names(valuesheet) == map[[i, 'Account_Number']])
        account$value[i] <- list( valuesheet[[valuecol]] )
        
        ## assign twr field
        twrcol           <- which( names(twrsheet)   == map[[i, 'Account_Number']])
        account$twr[i]   <- list( twrsheet[[twrcol]] )
    }

    ##----------------------------------------------------

    ## COMBINE ACCOUNTS AS NEEDED
    combine <- function(name, combine, all = account) {
        ## name    = name for new combined 'account'
        ## combine = vector with the indices of accounts to be combined
        value <- 0
        for (i in combine) {
            value <- value + all$value[i]
        }
        
        for (idate in 1:length(all$date) {
            
            for (i in combine) {
                value <- value + all$value[i]
                twr   <- twr 
            }
        }
        }
                                        # combine('one and two', c(1,2))

            ##----------------------------------------------------



            
            ## extract data for plotting
            df <- plotdata(3,2)


        i = 2
        plot(account$date[[i]], account$value[[i]], main=account$name[i])






        ##--- STOPPED HERE --------------------------------------------------


        ## # Before dropping rows with N/A, reduce dataframe to only columns I care about
        ## #df <- subset(avg, select=c(avg$'US Large Cap',avg$'US Large Cap Value'))
        ## avg$US_L  <- as.numeric(avg$'US Large Cap')
        ## avg$US_LV <- as.numeric(avg$'US Large Cap Value')
        ## avg$US_LG <- as.numeric(avg$'US Large Cap Growth')
        ## avg$US_M  <- as.numeric(avg$'US Mid Cap')
        ## avg$US_MV <- as.numeric(avg$'US Mid Cap Value')
        ## avg$US_MG <- as.numeric(avg$'US Mid Cap Growth')
        ## avg$US_S  <- as.numeric(avg$'US Small Cap')
        ## avg$US_SV <- as.numeric(avg$'US Small Cap Value')
        ## avg$US_SG <- as.numeric(avg$'US Small Cap Growth')
        ## avg$Int_Dev         <- as.numeric(avg$'Intl Developed ex-US Market')
        ## avg$Int_Emg         <- as.numeric(avg$'Emerging Markets')
        ## avg$Bond_short      <- as.numeric(avg$'Short-Term Investment Grade')
        ## avg$Bond_high_yield <- as.numeric(avg$'High Yield Corporate Bonds')
        ## avg$REIT            <- as.numeric(avg$REIT)
        ## avg$Cash            <- as.numeric(avg$Cash)
        ## 
        ## df <- subset(avg,select=c(Year
        ##                           ,US_L,US_LV,US_LG
        ##                           ,US_M,US_MV,US_MG
        ##                           ,US_S,US_SV,US_SG
        ##                           ,Int_Dev, Int_Emg
        ##                           ,Bond_short, Bond_high_yield
        ##                           ,REIT, Cash)
        ##             )
        ## 
        ## df <- subset(avg,select=c(Year
        ##                           ,US_L
        ##                           ,US_M
        ##                           ,US_S
        ##                           ,Int_Dev, Int_Emg
        ##                           ,Bond_short, Bond_high_yield
        ##                           ,REIT, Cash)
        ##             )
        ## 
        ## # Convert "N/A" with what R considers to be NA and then drop those rows
        ## df <- df
        ## df[df == "N/A"]  <- NA
        ## df <- na.omit(df)             # drops rows with any NA values
        ## 
        ## # Calculate combinations
        ## df$roll_schwab_70_30 = 0.40*df$US_L + 0.13*df$US_S + 0.17*df$Int_Dev + 0.25*df$Bond_short + 0.05*df$Cash
        ## 
        ## # Create 1+avg_return dataframe
        ## df_plus1 <- df
        ## df_plus1[,2:ncol(df)] <- 1 + df_plus1[,2:ncol(df)]
        ## 
        ## # Create function to calculate rolling average for all except 1st column
        ## rolling_return <- function(df,nyr) {
        ## # rolling$US_L <- roll_prod(df_plus1$US_L, n=nyr, align="right", fill=0)^(1/nyr) - 1
        ## rolling <- df
        ## i <- 1
        ## for (col in 2:ncol(df)) {
        ## i <- i+1
        ## rolling[[i]] <- roll_prod(df[[i]], n=nyr, align="right", fill=0)^(1/nyr)
        ## }
        ## rolling[rolling == 0]  <- NA
        ## rolling <- na.omit(rolling)    # strip away rows with NA in them (i.e., previously blank)
        ## rolling[,2:ncol(rolling)] <- rolling[,2:ncol(rolling)] - 1
        ## return(rolling)   # needed to return the entire rollign dataframe and not just the last line in function
        ## }
        ## 
        ## # Calculate rolling 1 year average annual return
        ## rolling_1 <- rolling_return(df_plus1,1)
        ## 
        ## # Calculate rolling 3 year average annual return
        ## rolling_3 <- rolling_return(df_plus1,3)
        ## 
        ## # Calculate rolling 5 year average annual return
        ## rolling_5 <- rolling_return(df_plus1,5)
        ## 
        ## # Calculate rolling 10 year average annual return
        ## rolling_10 <- rolling_return(df_plus1,10)
        ## 
        ## # Plot assets against eachother
        ## pairs(rolling_3)
        ## #pairs(rolling_3[,1:4])
        ## 
        ## pairs(rolling_5)
        ## 
        ## pairs(rolling_10)
        ## 
        ## 
        ## 
        ## 
        ## 
        ## # Plot
        ## dfplot <- rolling_1
        ## ylabel <- "Rolling 1year return"
        ## x <- cbind(dfplot$Year, dfplot$Year, dfplot$Year, dfplot$Year)
        ## y <- cbind(dfplot$roll_schwab_70_30,  dfplot$US_LV,  dfplot$US_SV,  dfplot$Int_Emg)
        ## plot  (x=NULL, y=NULL, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), xlab="Year", ylab=ylabel)
        ## points(x[,1],y[,1], pch=1, lty=1, type="b")
        ## points(x[,2],y[,2], pch=2, lty=2, type="b")
        ## points(x[,3],y[,3], pch=3, lty=3, type="b")
        ## points(x[,4],y[,4], pch=4, lty=4, type="b")
        ## legend("bottomright",legend=c("Schwab 70/30","US_LV","US_SV","Int_Emg"),pch=1:4, lty=1:4)
        ## abline(h=0)
        ## 
        ## # Plot
        ## dfplot <- rolling_5
        ## ylabel <- "Rolling 5 year return"
        ## x <- cbind(dfplot$Year, dfplot$Year, dfplot$Year, dfplot$Year)
        ## y <- cbind(dfplot$roll_schwab_70_30,  dfplot$US_LV,  dfplot$US_SV,  dfplot$Int_Emg)
        ## plot  (x=NULL, y=NULL, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), xlab="Year", ylab=ylabel)
        ## points(x[,1],y[,1], pch=1, lty=1, type="b")
        ## points(x[,2],y[,2], pch=2, lty=2, type="b")
        ## points(x[,3],y[,3], pch=3, lty=3, type="b")
        ## points(x[,4],y[,4], pch=4, lty=4, type="b")
        ## legend("bottomright",legend=c("Schwab 70/30","US_L","US_LV","Int_Emg"),pch=1:4, lty=1:4)
        ## abline(h=0)
        ## 
        ## 
        ## 
        ## 
        ## 
        ## 
        ## 
        ## 
        ## 
        ## 
        ## points(dfplot$Year,dfplot$US_LV, pch=2, lty=2, type="b")
        ## points(dfplot$Year,dfplot$US_SV, pch=3, lty=3, type="b")
        ## points(dfplot$Year,dfplot$US_LG, pch=4, lty=4, type="b")
        ## legend("bottomright",legend=c("Schwab 70/30","US_L","US_LV","US_LG"),pch=1:4, lty=1:4)
        ## abline(h=0)
        ## 
        ## 
        ## plot  (dfplot$Year,dfplot$roll_schwab_70_30, pch=1, lty=1, type="b", xlab="Year", ylab=ylabel)
        ## points(dfplot$Year,dfplot$US_LV, pch=2, lty=2, type="b")
        ## points(dfplot$Year,dfplot$US_SV, pch=3, lty=3, type="b")
        ## points(dfplot$Year,dfplot$US_LG, pch=4, lty=4, type="b")
        ## legend("bottomright",legend=c("Schwab 70/30","US_L","US_LV","US_LG"),pch=1:4, lty=1:4)
        ## abline(h=0)
        ## 
        ## # Plot
        ## dfplot <- rolling_5
        ## ylabel <- "Rolling 5 year return"
        ## plot  (dfplot$Year,dfplot$US_L , pch=1, lty=1, type="b", xlab="Year", ylab=ylabel)
        ## points(dfplot$Year,dfplot$US_LV, pch=2, lty=2, type="b")
        ## points(dfplot$Year,dfplot$US_LG, pch=3, lty=3, type="b")
        ## points(dfplot$Year,dfplot$roll_schwab_70_30, pch=4, lty=4, type="b")
        ## legend("bottomright",legend=c("US_L","US_LV","US_LG","Schwab 70/30"), pch=1:4, lty=1:4)
        ## abline(h=0)
    }
