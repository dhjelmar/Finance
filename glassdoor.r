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
## options

## look up all holdings and write to file (TRUE) or read from file (FALSE)
refresh <- FALSE

## starting value of portfolio and period for each twri value
value.start <- 10000
period      <- 'months'

## create PDF (TRUE) or plot to screen (FALSE)
createpdf <- FALSE

##-----------------------------------------------------------------------------

holdings <- '
year    hold1   hold2   hold3   hold4   hold5   hold6   hold7   hold8   hold9   hold10
2009    GIS     NFLX    ADBE    GOOGL   SAP     NTAP    INTU    FDS     PG      CAT
2010    LUV     GIS     PG      JNPR    NATI    GOOGL   NTAP    GS      FDS     MDT
2011    LUV     GIS     OSTK    NTAP    GS      NATI    AAPL    ADI     PG      SNPS
2012    GOOGL   REI     AAPL    GIS     CRM     CVX     LUV     NATI    CTXS    QCOM
2013    GOOGL   NATI    LUV     CVX     IT      AKAM    WDAY    CMI     REI     CRM
2014    TWTR    EMN     GWRE    GOOGL   DE      QCOM    CVX     COST    INTU    IT
2015    GOOGL   FFIV    CVX     PG      QCOM    LUV     ADBE    SYK     AAPL    IT
2016    GWRE    HUBS    GOOGL   ZG      EMN     EXPE    ADBE    DAL     AAPL    TWTR
2017    GOOGL   ADBE    CLX     PCTY    SAP     CRM     FORR    INTU    DAL     AKAM
2018    GOOGL   LULU    HUBS    SAP     CRM     DAL     LUV     NVDA    AVB     PCTY  # BCSF and DOCU in top 10 but not public until 11/2018 and 4/2018, respectively
2019    BCSF    GOOGL   LULU    LUV     CRM     ISRG    HUBS    DOCU    PCTY    SAP   # ZM in top 10 but not public until 4/2019
2020    HUBS    BCSF    DOCU    ISRG    LUV     GOOGL   NVDA    MSFT    LULU    CPT
2021    BCSF    NVDA    HUBS    GOOGL   DAL     LULU    MSFT    SYK     DOCU    CRM   # META and KNBE in top 10 but not public until 6/2021 and 5/2021
2022    NVDA    HUBS    BOX     GOOGL   LULU    CRM     RCL     FIVN    TWLO    ADBE
'
holding <- readall(holdings)

## ## work with subset for debugging purposes
## holding <- head(holding[,1:3], 2)

nhold   <- ncol(holding)-1

## equal weight for each holding (units = fraction of total portfolio value)
weight   <- rep(1/nhold, nhold)

## read in twri for portfolio if desired and available
if (isFALSE(refresh)) {
  twri.csv <- readall('glassdoor_twri.csv')
  rownames(twri.csv) <- twri.csv$X
  twri.csv$X <- NULL
  ## change dataframe to xts object
  twri.csv <- xts::as.xts(twri.csv)
  ## set dates consistent with how zoo and xts want them
  zoo::index(twri.csv) <- as.Date( zoo::index(twri.csv))

  twri.rds <- readRDS('glassdoor_twri.rds')
}

## obtain twrib and efdata xts objects
twrib  <- equity.twri('SPY', period=period)
efdata <- ef(model='Schwab', period=period, addline=FALSE)

## initialize xts objects
twri_list  <- NA
## twri    <- NA
twrc_list  <- NA
value_last <- value.start
value      <- NA

## evaluate portfolio
yeari.stop <- 99999
for (i in 1:nrow(holding)) {
  
    ## extract year from holding dataframe
    yeari <- as.character(holding[i,1])
    cat('year = ', yeari, '\n')

    ## starting and ending dates for yeari
    from <- paste(holding[i,1] - 1, '-12-31', sep='')
    to   <- paste(holding[i,1]    , '-12-31', sep='')
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')

    ## obtain xts object of holdings
    holding.yeari <- as.character(holding[i, 2:(nhold+1)])
    if (isTRUE(refresh)) {
        twri.yeari <- equity.twri(holding.yeari, period=period)
    } else {
        twri.yeari <- twri.csv[,1:nhold]   # 1:nhold needed to not include "portfolio"
        names(twri.yeari) <- as.character( holding[i, 2:(nhold+1)] )
    }
    twri.yeari <- twri.yeari[xtsrange]

    if (nrow(twri.yeari) < 3) {
        ## there are fewer than two rows of data for the given year
        ## break out of loop to skip the year 
        ## because 1st row should be from prior year,
        ## and need at least 2 more entries for a standard deviation
        cat('\nSkipping', yeari, '; insufficient number of dates to evaluate.\n')
        yeari.stop <- yeari
        break
    }

    ## calculate twri for portfolio
    twri.yeari$portfolio <- twri.yeari %*% weight

    ## also save twri as a list so retain holding names
    ## may want to drop twri from this loop at some point
    ## and construct it later from twri_list[i], but keeping for now
    twri_list[i]  <- list(twri.yeari)

    ## cumulative twr for yeari for each holding
    ## 1st date should have twrc = 0
    twrc.yeari   <- xts::as.xts( t(t(cumprod(twri.yeari+1)) / as.vector(twri.yeari[1,]+1) - 1) )
    twrc_list[i] <- list(twrc.yeari)

    ## calculate value of portfolio
    ## test:
    ##    matrix <- matrix(rep(1:3,each=5),nrow=3,ncol=5,byrow=TRUE)   # dim 3, 5
    ##    vector <- 1:5                                                # dim 1, 5
    ##    t( t(matrix) * vector )
    value.yeari <- t( t(1 + twrc.yeari) * c(weight,1) * value_last ) # weight for each holding; 1 for portfolio
    value.yeari <- xts::as.xts(value.yeari)
    
    ## resest portfolio value to be used for next year purchases and growth
    value_last  <- value.yeari[nrow(value.yeari),]$portfolio[[1]]

}

## create twri xts object containing all years
##    twri <- data.table::rbindlist(twri_list)  # only works for dataframe
twri <- do.call(rbind, twri_list)  # works for matrices (and xts)
## eliminate 2nd occurance of same times
## (eliminate 2nd since each list starts with end of prior year but for new holdings)
twri <- twri[!duplicated(time(twri)),]
## replace names since each holding changes every year
names(twri) <- c(names(holding)[2:ncol(holding)], 'portfolio')

dates <- zoo::index(twri)
if (isTRUE(refresh)) {
    twridf <- as.data.frame(dates=dates, twri)
    write.csv(twridf, 'glassdoor_twri.csv')

    saveRDS(twri, 'glassdoor_twri.rds')
    saveRDS(twri_list, 'glassdoor_twri_list.rds')
}

## print start of each year to make sure all selected securities are available
start.years <- twri[xts:::startof(twri, "years")]
print(signif(start.years, 3))

## cumulative twr
## 1st date should have twrcum = 0
twrc  <- xts::as.xts( t(t(cumprod(twri+1)) / as.vector(twri[1,]+1) - 1) )
twrcl <- t( xts::last(twrc) )

## calculate standard deviation (only meaningful for the portfolio)
std     <- t( as.matrix( apply(twri[2:nrow(twri),], 2, sd, na.rm=TRUE) ) )

## the following works but displays a problem in that 
## Yahoo hax not adjusted NVDA for a 4:1 stock split in June 2021
## not sure what other errors are in the Yahoo data
## for (i in 1:nrow(holding)) {
##     yeari <- as.character(holding[i,1])
##     twri.yeari <- twri_list[[i]][yeari]
##     twrc.yeari <- xts::as.xts( t(t(cumprod(twri.yeari+1)) / as.vector(twri.yeari[1,]+1) - 1) )
##     plot( plotxts(twrc.yeari) )
## }

if (isTRUE(createpdf)) pdf(file = "glassdoor.pdf", onefile = TRUE,          # creates a multi-page PDF file
                           ## file = "glassdoor%03d.pdf", onefile = FALSE,  # creates multiple PDF files
                           width = 9,  # The width of the plot in inches
                           height = 7) # The height of the plot in inches

## create risk/return plot
portfolioname <- 'Glassdoor'
from  <- '2008-12-31'
to    <- '2021-12-31'
duration <- paste(from, 'to', to, sep=' ')

##----------------------

## want to replace following
out <- portfolio.eval(names(holding)[2:(nhold+1)], weight=weight, twri=twri,
                      twrib='SPY', efdata=efdata,
                      plottype = c('twrc', 'rr', 'twri', 'ab'),
                      from=from, to=to, period=period,
                      main = paste(portfolioname, ': ', duration, 
                                   '; period=', period, sep=''))

## evaluate portfolio as if it was a mutual fund
out <- equity.eval(portfolioname, 'SPY', twri=twri$portfolio, period=period)

##----------------------

## replace above the following (not tested yet; then do similar in loop below)
plotspace(2,2)
twri   <- twrsheet[, (colnames(twrsheet) %in% tidyselect::all_of(portfolio))]
twrib  <- equity.twri('SPY', period='months')
port   <- portfolio.calc(twri[xtsrange], weight=weigh, twrib=twrib[xtsrange])
efdata <- ef(model='Schwab', period='months', addline=FALSE)
portfolio.plot(twri=port$twri, twrc=port$twrc, perf=port$perf, 
               twri.ef=efdata$twri[xtsrange],
               plottype=c('twri', 'ab', 'twrc', 'rra'),
               main=paste('Benchmark = ', names(twrib)[1], sep=''))

## evaluate portfolio as if it was a mutual fund
twri <- port$twri$portfolio
out  <- equity.eval(portfolioname, 'SPY', twri=twri, period='months')

##----------------------

## plot performance during each year of portfolio
for (i in 1:nrow(holding)) {
    yeari <- as.character(holding[i,1])
    if (yeari == yeari.stop) break  # too few rows to evaluate
    cat('Creating plots for year =', yeari, '\n')
    yearim1 <- as.character(holding[i,1]-1)
    symbols <- holding[i,2:(nhold+1)]
    from  <- paste(yearim1, '-12-31', sep='')
    to    <- paste(yeari  , '-12-31', sep='')
    xtsrange <- paste(from, '/', to, sep='')
    twri.yeari <- twri_list[[i]][xtsrange]
    ## twrc.yeari <- xts::as.xts( t(t(cumprod(twri.yeari+1)) / as.vector(twri.yeari[1,]+1) - 1) )
    out <- portfolio.eval(symbols, weight=weight, twri=twri.yeari, twrib=twrib, efdata=efdata,
                          rebalance = 'period',  # should be 'years' but not programmed yet
                          plottype = c('twrc', 'rr', 'twri', 'ab'),
                          from=from, to=to, period=period,
                          main = paste(portfolioname, '; duration =', xtsrange, sep=' '))
}

## collect results
twrc_EOY <- matrix(NA, nrow=nrow(holding), ncol=nhold+2)
colnames(twrc_EOY) <- c(names(holding), 'portfolio')
twrc_EOY_long <- as.data.frame( matrix(NA, nrow=nrow(holding), ncol=3) )
names(twrc_EOY_long) <- c('year', 'holding', 'twrc')
k <- 0
for (i in 1:nrow(holding)) {
    yeari <- as.character(holding[i,1])
    if (yeari == yeari.stop) break  # too few rows to evaluate  
    twrc_EOY[i,] <- c(holding[i,1], as.numeric( tail(twrc_list[[i]], 1) ) )
    for (j in 1:(nhold+1)) {
        k <- k+1
        ##                     year        , holding          , twrc
        twrc_EOY_long[k,] <- c(holding[i,1], names(twrc_list[[i]])[j], twrc_EOY[[i,1+j]])
    }
}
holding
twrc_EOY
twrc_EOY_long$year <- as.numeric(twrc_EOY_long$year)
twrc_EOY_long$twrc <- as.numeric(twrc_EOY_long$twrc)

plotspace(1,1)

barit <- function(df, year) {
    ## create bar chart of holdings kept more than 1 year
    df <- df[df$year >= year,]
    df <- aggregate(df$year, by=list(df$holding), FUN=length)  # FUN=count does not work
    names(df) <- c('holding', 'count')
    df <- df[order(df$count),]
    ## df <- df[df$count > 1,]
    
    ## the following works interactively but skips every other label when write to pdf
    ## barplot(df$count, names.arg=df$holding, las=2)
    
    ## this seems to work for both
    midpts <- barplot(df$count, names.arg=df$holding, xaxt="n")
    text(x=midpts, y=-0.7, df$holding, cex=0.8, srt=90, xpd=TRUE)
    
    return(df)
}
out <- barit(twrc_EOY_long, 2008)
out
out.abc <- out[order(out$holding),]
out.abc
holding.find <- function(symbol, df=holding) {
    df[apply(df, 1, function(r) any(r %in% symbol)),]
}
holding.find('PG')

plotoften <- function(df, year, freq) {
    ## plot holdings held at least freq years since year
    df <- df[df$year >= year,]
    often <- aggregate(df$year, by=list(df$holding), FUN=length)  # FUN=count does not work
    often <- as.character(often[often$x >= freq, 1] )
    df <- df[df$holding %in% tidyselect::all_of(often),]
    plotfit(df$year, df$twrc, df$holding, multifit = TRUE, interval = 'line',
            main=paste('Holdings since', year, 'held at least', freq, 'times', sep=' '))
    return(df)
    }
df <- plotoften(twrc_EOY_long, 2008, 4)

## look at last 1, 3, 5 years for current year holdings
symbols <- as.character( holding[nrow(holding), 2:ncol(holding)] )
## twri <- equity.twri(symbols, period='months')
out <- portfolio.eval(symbols, twri=twri, twrib='SPY', efdata=efdata, from='2020-12-31', to='2021-12-31')
out <- portfolio.eval(symbols, twri=twri, twrib='SPY', efdata=efdata, from='2018-12-31', to='2021-12-31')
out <- portfolio.eval(symbols, twri=twri, twrib='SPY', efdata=efdata, from='2016-12-31', to='2021-12-31')


if (isTRUE(createpdf)) dev.off() # close external pdf (or jpg) file

shinyplot(df, 'year', 'twrc')

## shares <- '
## year    hold1   hold2   hold3   hold4   hold5   hold6   hold7   hold8   hold9   hold10
## 2009    51      235     44      7       34      84      46      26      24      31
## 2010    151     66      37      68      103     5       56      11      27      46
## 2011    150     76      112     40      13      94      183     64      40      69
## 2012    5       407     130     57      65      23      209     79      35      39
## 2013    6       93      202     26      42      47      38      22      337     47
## 2014    44      45      61      6       39      51      33      30      42      42
## 2015    6       24      37      42      51      75      42      35      123     37
## 2016    54      57      5       127     55      27      34      70      128     137
## 2017    5       32      31      110     41      47      79      30      72      49
## 2018    341     5       56      50      42      43      83      112     69      90   # incorrectly assumes BCSF and DOCU
## 2019    364     76      5       39      102     35      31      38      116     80
## 2020    43      427     90      35      125     5       115     44      30      69   # incorrectly assumes ZM
## 2021    960     83      28      7       279     31      50      715     46      49   # incorrectly assumes META
## '
