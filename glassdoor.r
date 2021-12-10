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
2018    BCSF    GOOGL   LULU    HUBS    SAP     CRM     DAL     DOCU    LUV     NVDA
2019    BCSF    ZM      GOOGL   LULU    LUV     CRM     ISRG    HUBS    DOCU    PCTY
2020    HUBS    BCSF    DOCU    ISRG    LUV     GOOGL   NVDA    MSFT    LULU    CPT
2021    BCSF    NVDA    HUBS    GOOGL   DAL     LULU    MSFT    META    SYK     DOCU
'

shares <- '
year    hold1   hold2   hold3   hold4   hold5   hold6   hold7   hold8   hold9   hold10
2009    51      235     44      7       34      84      46      26      24      31
2010    151     66      37      68      103     5       56      11      27      46
2011    150     76      112     40      13      94      183     64      40      69
2012    5       407     130     57      65      23      209     79      35      39
2013    6       93      202     26      42      47      38      22      337     47
2014    44      45      61      6       39      51      33      30      42      42
2015    6       24      37      42      51      75      42      35      123     37
2016    54      57      5       127     55      27      34      70      128     137
2017    5       32      31      110     41      47      79      30      72      49
2018    341     5       56      50      42      43      83      112     69      90
2019    364     76      5       39      102     35      31      38      116     80
2020    43      427     90      35      125     5       115     44      30      69
2021    960     83      28      7       279     31      50      715     46      49
'

holding <- readall(holdings)
nhold   <- ncol(holding)-1

## equal weight for each holding (units = fraction of total portfolio value)
weight   <- rep(1/nhold, nhold)

## starting value of portfolio and period for each twri value
value_last <- 10000
period     <- 'months'

## initialize xts objects
twri_list <- NA
twri  <- xts::xts(t(rep(NA, nhold+1)), order.by=as.Date('2008-12-31'))
names(twri) <- c(names(holding[1,2:(nhold+1)]), 'portfolio')
twrc  <- twri
value <- xts::xts(t( c( rep(value_last/nhold, nhold), value_last) ),
                  order.by=as.Date('2008-12-31'))
names(value) <- names(twri)

## evaluate portfolio
for (i in 1:nrow(holding)) {

    ## extract year from holding dataframe
    yeari <- as.character(holding[i,1])
    cat('year = ', yeari, '\n')

    ## obtain xts object of holdings
    holding.yeari <- as.character(holding[i, 2:(nhold+1)])
    twri.yeari <- equity.twri(holding.yeari, period=period)
    twri_list[i]  <- list(twri.yeari)

    ## until list of holdings gets fixed, set NA to zero
    twri.yeari[is.na(twri.yeari)] <- 0
    
    ## calculate twri for portfolio
    twri.yeari$portfolio <- twri.yeari %*% weight
    
    ## cumulative twr for yeari for each holding
    ## 1st date should have twrcum = 0
    twrc.yeari  <- xts::as.xts( t(t(cumprod(twri.yeari+1)) / as.vector(twri.yeari[1,]+1) - 1) )

    ## calculate value of portfolio
    value.yeari <- twrc.yeari * value_last
    
    ## catonate results to those from  prior year
    twri  <- rbind(twri,   twri.yeari[yeari])
    twrc  <- rbind(twrc,   twrc.yeari[yeari])
    value <- rbind(value, value.yeari[yeari])

    ## resest portfolio value to be used for next year purchases and growth
    value_last  <- value.yeari[nrow(value.yeari),]$portfolio[[1]]

}
## remove 1st NA line
twri <- twri[-1,]

## print start of each year to make sure all selected securities are available
start.years <- twri[xts:::startof(twri, "years")]
print(signif(start.years, 3))

## cumulative twr and standard deviation
## 1st date should have twrcum = 0
twrc  <- xts::as.xts( t(t(cumprod(twri+1)) / as.vector(twri[1,]+1) - 1) )
twrcl <- t( xts::last(twrc) )
std     <- as.matrix( apply(twri[2:nrow(twri),], 2, sd, na.rm=TRUE) )

plotxts(twri)
plotxts(twrc)

## the following works but displays a problem in that 
## Yahoo hax not adjusted NVDA for a 4:1 stock split in June 2021
## not sure what other errors are in the Yahoo data
plotspace(2,2)
for (i in 1:nrow(holding)) {
    yeari <- as.character(holding[i,1])
    twri.yeari <- twri_list[[i]][yeari]
    twrc.yeari <- xts::as.xts( t(t(cumprod(twri.yeari+1)) / as.vector(twri.yeari[1,]+1) - 1) )
    plot( plotxts(twrc.yeari) )
}

