ef <- function(model='Schwab', from=NA, to=NA, efdata=NA, adjdates=NULL, period='months',
               annualize=TRUE, addline=TRUE, col='black', lty=1, pch=3) {
    ## create Efficient Frontier points to assess TWR vs. risk
    
    ## model = 'Schwab' uses a blend of US L, US S, Inter, Fixed, and Cash
    ##       = 'SSP'    uses a blend of US L           and Fixed

    ## duration defined from one of the following
    ## addline == FALSE:
    ##     if false, then execution is assumed primarily to obtain output
    ##     for subsequent call with ef(symbols, efdata=efdata, from=xx, to=yy)
    ## from, to, and period:
    ##     from   = end of day to start
    ##     to     = end of day to end
    ##     period = 'months' (default), 'days', 'weeks', 'years'
    ## efdata:
    ##     efdata = output from prior execution of ef
    ##              used to extract twri for benchmark so no call needed to yahoo
    ##              efdata$twri needs to contain incremental twr values for:
    ##                  'SPY', 'IWM', 'EFA', 'AGG', 'SHV'
    ##              where:
    ##                  US L  = SPY
    ##                  US S  = IWM (iShares Russel 2000)
    ##                  Inter = EFA (iShares MSCI EAFE of large and mid cap
    ##                               in developed contries excluding US and Canada)
    ##                  Fixed = AGG
    ##                  Cash  = SHV (iShares Short Treasury Bond, < 1 yr)

    ## to just get twri data for subsequent use
    ##    twrief <- ef(period='months', addline=FALSE)$twri
    ## then to create ef line
    ##    ef(model='Schwab', 

    
    if (is.na(efdata[1])) {
        ## efdata is not provided so need to get it
        symbol <- c('SPY', 'IWM', 'EFA', 'AGG', 'SHV')
        if (isFALSE(addline)) {
            ## do not worry about dates and just get data for subsequent call to ef()
            out <- equity.twri(symbol, period=period)
            twri <- na.omit(out)
        } else if (is.null(adjdates)) {
            out  <- equity.history(symbol, from=from, to=to, period=period)
            twri <- na.omit( out$twri )
        } else {
            out <- equity.twri(symbol, adjdates=adjdates)
            twri <- na.omit(out)
        }
        twri_in <- NA
        
    } else {
        ## efdata is provided so can use it directly
        if (class(efdata)[1] == 'xts') {
            ## input efdata is simply an xts object of twri values
            twri_in <- efdata
        } else {
            ## input efdata is full output of prior run of ef()
            twri_in <- efdata$twri
        }
        twri <- twri_in
    }
    
    ## restrict to duration
    if (is.na(from)) from <- zoo::index(twri)[1]
    if (is.na(to))   to   <- zoo::index(twri)[nrow(twri)]
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
    xtsrange
    twri <- twri[xtsrange]

    if (model == 'test') {
        twri <- head(twri, 4)
        twri[1,] <- c(0.1, 0.11, 0.1, 0.1, 0.1)
        twri[2,] <- c(0.1, 0.12, 0.1, 0.1, 0.1)
        twri[3,] <- c(0.1, 0.13, 0.1, 0.1, 0.1)
        twri[4,] <- c(0.1, 0.14, 0.11, 0.1, 0.1)
    }
        
    ## calculate twrc and standard deviation
    ## 1st date should have twrc = 0
    ## twrc_apply <- apply(twri, 2, function(x) { prod(x+1) - 1 }) / (twri[1,] + 1)
    twrc  <- xts::as.xts( t(t(cumprod(twri+1)) / as.vector(twri[1,]+1) - 1) )
    twrcl <- tail(twrc, 1)
    std    <- apply(twri[2:nrow(twri)], 2, sd)

    if (isTRUE(annualize)) {
        
        ## calculate annualized standard deviation from monthly standard deviation
        ##    std(xi) = sqrt ( sum(xi-xbar)^2 / N ) for population which seems to be what finance world uses
        ## If have x = monthly TWR and want std for y = yearly TWR
        ## then set F * std(x) = std(y) and solve for F.
        ## If assume yi = 12*xi and give credit for the fact that 1 yearly entry is from 12 measurments, 
        ## then can use Ny = 12*Nm and F = sqrt(12).
        ## std( TWR_monthly - avg_TWR_monthly )
        std  <- std * 12^0.5  

        ## average annual return
        days.held <- as.numeric(as.Date(to) - as.Date(from))
        twrcl  <- (1 + twrcl)^(365.25 / days.held) - 1
    }
        
    ## define asset class weights for requested benchmark model
    if (model == 'Schwab') {
        ##              US L  US S  Inter Fixed  Cash
        ##              ----- ----- ----- -----  ----
        schwab_95_5  <- c(0.50, 0.20, 0.25, 0.00,  0.05)  # bench - aggressive (95 /  5)
        schwab_80_20 <- c(0.45, 0.15, 0.20, 0.15,  0.05)  # bench - mod agg    (80 / 20)
        schwab_60_40 <- c(0.35, 0.10, 0.15, 0.35,  0.05)  # bench - moderage   (60 / 40)
        schwab_40_60 <- c(0.25, 0.05, 0.10, 0.50,  0.10)  # bench - mod consv  (40 / 60)
        schwab_20_80 <- c(0.15, 0.00, 0.05, 0.50,  0.30)  # bench - consverv   (20 / 80)
        schwab_0_100 <- c(0.00, 0.00, 0.00, 0.40,  0.60)  # bench - short term ( 0 /100)
        weight <- rbind(schwab_95_5, schwab_80_20, schwab_60_40,
                        schwab_40_60, schwab_20_80, schwab_0_100)

    } else if (model == 'test') {
        ##              US L  US S  Inter Fixed  Cash
        ##              ----- ----- ----- -----  ----
        one         <- c(0.2, 0.3,  0.2,  0.2,   0.2)
        two         <- c(0,   1,    0,    1,     0  )
        weight <- rbind(one, two)
        
    } else {
        ##              US L  US S  Inter Fixed  Cash
        ##              ----- ----- ----- -----  ----
        SSP_500     <- c(1.00, 0.00, 0.00, 0.00,  0.00)
        bench_80_20 <- c(0.80, 0.00, 0.00, 0.20,  0.00)
        bench_60_40 <- c(0.60, 0.00, 0.00, 0.40,  0.00)
        bench_40_60 <- c(0.40, 0.00, 0.00, 0.60,  0.00)
        bench_20_80 <- c(0.20, 0.00, 0.00, 0.80,  0.00)
        US_bonds    <- c(0.00, 0.00, 0.00, 1.00,  0.00)
        weight <- rbind(SSP_500, bench_80_20, bench_60_40, bench_40_60, bench_20_80, US_bonds)
    }
    colnames(weight) <- c('US L', 'US S', 'Inter', 'Fixed', 'Cash')
    
    ## ## approximation (not too bad for twr;
    ##                   no good for sd since different benchmarks can compensate for eachother)
    ## ## calculate benchmark twr to define efficient frontier twr
    ## eftwr <- weight %*% twrc          # column vector
    ## colnames(eftwr) <- 'EF TWR'

    ## calculate twri for efficient frontier model
    eftwri <- twri %*% t(weight)              # matrix
    ## turn back into xts
    eftwri <- xts::as.xts( zoo::as.zoo( eftwri, zoo::index(twri)))

    ## calculate cumulative twr and standard deviation
    eftwrc  <- t(t(cumprod(eftwri+1)) / as.vector(eftwri[1,]+1) - 1)
    eftwrcl <- t( xts::last(eftwrc) )
    colnames(eftwrcl) <- 'eftwrc'
    efstd <- as.matrix( apply(eftwri[2:nrow(eftwri),], 2, sd) )  # column vector
    colnames(efstd)  <- 'efstd'

    if (isTRUE(annualize)) {
        ## calculate annualized standard deviation from monthly standard deviation
        eftwrcl <- (1 + eftwrcl)^(365.25 / days.held) - 1
        efstd   <- efstd * 12^0.5  
    }
    
    ef <- as.data.frame( cbind(eftwrcl, efstd) )
    
    if (isTRUE(addline)) {
        ## plot lines for whatever period the data was supplied for
        lines(ef$efstd, ef$eftwrc, type='b', col=col, lty=lty, pch=pch)
    }
    
    return(list(model=model, weight=weight, twri_in=twri_in, twri=twri, twrc=twrc, std=std,
                eftwri=eftwri, ef=ef, from=from, to=to))
}
## ef(from='2020-12-31', to='2021-11-11')
## ef(model='simple', from='2015-12-31', to='2021-11-30')
