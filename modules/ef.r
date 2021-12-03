ef <- function(model='Schwab', from=NA, to=NA, efdata=NA, period='months',
               addline=FALSE, col='black', lty=1, pch=3) {
    ## create Efficient Frontier points to assess TWR vs. risk
    
    ## model = 'Schwab' uses a blend of US L, US S, Inter, Fixed, and Cash
    ##       = 'SSP'    uses a blend of US L           and Fixed

    ## duration defined from one of the following
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

    if (is.na(efdata[1])) {
        ## efdata is not provided so need to get it
        symbol <- c('SPY', 'IWM', 'EFA', 'AGG', 'SHV')
        out  <- equityhistory(symbol, from=from, to=to, period=period)
#        out  <- equityhistory(symbol, period=period)
        twri <- na.omit( out$twr )
        
    } else {
        ## efdata is provided so can use it directly
        twri <- efdata$twri
        from <- zoo::index(twri)[1]
        to   <- zoo::index(twri)[nrow(twri)]
    }
    
#    ## restrict to duration
#    ## xtsrange <- paste('"', noquote(duration[1]), '/', noquote(duration[2]), '"', sep='')
#    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
#    xtsrange
#    twri <- twri[xtsrange]

    if (model == 'test') {
        twri <- head(twri, 4)
        twri[1,] <- c(0.1, 0.11, 0.1, 0.1, 0.1)
        twri[2,] <- c(0.1, 0.12, 0.1, 0.1, 0.1)
        twri[3,] <- c(0.1, 0.13, 0.1, 0.1, 0.1)
        twri[4,] <- c(0.1, 0.14, 0.11, 0.1, 0.1)
    }
        
    ## calculate twrcum and standard deviation
    ## 1st date should have twrcum = 0
    twrcum_apply <- apply(twri, 2, function(x) { prod(x+1) - 1 }) / (twri[1,] + 1)
    twrcum <- t(t(cumprod(twri+1)) / as.vector(twri[1,]+1) - 1)
    twrcum <- tail(twrcum, 1)
    std    <- apply(twri[2:nrow(twri)], 2, sd)

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
    ## eftwr <- weight %*% twrcum          # column vector
    ## colnames(eftwr) <- 'EF TWR'

    ## calculate twri for efficient frontier model
    eftwri <- twri %*% t(weight)              # matrix
    ## turn back into xts
    eftwri <- xts::as.xts( zoo::as.zoo( eftwri, zoo::index(twri)))

    ## calculate cumulative twr and standard deviation
    eftwrcum <- cumprod(eftwri + 1) - 1
    eftwrcuml <- t( xts::last(eftwrcum) )
    ## following also works to just get total cum
    ## eftwrcum <- as.matrix( apply(eftwri, 2, function(x) { prod(x+1) - 1}) )
    colnames(eftwrcuml) <- 'eftwrcum'
    efstd <- as.matrix( apply(eftwri, 2, sd) )  # column vector
    colnames(efstd)  <- 'efstd'

    ef <- as.data.frame( cbind(eftwrcuml, efstd) )
    
    if (isTRUE(addline)) {
      lines(ef$efstd, ef$eftwrcum, type='b', col=col, lty=lty, pch=pch)
    }
    
    return(list(model=model, weight=weight, twri=twri, twrcum=twrcum, std=std,
                eftwri=eftwri, ef=ef))
}
## ef(from='2020-12-31', to='2021-11-11')
