portfolio.calc.test <- function() {
    holding <- c('SWPPX', 'AGG') 
    from <- '2016-12-30'   # 2016-12-31 is a Saturday
    to   <- '2021-11-30'
    xtsrange <- paste(noquote(from), '/', noquote(to), sep='')
    period  <- 'days'
    twri    <- equity.twri(holding, period=period)
    twrib   <- equity.twri('SPY',   period=period)
    port    <- portfolio.calc(twri=twri[xtsrange],
                              twrib=twrib[xtsrange])
}

portfolio.calc <- function(twri, weight=NA, value=NA, rebalance='period',
                           twrib) {

    ## Minimum input
    ## Given:     twri (incremental TWR (time weighted return)) for holdings
    ##            twrib (twri for benchmark)

    ## Output:    twrc (cumulative TWR)
    ##            twrcb (twrc for benchmark)
    ##            sd (standard deviation)
    ##            alpha
    ##            beta

    ## Options: twri      = xts object with twri for each holding over time
    ##          weight    = vector of weights of each holding in portfolio (sums to 1)
    ##                    = NA = 1/length(holding) if value is not supplied
    ##          value     = xts object with value of each holding over time (optional)
    ##          rebalance = 'no' to let assets grow without rebalancing
    ##                    = 'period' (default) to rebalance every period to same weight
    ##                      (note: this is what ef.r currently assumes)
    ##                    = 'years' to rebalance at end of each year
    ##          twrib     = xts object with twri or benchmark over time

    ## create portfolio twri
    if (class(value)[1] == 'xts') {
        ## check that twri and value have the same dates
       if (!identical(zoo::index(value), zoo::index(twri))) {
            ## problem with input value and twri not being consistent
            cat('\n\n#######################################################\n')
            cat(    '#    WARNING: VALUE AND TWRI DATES NOT CONSISTENT      \n')
            cat(    '#######################################################\n\n')
            return()
        }
        value0      <- value / (twri + 1)                       # values if back out holding twri
        value.port  <- apply(value , 1, sum)                    # sum rows
        value0.port <- apply(value0, 1, sum)                    # sum rows
        twri.port   <- value.port / value0.port - 1
        weight      <- as.numeric( value[nrow(value),] / sum(value[nrow(value),]) )
        
    } else {
        ## value over time is not defined, so use initial weight instead

        ## assume equal weights if not defined
        if (is.na(weight)) weight <- rep(1/length(holding), length(holding))
        
        if (rebalance == 'no') {
            ## let assets grow without rebalancing (i.e., determine new weight after every period)
            ## need to determine weight as a function of time
            ## create empty xts object for value
            ## value.port <- 1
            value      <- xts.create(datevec=zoo::index(twri), value=NA, names=names(twri))
            value.port <- xts.create(datevec=zoo::index(twri), value=NA, names='portfolio')
            value.port[1] <- 1
            value[1,]  <- weight * value.port[[1]]
            twri.port  <- xts.create(datevec=zoo::index(twri), value=0, names='portfolio')
            for (i in 2:nrow(twri)) {
                value[i,]     <- (twri[i,] + 1) * weight * value.port[[i-1]]  # new value for each holding
                value.port[i] <- sum(value[i,])                               # portfolio value
                twri.port[i]  <- value.port[[i]] / value.port[[i-1]] - 1      # portfolio twri
                weight        <- as.vector( value[i,] / value.port[[i]] )     # new weights
            }                

        } else if (rebalance == 'years') {
            ## rebalance at the end of every calendar year (dlh future option)
            cat('\nError: Option "rebalance = \'years\'" not programmed yet\n')
            return()
            
        } else {
            ## rebalance = 'period' (default)
            ## rebalance every period (i.e., hold weight constant throughout the evaluation)
            twri.port <- twri %*% as.matrix(weight)  # %*% specifies matrix multiplication
        }
    }        
    
    ## turn back into xts
    portfolio <- xts::as.xts( zoo::as.zoo( as.matrix(twri.port), zoo::index(twri)))
    colnames(portfolio) <- 'portfolio'
    
    ## combine into single xts object
    twriall <- cbind(twri, portfolio, twrib)

    ## cumulative twr and standard deviation
    ## 1st date should have twrc = 0
    twrc  <- xts::as.xts( t(t(cumprod(twriall+1)) / as.vector(twriall[1,]+1) - 1) )
    twrcl <- t( xts::last(twrc) )
    std   <- as.matrix( apply(twriall[2:nrow(twriall),], 2, sd, na.rm=TRUE) )

    ## calculate risk from standard deviation
    ##    std(xi) = sqrt ( sum(xi-xbar)^2 / N ) for population which seems to be what finance world uses
    ## If have x = monthly TWR and want std for y = yearly TWR
    ## then set F * std(x) = std(y) and solve for F.
    ## If assume yi = 12*xi and give credit for the fact that 1 yearly entry is from 12 measurments, 
    ## then can use Ny = 12*Nm and F = sqrt(12).
    ## std( TWR_monthly - avg_TWR_monthly )
    std.ann  <- std * 12^0.5  

    ## average annual return
    days.held <- as.numeric( sum( diff(zoo::index(twri)) ) )
    twrc.ann  <- (1 + twrcl)^(365.25 / days.held) - 1
    
    ##-----------------------------------------------------------------------------
    ## create dataframe of holdings, portfolio, and benchmark
    out       <- data.frame(colnames(twriall))
    colnames(out) <- 'holding'
    perf  <- cbind(out, twrcl, std, twrc.ann, std.ann)
    names(perf) <- c('holding', 'twrc', 'std', 'twrc.ann', 'std.ann')
    rownames(perf) <- 1:nrow(perf)

    ##-----------------------------------------------------------------------------
    ## add alpha and beta to perf
    ## use first benchmark to determine alpha and beta
    perf$alpha <- NA
    perf$beta  <- NA
    benchi <- as.numeric(twrib[, 1])
    for (i in 1:ncol(twriall)) {
        out <- alpha.beta(as.numeric(twriall[2:(nrow(twriall)-1),i]),
                          benchi[2:(nrow(twriall)-1)],
                          plot=FALSE)
        perf[i,]$alpha <- out$alpha
        perf[i,]$beta  <- out$beta
    }

    
    ## ##-----------------------------------------------------------------------------
    ## ## separate into 3 dataframes
    ## nport <- grep('portfolio', rownames(perf))
    ## perfhold  <- perf[1:(nport-1),]
    ## perfport  <- perf[nport,]
    ## perfbench <- perf[(nport+1):nrow(perf),]
    

    ##-----------------------------------------------------------------------------    
    ## add weights and value to perf
    perf$weight <- c(weight                         , rep(NA, 1+ncol(twrib)))
    perf$value  <- c(as.numeric(value[nrow(value),]), rep(NA, 1+ncol(twrib)))
    
    ##-----------------------------------------------------------------------------
    ## return various values including range of twriall
    xtsrange <- range(zoo::index(twri))
    xtsrange <- paste(xtsrange[1], '/', xtsrange[2], sep='')
    
    return(list(twri = twriall,
                twrc = twrc,
                xtsrange = xtsrange,
                perf = perf))
}

