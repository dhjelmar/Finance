twri.adjust <- function(xts, d2m=FALSE, dates.new=NA, twri.input=TRUE) {

    ## ADJUST DATES FOR XTS OBJECT TO CLOSEST DATES THE MARKET IS OPEN

    ## Input: xts = XTS object (does not have to be an XTS of twri values)
    ##        align = TRUE (default) aligns xts dates to closest market dates
    ##        d2m = FALSE (default) does not attempt to convert xts column values
    ##            = TRUE assumes xts object contains twri values and converts from days to months
    ##        dates.new = NA (default) option is ignored
    ##                  = vector of new, more coarse dates to align xts object to rather than months
    ##                    (d2m is ignored in this case)

    ## Output: xts object with potentially new dates and potentially new values

    ## extract dates from xts object
    xtsdates <- zoo::index(xts)

    ## determine the dates the market is open
    out <- equity.history('SPY', period='days')
    
    ## function to find the closest match
    closest <- function(x, findin=out$market.open.days) {
        ## market.open[findInterval(as.Date('2021-01-31'), as.Date(market.open))]
        findin[findInterval(as.Date(x), as.Date(findin))]
    }
    ## closest('2021-01-31')
    
    ## apply function to find closest match
    adjust <- unlist( purrr::pmap(list(x = xtsdates), function(x) closest(x)) )
    zoo::index(xts) <- as.Date(adjust, origin='1970-01-01')
        
    ##-----------------------------------------------------------------------------
    if (isTRUE(d2m) | !is.na(dates.new[1])) {
        ## convert to months or provided dates

        ## number of columns in original xts object
        xts.cols <- ncol(xts)

        ## define range of dates in input xts object
        xts.range <- paste(xtsdates[1], '/', xtsdates[length(xtsdates)], sep='')
        
        if (is.na(dates.new[1])) {
            ## create xts object from market open months for the input xts range
            xts.open <- xts.create(out$market.open.months, 1)[xts.range]
        } else {
            ## create xts object from provided dates for the input xts range
            xts.open <- xts.create(dates.new, 1)[xts.range]
            ## narrow the range of the input xts object if it is wider than for xts.open
            xts.range.open <- paste(zoo::index(xts.open[1]), '/',
                                    zoo::index(xts.open[nrow(xts.open)]), sep='')
            xts <- xts[xts.range.open]
        }
            
        ## combine the two xts objects
        xts.names      <- names(xts)
        xts.open.names <- names(xts.open)
        xts <- cbind(xts, xts.open)
        names(xts) <- c(xts.names, xts.open.names)

        if (isFALSE(twri.input)) {
            ## input xts object is not twri so do elmininate dates not at month ends
            ## but do not modify the values themselves
            xts <- xts[xts$xts.created == 1, 1:xts.cols]
            
        } else {
            ## input xts object is twri so need to adjust month end values
            ## if eliminate an intermediate twri
            
            ## consider time t in xts object
            xts.elim <- rep(0, xts.cols)
            for (t in 1:nrow(xts)) {

                if (is.na(xts[t, xts.cols+1])) {
                    ## need to eliminate this time and add twri to next twri for each column
                    for (h in 1:xts.cols) {
                        xts.elim[h] <- (xts.elim[h] + 1) * (xts[t, h] + 1) - 1
                    }
                    xts.elim <- as.numeric(xts.elim)
                    ## cat(t, xts[t,], xts.elim,'\n')

                } else {
                    ## calculate a new xts if needed for each holding h
                    for (h in 1:xts.cols) {
                        ## No change if prior time was not NA (i.e., xts.elim=0)
                        xts[t, h] <- (xts.elim[h] + 1) * (xts[t, h] + 1) - 1
                    }
                    ## reset xts.elim
                    xts.elim <- rep(0, xts.cols)
                    ## cat(t, xts[t,], xts.elim,'\n')
                }
            }
            xts <- xts[xts$xts.created == 1, 1:xts.cols]
        }
        
    }
    
    return(xts)

}

## # a and a.adj should be identical
## a.data <- '
## CCF     CER     CTR
## 2019-12-31  0.0258  0.0212  0.0305
## 2020-01-31 -0.0117 -0.0146 -0.0124
## 2020-02-28 -0.0738 -0.0493 -0.0566
## 2020-03-31 -0.1758 -0.1210 -0.1282
## 2020-04-30  0.1128  0.0659  0.0701
## '
## a <- xts::as.xts( readall(a.data) )
## a.adj <- twri.adjust(a, d2m=TRUE)
## a
## a.adj


## ## b has an extra, mid-month twri
## b.data <- '
##                   CCF         CER         CTR
## 2019-12-31  0.0258000  0.02120000  0.03050000
## 2020-01-31 -0.0117000 -0.01460000 -0.01240000
## 2020-02-20  0.0635000  0.01900000  0.02780000
## 2020-02-28 -0.1291020 -0.06702650 -0.08211714
## 2020-03-23 -0.2945000 -0.19400000 -0.21920000
## 2020-03-31  0.1682495  0.09057072  0.11654713
## 2020-04-30  0.1128000  0.06590000  0.07010000
## '
## b <- xts::as.xts( readall(b.data) )
## b <- twrsheet['2019-12-31/2020-04', 1:3]
## b.adj <- twri.adjust(b, d2m=TRUE)
## b.twrc     <- cumprod(b     + 1) - 1 
## b.adj.twrc <- cumprod(b.adj + 1) - 1
## diff <- tail(b.adj.twrc,1) - tail(b.twrc,1)
## if (sum(abs(diff)) < 0.0001) {
##     cat('Verification checks out\n')
##     print(diff)
## } else {
##     cat('Verification error\n')
##     print(diff)
## }


## cc <- equity.twri('SPY', period='days')['2019-12/2020']
## cc.adj <- twri.adjust(cc, d2m=TRUE)
## cc.twrc     <- twrc.calc(cc    , zero.from = FALSE)
## cc.adj.twrc <- twrc.calc(cc.adj, zero.from = FALSE)
## tail(cc.twrc)
## tail(cc.adj.twrc)
## comb <- cbind(cc.twrc, cc.adj.twrc)
## plot(zoo::na.approx(comb),screens=1,auto.legend=TRUE)


## ## adjust days to some alternate set of dates (i.e., not just month ends)
## fine <- equity.twri('SPY', period='days')['2019-12/2020']
## dates.coarse <- c('2019-12-31', '2020-01-31', '2020-02-20', '2020-02-28',
##                   '2020-03-23', '2020-03-31', '2020-04-30 ')
## coarse       <- twri.adjust(fine, dates.new = dates.coarse)
## ## check that both have same twrc for same duration
## xtsrange <- paste(dates.coarse[1], '/', dates.coarse[length(dates.coarse)], sep='')
## xtsrange <- '2019-12-31/2020-01-31'
## fine[xtsrange]
## coarse[xtsrange]
## fine.twrc    <- cumprod(fine[xtsrange]   + 1) - 1 
## coarse.twrc  <- cumprod(coarse[xtsrange] + 1) - 1
## diff <- tail(coarse.twrc,1) - tail(fine.twrc,1)
## if (sum(abs(diff)) < 0.0001) {
##     cat('Verification checks out\n')
##     print(diff)
## } else { 
##     cat('Verification error\n')
##     print(diff)
## }
