twri.adjust <- function(xts, d2m=FALSE) {

    ## ADJUST DATES FOR XTS OBJECT TO CLOSEST DATES THE MARKET IS OPEN

    ## Input: xts = XTS object (does not have to be an XTS of twri values)
    ##        d2m = FALSE (default) does not attempt to convert xts column values
    ##            = TRUE assumes xts object contains twri values and converts from days to months

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
    if (isTRUE(d2m)) {

        ## WORK IN PROGRESS; SOME OF WHAT IS BELOW WILL NOT WORK
        
        ## xts object should contain twri values and option was selected to convert to months
        twri <- xts

        ## number of columns in original xts object
        twri.cols <- ncol(twri)
        
        ## create xts object from market open months for the range of the input xts object
        twri.range <- paste(xtsdates[1], '/', xtsdates[length(xtsdates)], sep='')
        twri.open  <- xts.create(out$market.open.months, 1)[twri.range]

        ## combine the two xts objects
        twri <- cbind(twri, twri.open)

        ## consider time t in xts object
        twri.elim <- rep(0, twri.cols)
        for (t in 1:nrow(twri)) {

            if (is.na(twri[t, twri.cols+1])) {
                ## need to eliminate this time and add twri to next twri for each column
                twri.elim <- as.numeric(twri[t,])
            }

            for (c in 1:twri.cols) {
                ## for each column, make no change if prior time was a month end
                twri[t, c] <- (twri.elim[c] + 1) * (twri[t, c] + 1) - 1
            }
            ## reset twri.elim
            twri.elim <- rep(0, twri.cols)

        }
        twri <- na.omit(twri)
        xts <- twri[, 1:twri.cols]
    }
    
    return(xts)

}
## # a and a.adj should be identical
##  a <- twrsheet[1:9, 1:3]
##  a.adj <- twri.adjust(a, d2m=TRUE)

# b has an extra, mid-month twri
b <- twrsheet['2019-12-31/2020-04', 1:3]
b.adj <- twri.adjust(b, d2m=TRUE)
b.twrc     <- cumprod(b     + 1) - 1 
b.adj.twrc <- cumprod(b.adj + 1) - 1
identical(tail(b.twrc,1), tail(b.adj.twrc,1))    # should return TRUE but still returns FALSE


## out <- equity.history('SPY', period='months')
## adjclose <- out$adjprice['2019-12/2020']
## twri <- equity.twri('SPY', period='months')['2019-12/2020']
## twri.adjust(twri, d2m=TRUE)
