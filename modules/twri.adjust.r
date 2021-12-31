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
    closest <- function(x, range=out$market.open.days) {
        ## market.open[findInterval(as.Date('2021-01-31'), as.Date(market.open))]
        market.open[findInterval(as.Date(x), as.Date(market.open))]
    }
    ## closest('2021-01-31')

    ## apply function to find closest match
    adjust <- unlist( purrr::pmap(list(x = xtsdates), function(x) closest(x)) )
    zoo::index(xts) <- as.Date(adjust, origin='1970-01-01')

    ##-----------------------------------------------------------------------------
    if(isTRUE(d2m)) {

        ## WORK IN PROGRESS; SOME OF WHAT IS BELOW WILL NOT WORK
        
        ## xts object should contain twri values and option was selected to convert to months

        for (1 in 1:ncol(xts)) {
            ## first convert twri to adjusted closing values
            adjprice <- (twri + 1) * xts::lag.xts(twri, -1) - 1 # need to test this
            
            ## map to market.open.months then strip out NA values where there is no match
            adjprice <- cbind(adjprice, market.open.months)  # likley need to change market.open.months to XTS first
            adjprice <- na.omit(adjprice)
            adjprice <- adjprice[1:ncol(xts)]
            
            ## convert back to twri
            twri  <- adjprice / xts::lag.xts(adjprice, 1) - 1
            ## remove 1st row since NA
            twri <- twri[-1,]
        }
    }
    
    return(xts)

}
valuesheet <- twri.adjust(valuesheet)$
