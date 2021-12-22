equity.twri.test <- function() {
    security <- c('SPY', 'T')
    adjdates <- c('2020-01-29', '2020-02-26', '2020-03-24', '2020-03-31') 
    out <- equity.twri(security, adjdates=adjdates)
    return(out)
}

equity.twri <- function(security, refresh=TRUE, file=NA, period='months', adjdates=NULL) {
    
    ## Returns an xts object with twri for vector of securities.
    ## Defaults returns twri for all available dates.
    ## Period option can be used to select days, weeks, months, or years.
    ## Dates option overrides period and extracts adjusted closing costs for
    ## the vector of dates used to determine twri 
    ## (note: "date" option returns twri for all but the first date)
      
    if (isTRUE(refresh)) {
        ## get incremental twr for each security
        ## twri <- equity.history(security, period='months')  # security[1:50] works, [1:60] does not
        alltwri <- NA
        if (!is.null(adjdates)) {
            ## create xts object of requested dates
            df           <- data.frame(Date=adjdates)
            rownames(df) <- df$Date
            df$keep      <- 1
            df$Date      <- NULL
            adjdates     <- xts::as.xts(df, order.by=as.Date(rownames(df)))
        }
        for (i in 1:length(security)) {
            ## equity.history works for list of 50 symbols but not 60
            ## maybe just do 1 at a time for now to keep simple

            cat('i = ', i, 'security =', security[i], '\n')
            
            if (security[i] == 'Cash') {
                ## cannot download price info  with tiny variation so statistics work later
                ## this will probably bomb if Cash is the first security
                Cash <- rnorm(nrow(alltwri), mean=0, sd=0.00001)
                alltwri <- cbind(alltwri, Cash)
                
            } else {
                if (is.null(adjdates[1])) {
                    ## specific dates not provided so use period
                    new <- equity.history(security[i], period=period)  # 50 works, 60 does not
                    alltwri <- cbind(alltwri, new$twri)     # xts cbind nicely lines up dates
                    
                } else {
                    ## specific dates are provided so use period=days
                    new <- equity.history(security[i], period='days')
                    ## extract adjusted prices
                    adjprice <- new$adjprice[,1]
                    ## combine dates with adjprice
                    adjprice <- cbind(adjprice, adjdates)
                    ## strip unused dates
                    adjprice      <- na.omit(adjprice)
                    ## eliminate placeholer column
                    adjprice$keep <- NULL
                    ## calculate incremental TWR
                    twri  <- adjprice / xts::lag.xts(adjprice, 1) - 1
                    ## remove 1st row since NA
                    twri <- twri[-1,]

                    alltwri <- cbind(alltwri, twri)
                    
                }
            }
        }
        
        ## correct symbols to be consistent with allprice if needed
        symbols <- gsub("\\.|\\/", "-", names(alltwri))
        names(alltwri) <- symbols
        ## strip off 1st column
        alltwri$alltwri <- NULL 
        tail(alltwri)
        
        ## write file of incremental twr values
        if (!is.na(file)) zoo::write.zoo(alltwri, file, sep=',')

    } else {
        ## read historical incremental twr info from file
        alltwri <- readall(file)
        rownames(alltwri) <- alltwri$Index
        alltwri$Index     <- NULL
        alltwri <- xts::as.xts(alltwri)

    }
    
    return(twri=alltwri)
}
## equity.twri(c('SPY', 'AAPL'))
