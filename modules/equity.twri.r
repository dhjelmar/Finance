equity.twri <- function(security, refresh=TRUE, file=NA, period='months') {
    
    if (isTRUE(refresh)) {
        ## get incremental twr for each security
        ## twri <- equity.history(security, period='months')  # security[1:50] works, [1:60] does not
        alltwri <- NA
        for (i in 1:length(security)) {
            ## equity.history works for list of 50 symbols but not 60
            ## maybe just do 1 at a time for now to keep simple
            cat('i = ', i, 'security =', security[i], '\n')
            if (security[i] == 'Cash') {
                ## cannot download price info  with tiny variation so statistics work later
                Cash <- rnorm(nrow(alltwri), mean=0, sd=0.00001)
                alltwri <- cbind(alltwri, Cash)
            } else {
                new <- equity.history(security[i], period=period)  # 50 works, 60 does not
                alltwri <- cbind(alltwri, new$twri)     # xts cbind nicely lines up dates
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
