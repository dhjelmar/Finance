equitytwr <- function(security, refresh=TRUE, file=NA, period='months') {
    
    if (isTRUE(refresh)) {
        ## get twr for each security
        ## twr <- equityhistory(security, period='months')  # security[1:50] works, [1:60] does not
        alltwr <- NA
        for (i in 1:length(security)) {
            ## equityhistory works for list of 50 symbols but not 60
            ## maybe just do 1 at a time for now to keep simple
            cat('i = ', i, 'security =', security[i], '\n')
            if (security[i] == 'Cash') {
                ## cannot download price info  with tiny variation so statistics work later
                Cash <- rnorm(nrow(alltwr), mean=0, sd=0.00001)
                alltwr <- cbind(alltwr, Cash)
            } else {
                new <- equityhistory(security[i], period=period)  # 50 works, 60 does not
                alltwr <- cbind(alltwr, new$twr)     # xts cbind nicely lines up dates
            }
        }
        ## correct symbols to be consistent with allprice if needed
        symbols <- gsub("\\.|\\/", "-", names(alltwr))
        names(alltwr) <- symbols
        ## strip off 1st column
        alltwr$alltwr <- NULL 
        tail(alltwr)
        
        ## write file of twr values
        if (!is.na(file)) zoo::write.zoo(alltwr, file, sep=',')

    } else {
        ## read historical twr info from file
        alltwr <- readall(file)
        rownames(alltwr) <- alltwr$Index
        alltwr$Index     <- NULL
        alltwr <- xts::as.xts(alltwr)

    }
    
    return(twr=alltwr)
}
