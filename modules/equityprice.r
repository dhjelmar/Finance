equityprice <- function(security, refresh=TRUE, file=NA) {
    
    if (isTRUE(refresh)) {
        ## get current price info for each security
        allprice <- equityinfo(security, extract=c('Name', 'Previous Close'))
        names(allprice) <- c('Date', 'Name', 'Close')
        allprice$Date <- NULL
        ## replace NA closing value for Cash with 1
        ## money markets and individual bonds will also fall into this
        narow <- which(is.na(allprice$Close))
        allprice[narow,]$Close <- 1
        ## rename rows with NA for rowname
        rownames(allprice)[rownames(allprice) == "NA"] <- "Cash" # not a true "NA" so not using is.na()
        ## write prices to file
        if (!is.na(file)) write.csv(allprice, file)

    } else {
        ## read current price info from allprice.csv
        allprice <- read.csv(file)
        rownames(allprice) <- allprice$X
        allprice$X <- NULL
    }
    
    return(allprice)
}
    
