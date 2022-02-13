equity.price <- function(security, refresh=TRUE, file=NA) {
    
    if (isTRUE(refresh)) {
      
        ## get current price info for each security
        allprice <- equity.info(security, extract=c('Name', 'Previous Close'))
        names(allprice) <- c('Date', 'Name', 'Close')
        allprice$Date <- NULL
        
        ## Some assets will return NA so search for NA and, if it exists, replace with 1 and label it "Cash"
        ## Cash, money markets and individual bonds also fall into this
        ## rename rows with NA for rowname to "Cash"
        rownames(allprice)[rownames(allprice) == "NA"] <- "Cash" # not a true "NA" so not using is.na()
        ## replace value for 1 unit of "Cash" to 1
        allprice[rownames(allprice) == "Cash", 2] <- 1
        
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
    
