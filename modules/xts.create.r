xts.create <- function(datevec, value=1, names='xts.created') {
    ## given: datevec = vector of dates (can be character strings or dates)
    ##        value   = single number, vector, or matrix
    ##        names   = single character string if a single number or vector is supplied for value
    ##                  option is not used if a matrix is supplied
    if (class(value) != 'matrix') {
        mat <- matrix(value, 
                      nrow=length(datevec),
                      ncol=length(names))
        colnames(mat) <- names
    }
    zoo <- zoo::as.zoo(mat, as.Date(datevec))
    xts <- xts::as.xts(zoo)
    return(xts)
}
## dates  <- c('2021-11-01', '2021-11-02', '2021-11-03')
## xtsobj <- xts.create(dates, 1)
## class(xtsobj)

## xts.create(dates, c(1,2,3))
## xts.create(dates, c(1,2,3), names='fred')

## a <- c(1,2,3)
## b <- c(4,5,6)
## mat <- cbind(a,b)
## xts.create(dates, mat)
