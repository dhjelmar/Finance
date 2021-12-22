twrc.calc <- function(twri, from=NA, to=NA, zero.from=FALSE) {
    if (class(twri)[1] == 'numeric') {
        ## use all twri entries
        twrc <- cumprod(twri + 1) - 1
    } else if (class(twri)[1] == 'matrix') {
        ## use all twri entries
        ## twrc <- cumprod(twri + 1) - 1
        # twrc <- t(t(cumprod(twri + 1))) - 1
        twrc <- apply(twri, 2, function(x) t(t(cumprod(x + 1))) - 1 )
    } else {
        ## twri must be an xts object
        ## use first and/or last twri entries for from and to if not defined
        if (is.na(from)) from <- zoo::index(twri[1,])
        if (is.na(to))   to   <- zoo::index(twri[nrow(twri),])
        ## cumprod works differently on xts than regular matrix so following is a bit complex
        duration <- paste(from, '/', to, sep='')
        if (isTRUE(zero.from)) {
            ## set twrc for from date to zero
            twrc <- xts::as.xts( t(t(cumprod(twri[duration]+1)) / as.vector(twri[from]+1) - 1) )
        } else {
            ## use from date when calculating twrc
            twrc <- xts::as.xts( t(t(cumprod(twri[duration]+1)) - 1) )
        }
    }
    return(twrc)
}
## twrivec1 <- c(0.1, 0.2, 0.3, 0.4, 0.5)
## twrivec2 <- c(0.11, 0.12, 0.13, 0.14, 0.15)
## twrimat  <- cbind(a=twrivec1, b=twrivec2)
## rownames(twrimat) <- c('2020-01-31', '2020-02-28', '2020-03-31', '2020-04-30', '2020-05-31')
## twrixts  <- xts::as.xts(twrimat)
## twrcvec1 <- twrc.calc(twrivec1)
## twrcmat  <- twrc.calc(twrimat)
