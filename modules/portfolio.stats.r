portfolio.stats <- function(holding) {
    out <- quantmod::getQuote(holding, src='yahoo',
                              what = quantmod::yahooQF(c('Previous Close',
                                                         'P/E Ratio',
                                                         'Price/EPS Estimate Next Year',
                                                         'Price/Book',
                                                         'EPS',
                                                         '50-day Moving Average',
                                                         '200-day Moving Average')))
    out$'Trade Time' <- NULL
    names(out) <- c('close', names(out[2:ncol(out)]))
    return(out)
}
