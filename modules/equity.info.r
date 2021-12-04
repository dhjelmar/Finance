equity.info <- function(symbol, extract=NULL) {
    
    if (is.null(extract)) {
        ## interactive with list of info available
        out <- quantmod::getQuote(symbol, src='yahoo', what = quantmod::yahooQF())
    } else {
        out <- quantmod::getQuote(symbol, src='yahoo', what = quantmod::yahooQF(extract))
    }
    return(out)
}

## equity.info('SPY')
## equity.info('AAPL', extract='P/E Ratio')
## equity.info('AAPL', extract=c('Name (Long)', 'P/E Ratio', 'Price/EPS Estimate Next Year', 'Price/Book', 'Dividend Yield'))
