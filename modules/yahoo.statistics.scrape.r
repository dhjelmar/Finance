yahoo.statistics.scrape <- function(symbol) {
    
    ## https://stackoverflow.com/questions/40245464/web-scraping-of-key-stats-in-yahoo-finance-with-r
    url <- paste('https://finance.yahoo.com/quote/', symbol,
                 '/key-statistics?p=', symbol, sep='')
    webpage <- readLines(url)
    html             <- XML::htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
    tableNodes       <- XML::getNodeSet(html, "//table")

    valuation        <- XML::readHTMLTable(tableNodes[[1]])
    dates <- names(out$valuation)[2:ncol(out$valuation)]
    names(valuation) <- c('measure', 'current', dates)
    peg <- out$valuation[out$valuation$measure == 'PEG Ratio (5 yr expected) 1',]
    
    price            <- XML::readHTMLTable(tableNodes[[2]])
    share_statistics <- XML::readHTMLTable(tableNodes[[3]])
    dividends_splits <- XML::readHTMLTable(tableNodes[[4]])
    fiscal_year      <- XML::readHTMLTable(tableNodes[[5]])
    profitability    <- XML::readHTMLTable(tableNodes[[6]])
    effectiveness    <- XML::readHTMLTable(tableNodes[[7]])
    income_statement <- XML::readHTMLTable(tableNodes[[8]])
    balance_sheet    <- XML::readHTMLTable(tableNodes[[9]])
    cash_flow        <- XML::readHTMLTable(tableNodes[[10]])

    return(list(valuation        = valuation,
                price            = price,
                share_statistics = share_statistics,
                dividends_splits = dividends_splits,
                fiscal_year      = fiscal_year,
                profitability    = profitability,
                effectiveness    = effectiveness,
                income_statement = income_statement,
                balance_sheet    = balance_sheet,
                cash_flow        = cash_flow,
                peg              = peg))
    
}
## out <- yahoo.statistics.scrape('AAPL')
## peg <- out$valuation[out$valuation$measure == 'PEG Ratio (5 yr expected) 1',]
