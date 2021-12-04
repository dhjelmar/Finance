weights <- function(portfolio, portfolioname=NULL) {
    ## input:  portfolio = dataframe with columns labeled "Holding" and "Quantity"
    ## output: portfolio with added columns for "Market_Value" and "Weight"
    
    ##-------------------------------------------------------------------------
    ## COLLAPSE IDENTICAL HOLDINGS (ESPECIALLY FOR COMBINED ACCOUNTS)

    ## combine duplicate holdings
    portfolio <- aggregate(portfolio$Quantity, by=list(portfolio$Holding), FUN=sum)
    names(portfolio) <- c('Holding', 'Quantity')


    ##-------------------------------------------------------------------------
    ## DETERMINE WEIGHT OF EACH HOLDING

    ## obtain closing price data and merge with portfolio
    closeall   <- equity.price(portfolio$Holding)
    closeall$Holding <- rownames(closeall)
    portfolio <- merge(portfolio, closeall, by='Holding')
    portfolio <- select(portfolio, c('Holding', 'Name', 'Quantity', 'Close'))

    ## determine weight
    portfolio$Market_Value <- portfolio$Quantity * portfolio$Close
    totalvalue             <- sum(portfolio$Market_Value)
    portfolio$Weight       <- portfolio$Market_Value / totalvalue

    ## print(portfolio)

    return(portfolio)
}
