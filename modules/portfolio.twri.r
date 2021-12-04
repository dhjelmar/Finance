portfolio.twri <- function(twriall, holding) {
    ## input:   twriall = xts object with twri for many possible holdings
    ##                    index contains dates
    ##                    column for each possible holding symbol
    ##          holding = vector of holding symbols
    ## output:  twri    = xts object with twri for holding symbols only

    ## GET TWRI FOR PORTFOLIO FROM TWRIALL
    ## all_of() function used to silance a warning message
    twri <- twriall[, (colnames(twriall) %in% tidyselect::all_of(holding))]
    times <- zoo::index(twri)

    ## reorder to match order in portfolio dataframe
    ## first convert to dataframe
    twri <- as.data.frame(twri)
    ## reorder to match portfolio
    twri <- select(twri, holding)
    ## convert back to xts
    twri <- xts::as.xts(twri)
    ## above did not recover the exact same timestamp so replacing
    zoo::index(twri) <- times

    return(twri)
}
