# Finance

# Example uses of modules
- portfolio_statistics_example1.r = simple example for a portfolio with a single account with holdings and weights input as vector
- portfolio_statistics.r          = complex example for multiple accounts that make up a portfolio; holdings and quantities read from Excel

# Primary modules
- portfolio.eval = options to plot following for all holdings, the overall portfolio, and a benchmark:
-- twri (incremental TWR)
-- twrcum (cumulative TWR)
-- risk/return plot (i.e., twrcum vs. standard deviation; not annualized yet)
-- alpha vs. beta

- equity.eval = options to plot following for one holding and benchmark
-- twri 
-- twrcum
-- twri for equity vs. twri for baseline to obtain alpha and beta


# Supporting modules
- equity.twri = pulls twri values for many holdings into xml object (uses equity.history)
- equity.history = calculates incremental TWR (twri) and cumulative TWR (twrcum) from Yahoo using quantmod::getSymbols() and stores in XML object
- ef = adds effective frontier line to a risk/return plot
- equity.history used to obtain twrcum
- alpha.beta = determine alpha and beta and create plot for single holding
