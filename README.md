# Finance

## Example use of modules
- portfolio_statistics_example1 = simple example for a portfolio with a single account with holdings and weights input as vector
- portfolio_statistics          = complex example for multiple accounts that make up a portfolio; holdings and quantities read from Excel
- glassdoor = evaluates an idea from github.com/bhjelmar (see glassdoor.pdf output)
   - Note: Performance is calculated from Yahoo adjusted prices which do not always appropriately correct for stock splits. Coding does not correct for this yet.
- performance = Evaluates performance of a portfolio where monthly (in general) TWR values for the accounts in the portfolio are read from Excel (plots value, cumulative TWR, incremental TWR, risk vs. return, and alpha/beta)

## Primary modules
- portfolio.eval = options to plot following for all holdings, the overall portfolio, and a benchmark:
   - twri (incremental TWR) 
   - twrcum (cumulative TWR)
   - risk/return plot (i.e., twrcum vs. standard deviation; not annualized yet)
   - alpha vs. beta

- equity.eval = options to plot following for one holding and benchmark
   - twri 
   - twrcum
   - twri for equity vs. twri for baseline to obtain alpha and beta

## Supporting modules
- equity.twri = pulls twri values for many holdings into xml object (uses equity.history)
- equity.history = calculates incremental TWR (twri) and cumulative TWR (twrcum) from Yahoo using quantmod::getSymbols() and stores in XML object
- ef = adds effective frontier line to a risk/return plot
- equity.history used to obtain twrcum
- alpha.beta = determine alpha and beta and create plot for single holding
