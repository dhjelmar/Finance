prep <- function(portfolio) {

    ## input: portfolio = vector of names in portfolio (as named in twrsheet)

    ## passive input (lazy since I am not passing everything needed in function call):
    ##         twrsheet   = XTS object with twri  for all portfolio holdings (can contain more)
    ##         valuesheet = XTS object with value for all portfolio holdings (can contain more)

    ## output: twri  = XTS object of twri  values for portfolio
    ##         twrib = XTS object of twrti values for options for baseline
    ##         efdata = efficient frontier data
    ##         value = value of each portfolio holding


    ##-----------------------------------------------------------------------------
    ## pull twri from twrsheet
    twri <- twrsheet[, (colnames(twrsheet) %in% tidyselect::all_of(portfolio))]
    ## adjust twri to market days
    twri <- twri.adjust(twri)

    ##-----------------------------------------------------------------------------
    ## get effective frontier data
    efdata <- ef(model='Schwab', period='days', addline=FALSE)
    ## adjust dates to same as defined in twri
    efdata$twri <- twri.adjust(efdata$twri, dates.new = zoo::index(twri))

    ##-----------------------------------------------------------------------------
    ## define benchmarks
    twrib  <- (efdata$eftwri$schwab_60_40 + efdata$eftwri$schwab_80_20) / 2
    names(twrib) <- 'Schwab_70_30'
    twrib <- twri.adjust(twrib, dates = zoo::index(twri))

    ## add CPI+5% as a benchmark if exists inside twrsheet
    if (which(grepl('CPI', names(twrsheet))) > 0) {
        ## twri for CPI (consumer price index) provided for plotting
        cpi          <- twri.adjust( twrsheet$CPI )
        twrib        <- cbind(twrib, cpi)
        p5           <- 0
        twrib$CPI.p5 <- 0
        date <- as.numeric( zoo::index(twrib) )
        for (i in 2:nrow(twrib)) {
            ## add 5% to CPI
            elapsed <- date[i] - date[i-1]
            p5[i]   <- 0.05 / (365.25 / elapsed)
            twrib$CPI.p5[i] <- twrib$CPI[i] + p5[i]
            ## twrc.cpi <- twrc.calc(twri.cpi, zero.from=TRUE)
        }
        ## assume 1st p5 entry is same as the 2nd
        twrib$CPI.p5[1] <- twrib$CPI[1] + p5[2]
        ## eliminate the CPI column
        twrib$CPI <- NULL
        ## rename 'CPI.p5' to 'CPI+5%'
        ## the following works but the complex name gets messed up and replaced down the line
        ## names(twrib) <- stringr::str_replace(names(twrib), 'CPI.p5', 'CPI+5%')
    }


    ##-----------------------------------------------------------------------------
    ## pull value from valuesheet
    value <- valuesheet[, (colnames(valuesheet) %in% tidyselect::all_of(portfolio))]
    ## adjust twri to market days
    value <- twri.adjust(value, twri.input = FALSE)


    ##-----------------------------------------------------------------------------
    ## evaluate portfolio
    ## look at available date range
    print('Available dates:')
    print(zoo::index(twri))

    return(list(twri=twri, twrib=twrib, efdata=efdata, value=value))
    
}
