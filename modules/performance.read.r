performance.read <- function() {
    os <- .Platform$OS.type

    ##-----------------------------------------------------------------------------
    ## READ DATA AS TIBBLE
    if (os == 'unix') {
        filename <- 'performance_data_example.xlsx'
        map        <- NA
        valuesheet <- readall(filename, sheet = 'value', header.row=5, data.start.row=7, rename=FALSE)
        twrsheet   <- readall(filename, sheet = 'TWR',   header.row=5, data.start.row=7, rename=FALSE)
    } else {
        source('modules/path.fdata.r')
        filename <- paste(path.fdata, 'performance_data.xlsx', sep='')
        map        <- readall(filename, sheet = 'Map',    header.row=3)
        valuesheet <- readall(filename, sheet = 'valueR', header.row=5, data.start.row=7, rename=FALSE)
        twrsheet   <- readall(filename, sheet = 'TWRR',   header.row=5, data.start.row=7, rename=FALSE)
    }
    names(valuesheet) <- c('Date', names(valuesheet)[2:length(names(valuesheet))])
    names(twrsheet)   <- c('Date', names(twrsheet)[2:length(names(twrsheet))])
    
    
    ## strip off ending rows that with NA for date
    na.rows <- which(is.na(valuesheet$Date))
    if (sum(na.rows) > 0) valuesheet <- valuesheet[1:(na.rows[1]-1),]
    na.rows <- which(is.na(twrsheet$Date))
    if (sum(na.rows) > 0) twrsheet <- twrsheet[1:(na.rows[1]-1),]

    ## CONVERT tibble TO XTS
    ## first change to dateframe so can set rownames as dates
    ## could convert directly to XTS but then, since "Date" is character, 
    ## all matrix columns would be character
    ##-----------------------------------------------------------------------------
    valuesheet           <- as.data.frame(valuesheet)
    rownames(valuesheet) <- valuesheet$Date
    valuesheet$Date      <- NULL
    valuesheet           <- xts::as.xts(valuesheet, order.by=as.Date(rownames(valuesheet)))

    twrsheet           <- as.data.frame(twrsheet)
    rownames(twrsheet) <- twrsheet$Date
    twrsheet$Date      <- NULL
    twrsheet           <- xts::as.xts(twrsheet, order.by=as.Date(rownames(twrsheet)))


    ##-----------------------------------------------------------------------------
    ## ADJUST ANY MONTH END DATES TO DATES THE MARKET IS OPEN
    ## Needed if excel list of twri values uses month ends rather than last market day in each month
    out <- equity.history('SPY', period='days')
    market.open <- as.Date( zoo::index(out$close) )

    valuedates <- as.Date( zoo::index(valuesheet) )
    twrdates   <- zoo::index(twrsheet)

    closest <- function(x, range=market.open) {
        ## market.open[findInterval(as.Date('2021-01-31'), as.Date(market.open))]
        market.open[findInterval(as.Date(x), as.Date(market.open))]
    }
    ## closest('2021-01-31')

    adjust <- unlist( purrr::pmap(list(x = valuedates), function(x) closest(x)) )
    zoo::index(valuesheet) <- as.Date(adjust, origin='1970-01-01')

    adjust <- unlist( purrr::pmap(list(x = twrdates), function(x) closest(x)) )
    dates  <- as.Date(adjust, origin='1970-01-01')
    zoo::index(twrsheet) <- dates


    ##-----------------------------------------------------------------------------
    return(list(map=map, valuesheet=valuesheet, twrsheet=twrsheet, market.open=market.open))

}
