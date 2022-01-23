portfolio.calc.print <- function(df) {
    ## df = output from portfolio.calc
    df <- df$perf
    ## change twrc, std, twrc.ann, and std.ann to character and apply % sign
    df[2:5]   <- sapply(df[2:5], function(x) scales::percent(x, accuracy=0.01))
    df$alpha  <- round(df$alpha , 3)
    df$beta   <- round(df$beta  , 3)
    df$weight <- round(df$weight, 2)
    df$value  <- scales::dollar(df$value)
    df
}
