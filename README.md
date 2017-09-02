# esback

This `esback` package contains function that can be used to evaluate and backtest
[expected shortfall](https://en.wikipedia.org/wiki/Expected_shortfall) forecasts.

## Installation

### CRAN (stable release)

`esback` is not on [CRAN](https://cran.r-project.org/) yet.

### GitHub (development)

The latest version of package is under development at GitHub. You can install version from github with:

    install.packages("devtools")
    devtools::install_github('BayerSe/esback').

## Examples

    # Load the esback package
    library(esback)
   
    # Load the data
    data(df)
    
    # Plot the returns and expected shortfall forecasts
    plot(df$r, xlab = "Observation Number", ylab = "Return and ES forecasts")
    lines(df$e1, col = "red")
    lines(df$e2, col = "blue")
    legend("topright", col = c("black", "red", "blue"), 
       lwd = 1, pch = c(1, NA, NA), lty = c(NA, 1, 1), cex = 0.75, 
       legend=c("Return", "APARCH-t", "Historical Simulation"))

    # Backtest the forecasts
    esr_backtest(r = df$r, e = df$e1, alpha = 0.025)
    esr_backtest(r = df$r, e = df$e2, alpha = 0.025)
    
## References

TBA
