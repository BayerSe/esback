# esback

The esback package contains function that can be used to evaluate and backtest
[expected shortfall](https://en.wikipedia.org/wiki/Expected_shortfall) (ES) forecasts.

## Installation

### CRAN (stable release)

esback is not on [CRAN](https://cran.r-project.org/) yet.

### GitHub (development)

The latest version of the package is under development at GitHub. 
You can install the development version using these commands:

    install.packages("devtools")
    devtools::install_github("BayerSe/esback"").
    
## Implemented Backtests

* Exceedance Residuals Backtest ([McNeil & Frey, 2000])
* Conditional Calibration Backtest ([Nolde & Ziegel, 2017])
* Expected Shortfall Regression Backtest (Bayer & Dimitriadis, 2017)

While all these tests aim at backtesting the ES, they differ with respect to data
requirements and possible alternative hypotheses.

Some of the tests require, in addition to the ES point forecasts, point predictions of 
the Value-at-Risk and the volatility.

While all backtests can be used to test whether the ES under- or overestimates 
the true risk, some of them can furthermore be used to test the ES against the 
one-sided alternative that the ES is under-estimated.
This is useful from a regulator's point of view, 
as holding more capital than required is not problematic.

The following table provides details on the properties of the tests.

| Backtest                  | Source                     | Requires VaR | Requires Volatility | One Sided Alternative |
|---------------------------|----------------------------|--------------|---------------------|-----------------------|
| ER                        | [McNeil & Frey (2000)]     | x            |                     | x                     |
| Standardized ER           | [McNeil & Frey (2000)]     | x            | x                   | x                     |
| Simple CCT                | [Nolde & Ziegel (2017)]    | x            |                     | x                     |
| General CCT               | [Nolde & Ziegel (2017)]    | x            | x                   | x                     |
| ESR (intercept)           | Bayer & Dimitriadis (2017) |              |                     | x                     |
| ESR (intercept and slope) | Bayer & Dimitriadis (2017) |              |                     |                       |


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

[McNeil & Frey (2000)]: https://doi.org/10.1016/S0927-5398(00)00012-8
[McNeil & Frey, 2000]: https://doi.org/10.1016/S0927-5398(00)00012-8
[Nolde & Ziegel (2017)]: https://arxiv.org/abs/1608.05498
[Nolde & Ziegel, 2017]: https://arxiv.org/abs/1608.05498
