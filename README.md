# esback

The esback can be used to [backtest](https://en.wikipedia.org/wiki/Backtesting) forecasts of the
[expected shortfall](https://en.wikipedia.org/wiki/Expected_shortfall) risk measure.

## Installation

### CRAN (stable release)

You can install the released version from
[CRAN](https://cran.r-project.org/) via:

    install.packages("esback")


### GitHub (development)

The latest version of the package is under development at [GitHub](https://github.com/BayerSe/esback). 
You can install the development version using these commands:

    install.packages("devtools")
    devtools::install_github("BayerSe/esback", ref = "master")
    
## Implemented Backtests

This package implements the following backtests:

* Expected Shortfall Regression Backtest ([Bayer & Dimitriadis, 2020])
* Exceedance Residuals Backtest ([McNeil & Frey, 2000])
* Conditional Calibration Backtest ([Nolde & Ziegel, 2017])

## Examples

    # Load the esback package
    library(esback)
   
    # Load the data
    data(risk_forecasts)
    
    # Plot the returns and expected shortfall forecasts
    plot(risk_forecasts$r, xlab = "Observation Number", ylab = "Return and ES forecasts")
    lines(risk_forecasts$e, col = "red", lwd = 2)
  
    # Backtest the forecast using the ESR test
    esr_backtest(r = risk_forecasts$r, e = risk_forecasts$e, alpha = 0.025, version = 1)


[McNeil & Frey (2000)]: https://doi.org/10.1016/S0927-5398(00)00012-8
[McNeil & Frey, 2000]: https://doi.org/10.1016/S0927-5398(00)00012-8
[Nolde & Ziegel (2017)]: https://projecteuclid.org/euclid.aoas/1514430265
[Nolde & Ziegel, 2017]: https://projecteuclid.org/euclid.aoas/1514430265
[Bayer & Dimitriadis (2020)]: https://doi.org/10.1093/jjfinec/nbaa013
[Bayer & Dimitriadis, 2020]: https://doi.org/10.1093/jjfinec/nbaa013
