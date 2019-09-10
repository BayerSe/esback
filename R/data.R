#' Returns and risk forecasts for the S&P 500 index
#'
#' A dataset containing the daily log returns and risk forecasts for the S&P 500 index.
#' The quantile and expected shortfall forecasts are for the probability level 2.5\%.
#'
#' Description of the variables:
#' \describe{
#'   \item{r}{Daily log returns from January 3, 2000 to September 29, 2017 (4465 days)}
#'   \item{q}{Value-at-Risk forecasts of the Historical Simulation approach}
#'   \item{e}{Expected shortfall forecasts of the Historical Simulation approach}
#'   \item{s}{Volatility forecasts of the Historical Simulation approach}
#' }
#' @docType data
#' @keywords datasets
#' @name risk_forecasts
#' @usage data(risk_forecasts)
#' @format A data.frame with 4396 rows and 4 variables
NULL
