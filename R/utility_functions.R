#' Parameter Definitions
#'
#' Defines commonly used parameters.
#'
#' @param r A vector of returns.
#' @param q A vector of Value-at-Risk forecasts.
#' @param e A vector of Expected Shortfall forecasts.
#' @param s A vector of volatility forecasts.
#' @param alpha Scalar probability level in (0, 1).
#' @param B Number of bootstrap samples. Set to 0 to disable bootstrapping.
#' @keywords internal
parameter_definition <- function(r, q, e, s, alpha, B) {}
