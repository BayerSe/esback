#' Joint Quantile and Expected Shortfall Loss Function
#'
#' Computes the joint (VaR, ES) loss. Defaults to the 0-homogeneous variant.
#'
#' @inheritParams parameter_definition
#' @param g1 1, 2
#' @param g2 1, 2, 3, 4, 5
#' @param return_mean If TRUE returns the average loss, else the individual values
#' @examples
#' data(risk_forecasts)
#' joint_loss(r=risk_forecasts$r, q=risk_forecasts$q, e=risk_forecasts$e, alpha=0.025)
#' @references \href{http://dx.doi.org/10.1214/16-AOS1439}{Fissler & Ziegel (2016)}
#' @export
joint_loss <- function(r, q, e, alpha, g1 = 2L, g2 = 1L, return_mean = TRUE) {
  n <- length(r)
  if (g1 == 1) {
    G1_r <- r
    G1_q <- q
  } else if (g1 == 2) {
    G1_r <- rep(0, n)
    G1_q <- rep(0, n)
  }
  if (g2 == 1) {
    G2_curly_e <- -log(-e)
    G2_e <- -1/e
  } else if (g2 == 2) {
    G2_curly_e <- -sqrt(-e)
    G2_e <- 0.5/sqrt(-e)
  } else if (g2 == 3) {
    G2_curly_e <- -1/e
    G2_e <- 1/e^2
  } else if (g2 == 4) {
    G2_curly_e <- log1p(exp(e))
    G2_e <- 1 / (1 + exp(-e))
  } else if (g2 == 5) {
    G2_curly_e <- exp(e)
    G2_e <- exp(e)
  }
  loss <- ((r <= q) - alpha) * (G1_q - G1_r) +
    G2_e * (e - q + (q - r) * (r <= q)/alpha) - G2_curly_e

  if (return_mean) {
    mean(loss)
  } else {
    loss
  }
}
