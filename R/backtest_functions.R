#' Exceedance Residuals Backtest
#'
#' Tests whether the mean of the exceedance residuals, respectively the
#' mean of the standardized exceedance residuals is zero.
#'
#' @inheritParams parameter_definition
#' @param B Number of bootstrap iterations
#' @return Returns a list with the following components:
#' * pvalue_twosided_simple
#' * pvalue_onesided_simple
#' * pvalue_twosided_standardized
#' * pvalue_onesided_standardized
#' @examples
#' data(risk_forecasts)
#' r <- risk_forecasts$r
#' q <- risk_forecasts$q
#' e <- risk_forecasts$e
#' s <- risk_forecasts$s
#' er_backtest(r = r, q = q, e = e, s = s)
#' @references [McNeil & Frey (2000)](https://doi.org/10.1016/S0927-5398(00)00012-8)
#' @export
#' @md
er_backtest <- function(r, q, e, s = NULL, B = 1000) {
  er_backtest_fun <- function(x) {
    set.seed(1)
    boot_x <- matrix(sample(x, size = length(x) * B, replace = TRUE), nrow = B)
    f <- function(x) mean(x) / stats::sd(x) * sqrt(length(x))
    t0 <- f(x)
    t <- apply(boot_x, 1, f)
    t <- t[is.finite(t) & !is.na(t)]
    c(pv_2s = mean(abs(t - mean(t)) >= abs(t0)),
      pv_1s = mean(t - mean(t) <= t0))
  }

  # Get the backtest p-values
  er_simple <- er_backtest_fun((r - e)[r <= q])
  if (!is.null(s)) {
    er_standardized <- er_backtest_fun(((r - e) / s)[r <= q])
  } else {
    er_standardized <- c(NA, NA)
  }

  # Return results
  ret <- list(
    pvalue_twosided_simple = unname(er_simple[1]),
    pvalue_onesided_simple = unname(er_simple[2]),
    pvalue_twosided_standardized = unname(er_standardized[1]),
    pvalue_onesided_standardized = unname(er_standardized[2])
  )
  ret
}


#' Conditional Calibration Backtest
#'
#' The simple and general conditional calibration backtests of
#' [Nolde & Ziegel (2007)](https://projecteuclid.org/euclid.aoas/1514430265).
#'
#' @inheritParams parameter_definition
#' @param hommel If TRUE, use Hommels correction,
#' otherwise use the classical Bonferroni correction.
#' @return Returns a list with the following components:
#' * pvalue_twosided_simple
#' * pvalue_onesided_simple
#' * pvalue_twosided_general
#' * pvalue_onesided_general
#' @examples
#' data(risk_forecasts)
#' r <- risk_forecasts$r
#' q <- risk_forecasts$q
#' e <- risk_forecasts$e
#' s <- risk_forecasts$s
#' cc_backtest(r = r, q = q, e = e, s = s, alpha = 0.025)
#' @references [Nolde & Ziegel (2007)](https://projecteuclid.org/euclid.aoas/1514430265)
#' @export
#' @md
cc_backtest <- function(r, q, e, s=NULL, alpha, hommel=TRUE) {
  # Sample length
  n <- length(r)

  # Identification function matrix (n x 2)
  V <- cbind(alpha - (r <= q),
             e - q + (r <= q) * (q - r) / alpha)

  # Test functions; n x q x 2
  H1 <- aperm(replicate(n, diag(2)))  # q = 2
  if (!is.null(s)) {
    H2 <- array(cbind((q - e) / alpha / s, 1 / s), dim = c(n, 1, 2))  # q = 1
    H3 <- array(NA, dim = c(n, 4, 2))  # q = 4
    H3[,, 1] <- cbind(1, abs(q), 0, 0)
    H3[,, 2] <- cbind(0, 0, 1, 1 / s)
  }

  # n x q matrices of h * V
  hV1 <- apply(H1, 2, function(x) rowSums(x * V))
  if (!is.null(s)) {
    hV2 <- apply(H2, 2, function(x) rowSums(x * V))
    hV3 <- apply(H3, 2, function(x) rowSums(x * V))
  }

  # Estimates of the covariance matrix of h * V
  omega1 <- crossprod(hV1) / n
  if (!is.null(s)) {
    omega2 <- crossprod(hV2) / n
    omega3 <- crossprod(hV3) / n
  }

  # Test statistics and p-values
  t1 <- as.numeric(n * colMeans(hV1) %*% solve(omega1) %*% colMeans(hV1))
  t3 <- sqrt(n) * diag(omega1)^(-1/2) * colMeans(hV1)
  p1 <- 1 - stats::pchisq(t1, 2)
  p3 <- ifelse(hommel,
               min(2 * sum(1 / (1:2)) * min(sort(1 - stats::pnorm(t3)) / (1:2)), 1),
               min(2 * min(1 - stats::pnorm(t3)), 1))
  if (!is.null(s)) {
    t2 <- as.numeric(n * colMeans(hV2) %*% solve(omega2) %*% colMeans(hV2))
    t4 <- sqrt(n) * diag(omega3)^(-1/2) * colMeans(hV3)
    p2 <- 1 - stats::pchisq(t2, 1)
    p4 <- ifelse(hommel,
                 min(4 * sum(1 / (1:4)) * min(sort(1 - stats::pnorm(t4)) / (1:4)), 1),
                 min(4 * min(1 - stats::pnorm(t4)), 1))
  } else {
    p2 <- NA
    p4 <- NA
  }

  # Return results
  ret <- list(
    pvalue_twosided_simple = p1,
    pvalue_onesided_simple = p3,
    pvalue_twosided_general = p2,
    pvalue_onesided_general = p4
  )
  ret
}


#' Expected Shortfall Regression Backtest
#'
#' This function implements multiple expected shortfall regression (esreg)
#' based backtests.
#' Using the `version` argument, the following backtests are available:
#' 1. ("Strict ESR") Regresses the returns on the expected shortfall forecasts
#'     and tests the ES coefficients for (0, 1).
#' 1. ("Auxiliary ESR") Regresses the returns on the quantile and the expected shortfall forecasts
#'    and tests the ES coefficients for (0, 1).
#' 1. ("Strict Intercept") Tests whether the expected shortfall of the forecast error r - e is zero.
#'
#' @inheritParams parameter_definition
#' @param version Version of the backtest to be used
#' @param cov_config a list with three components: sparsity, sigma_est, and misspec, see \link[esreg]{vcovA}
#' @return Returns a list with the following components:
#' * pvalue_two_sided_asymptotic
#' * pvalue_one_sided_asymptotic (for version 3)
#' * pvalue_two_sided_bootstrap
#' * pvalue_one_sided_bootstrap (for version 3)
#' @examples
#' data(risk_forecasts)
#' r <- risk_forecasts$r
#' q <- risk_forecasts$q
#' e <- risk_forecasts$e
#' esr_backtest(r = r, q = q, e = e, alpha = 0.025, version = 1)
#' @references
#' @references [Bayer & Dimitriadis (2020)](https://doi.org/10.1093/jjfinec/nbaa013)
#' @export
#' @md
esr_backtest <- function(r, q, e, alpha, version, B = 0,
                         cov_config=list(sparsity='nid', sigma_est='scl_sp', misspec=TRUE)) {
  if (missing(q) & version %in% c(2)) {
    stop('You need to supply VaR forecast `q` for backtest version ', version)
  }

  if (missing(q)) {
    data <- data.frame(r = r, e = e)
  } else {
    data <- data.frame(r = r, q = q, e = e)
  }

  # Set the details for the selected version of the backtest
  if (version == 1) {
    model <- r ~ e
    h0 <- c(NA, NA, 0, 1)
    one_sided <- FALSE
  } else if (version == 2) {
    model <- r ~ q | e
    h0 <- c(NA, NA, 0, 1)
    one_sided <- FALSE
  } else if (version == 3) {
    model <- I(r - e) ~ e | 1
    h0 <- c(NA, NA, 0)
    one_sided <- TRUE
  } else {
    stop('This is a non-supported backtest version!')
  }

  # Fit the model
  fit0 <- esreg::esreg(model, data = data, alpha = alpha, g1 = 2, g2 = 1)
  cov0 <- esreg::vcovA(fit0,
                       sparsity = cov_config$sparsity,
                       sigma_est = cov_config$sigma_est,
                       misspec = cov_config$misspec)
  s0 <- fit0$coefficients - h0
  mask <- !is.na(h0)

  # Compute the asymptotic test statistic and p-value
  if (version %in% c(1, 2)) {
    t0 <- as.numeric(s0[mask] %*% solve(cov0[mask, mask]) %*% s0[mask])
    pv0_1s <- NA
    pv0_2s <- 1 - stats::pchisq(t0, sum(mask))
  } else if (version %in% c(3)) {
    t0 <- as.numeric(s0[mask] / sqrt(cov0[mask, mask]))
    pv0_1s <- stats::pnorm(t0)
    pv0_2s <- 2 * (1 - stats::pnorm(abs(t0)))
  }

  # Compute the bootstrap p-values
  if (B > 0) {
    n <- length(r)
    idx <- matrix(sample(1:n, n*B, replace=TRUE), nrow=n)
    bs_estimates <- apply(idx, 2, function(id) {
      tryCatch({
        fitb <- esreg::esreg(model, data = data[id,], alpha = alpha, g1 = 2, g2 = 1, early_stopping = 0)
        sb <- fitb$coefficients - fit0$coefficients
        covb <- esreg::vcovA(fitb,
                             sparsity = cov_config$sparsity,
                             sigma_est = cov_config$sigma_est,
                             misspec = cov_config$misspec)
        list(sb = sb, covb = covb)
      }, error=function(e) NA)
    })
    idx_na <- is.na(bs_estimates)
    share_na <- mean(idx_na)
    if (share_na >= 0.05) stop('More than 5% of the bootstrap replications failed!')

    bs_estimates <- bs_estimates[!idx_na]

    if (version %in% c(1, 2)) {
      tb <- sapply(bs_estimates, function(x) {
        as.numeric(x$sb[mask] %*% solve(x$covb[mask, mask]) %*% x$sb[mask])
      })
      tb <- tb[!is.na(tb)]
      pvb_2s <- mean(tb >= t0)
      pvb_1s <- NA
    } else if (version %in% c(3)) {
      tb <- sapply(bs_estimates, function(x) {
        x$sb[mask] / sqrt(x$covb[mask, mask])
      })
      tb <- tb[!is.na(tb)]
      pvb_2s <- mean(abs(t0) <= abs(tb))
      pvb_1s <- mean(tb <= t0)
    }
  } else {
    pvb_2s <- NA
    pvb_1s <- NA
  }

  # Return results
  ret <- list(
    pvalue_twosided_asymptotic = pv0_2s,
    pvalue_twosided_bootstrap = pvb_2s
  )

  if (version %in% c(3)) {
    ret['pvalue_onesided_asymptotic'] <- pv0_1s
    ret['pvalue_onesided_bootstrap'] <- pvb_1s
  }

  ret
}
