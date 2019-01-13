#' Exceedance Residuals Backtest
#'
#' Tests whether the mean of the exceedance residuals, respectively the
#' mean of the standardized exceedance residuals is zero.
#'
#' @inheritParams parameter_definition
#' @param B Number of bootstrap iterations
#' @return Returns a 2x2 matrix with p-values
#' @examples
#' data(risk_forecasts)
#' r <- risk_forecasts$r
#' q <- risk_forecasts$q
#' e <- risk_forecasts$e
#' s <- risk_forecasts$s
#' er_backtest(r = r, q = q, e = e, s = s)
#' @references \href{https://doi.org/10.1016/S0927-5398(00)00012-8}{McNeil & Frey (2000)}
#' @export
er_backtest <- function(r, q, e, s=NULL, B=1000) {
  fun <- function(x) {
    set.seed(1)
    boot_x <- matrix(sample(x, size = length(x) * B, replace = TRUE), nrow = B)
    f <- function(x) mean(x) / stats::sd(x) * sqrt(length(x))
    t0 <- f(x)
    t <- apply(boot_x, 1, f)
    t <- t[is.finite(t) & !is.na(t)]
    c(pv_2s = mean(abs(t - mean(t)) >= abs(t0)),
      pv_1s = mean(t - mean(t) <= t0))
  }


  # Initialize return object
  out <- matrix(NA, 2, 2, dimnames = list(c("Simple", "Standardized"),
                                          c("Two_Sided", "One_Sided")))
  # Store test statistic and p-values
  out[1, ] <- fun(x = (r - e)[r <= q])
  if (!is.null(s)) {
    out[2, ] <- fun(x = ((r - e) / s)[r <= q])
  }

  # Return results
  out
}


#' Conditional Calibration Backtest
#'
#' The simple and general conditional calibration backtests of Nolde & Ziegel (2017).
#'
#' @inheritParams parameter_definition
#' @param hommel If TRUE, use Hommels correction, else use the classical Bonferroni correction.
#' @return Returns a 2x2 matrix with p-values
#' @examples
#' data(risk_forecasts)
#' r <- risk_forecasts$r
#' q <- risk_forecasts$q
#' e <- risk_forecasts$e
#' s <- risk_forecasts$s
#' cc_backtest(r = r, q = q, e = e, s = s, alpha = 0.025)
#' @references\href{https://arxiv.org/abs/1608.05498}{Nolde & Ziegel (2007)}
#' @export
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
  out <- rbind(c(p1, p3), c(p2, p4))
  rownames(out) <- c("Simple", "General")
  colnames(out) <- c("Two_Sided", "One_Sided")
  out
}


#' Expected Shortfall Regression Backtest
#'
#' This function implements multiple expected shortfall regression (esreg)
#' based backtests. Using the `version` argument, the following backtests ar
#' available:
#' \enumerate{
#'   \item Regresses the expected shortfall forecasts on
#'   the returns and tests the ES coefficients for (0, 1).
#'   \item Regresses the quantile and the expected shortfall forecasts on
#'   the returns and tests the ES coefficients for (0, 1).
#'   \item Regresses the quantile and the expected shortfall forecasts on
#'   the returns and tests the coefficients for (0, 1, 0, 1).
#'   \item Tests whether the expected shortfall of the forecast error r - e is zero.
#'   \item Tests whether the expected shortfall of the forecast error 1 - r/e is zero.
#' }
#'
#' @inheritParams parameter_definition
#' @param version Version of the backtest to be used
#' @return Returns a named vector with p-values.
#' @examples
#' data(risk_forecasts)
#' r <- risk_forecasts$r
#' q <- risk_forecasts$q
#' e <- risk_forecasts$e
#' esr_backtest(r = r, q = q, e = e, alpha = 0.025, version = 3)
#' @references Bayer & Dimitriadis (2017)
#' @export
esr_backtest <- function(r, q, e, alpha, version, B = 0) {
  data <- data.frame(r = r, q = q, e = e)

  # Set the details for the selected version of the backtest
  if (version == 1) {
    model <- r ~ e
    h0 <- c(NA, NA, 0, 1)
    g_function <- c(2, 1)
    estimator <- c('iid', 'scl_sp')
    one_sided <- FALSE
  } else if (version == 2) {
    model <- r ~ q | e
    h0 <- c(NA, NA, 0, 1)
    g_function <- c(2, 1)
    estimator <- c('iid', 'scl_sp')
    one_sided <- FALSE
  } else if (version == 3) {
    model <- r ~ q | e
    h0 <- c(0, 1, 0, 1)
    g_function <- c(2, 1)
    estimator <- c('nid', 'scl_sp')
    one_sided <- FALSE
  } else if (version == 4) {
    model <- I(r - e) ~ 1
    h0 <- c(NA, 0)
    g_function <- c(2, 4)
    estimator <- c('iid', 'ind')
    one_sided <- TRUE
  } else if (version == 5) {
    model <- I(1 - r/e) ~ 1
    h0 <- c(NA, 0)
    g_function <- c(2, 4)
    estimator <- c('iid', 'ind')
    one_sided <- TRUE
  }

  # Fit the model
  fit0 <- esreg::esreg(model, data = data,
                       alpha = alpha, g1 = g_function[1], g2 = g_function[2])
  cov0 <- stats::vcov(object = fit0,
                      sparsity = estimator[1], cond_var = estimator[2])
  s0 <- fit0$coefficients - h0
  mask <- !is.na(h0)

  # Compute the asymptotic test statistic and p-value
  if (version %in% c(1, 2, 3)) {
    t0 <- as.numeric(s0[mask] %*% solve(cov0[mask, mask]) %*% s0[mask])
    pv0_1s <- NA
    pv0_2s <- 1 - stats::pchisq(t0, sum(mask))
  } else if (version %in% c(4, 5)) {
    t0 <- s0[mask] / sqrt(cov0[mask, mask])
    pv0_1s <- stats::pnorm(t0)
    pv0_2s <- 2 * (1 - stats::pnorm(abs(t0)))
  }

  # Compute the bootstrap p-values
  if (B > 0) {
    n <- length(r)
    idx <- matrix(sample(1:n, n*B, replace=TRUE), nrow=n)
    bs_estimates <- apply(idx, 2, function(id) {
      tryCatch({
        fitb <- esreg::esreg(model, data = data[id,], alpha = alpha,
                             g1 = g_function[1], g2 = g_function[2], early_stopping = 0)
        sb <- fitb$coefficients - fit0$coefficients
        covb <- stats::vcov(fitb, sparsity = estimator[1], cond_var = estimator[2])
        list(sb = sb, covb = covb)
      }, error=function(e) NA)
    })
    if (version %in% c(1, 2, 3)) {
      tb <- sapply(bs_estimates, function(x) {
        as.numeric(x$sb[mask] %*% solve(x$covb[mask, mask]) %*% x$sb[mask])
      })
      tb <- tb[!is.na(tb)]
      pvb_2s <- mean(tb >= t0)
      pvb_1s <- NA
    } else if (version %in% c(4, 5)) {
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
  c(
    p_value_two_sided_asymptotic = pv0_2s,
    p_value_one_sided_asymptotic = pv0_1s,
    p_value_two_sided_bootstrap = pvb_2s,
    p_value_one_sided_bootstrap = pvb_1s
  )
}
