#' Check Inputs
#'
#' Checks the input data.
#' @param r A vector of returns
#' @param q A vector of Value-at-Risk forecasts
#' @param e A vector of expected shortfall forecasts
#' @param s A vector of volatility forecasts
#' @param alpha Scalar probability level in (0, 1)
#' @keywords internal
#' @export
check_inputs <- function(r, q=NULL, e, s=NULL, alpha=NULL) {
  # Check the probability
  if (!is.null(alpha)) {
    if ((alpha <= 0) | (1 <= alpha)) {
      stop("alpha not in (0, 1)!")
    }
  }

  # Check for errors in the input data
  # TODO: improve the check!
  df <- tryCatch(cbind(r, q, e, s),
                 warning = function(w) stop("Some error with the input data!"),
                 error = function(e) stop("Some error with the input data!"))
}

#' Exceedance Residuals Backtest
#'
#' Tests whether the mean of the exceedance residuals, respectively the
#' mean of the standardized exceedance residuals is zero.
#' @inheritParams check_inputs
#' @param B Number of bootstrap iterations
#' @return Returns a 2x2 matrix with p-values
#' @examples
#' data(df)
#' er_backtest(r = df$r, q = df$q1, e = df$e1, s = df$s1)
#' er_backtest(r = df$r, q = df$q2, e = df$e2)
#' @references \href{https://doi.org/10.1016/S0927-5398(00)00012-8}{McNeil & Frey (2000)}
#' @export
er_backtest <- function(r, q, e, s=NULL, B=1000) {
  check_inputs(r = r, q = q, e = e, s = s)

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
#' @inheritParams check_inputs
#' @param hommel If TRUE, use Hommels correction, else use the classical Bonferroni correction.
#' @return Returns a 2x2 matrix with p-values
#' @examples
#' data(df)
#' calibration_backtest(r = df$r, q = df$q1, e = df$e1, s = df$s1, alpha = 0.025)
#' calibration_backtest(r = df$r, q = df$q2, e = df$e2, alpha = 0.025)
#' @references\href{https://arxiv.org/abs/1608.05498}{Nolde & Ziegel (2007)}
#' @export
calibration_backtest <- function(r, q, e, s=NULL, alpha, hommel=TRUE) {
  check_inputs(r = r, q = q, e = e, s = s, alpha = alpha)

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


#' Expected Shortfall Regression Intercept Backtest
#'
#' Tests whether the expected shortfall of the forecast error r - e is zero-
#'
#' Contains two backtests using the esreg package.
#' The first is an intercept-only backtest. The forecast error,
#' r - e is regressed on an intercept, which is tested for zero.
#' The second test regresses the expected shortfall forecasts
#' and an intercept term on the returns and tests the coefficients for (0, 1).
#'
#' @inheritParams check_inputs
#' @param B Number of bootstrap samples. Set to 0 to disable bootstrapping.
#' @param avg_block_size Average length of the blocks of the stationary bootstrap.
#' @return Returns a 2x2 matrix with p-values
#' @examples
#' data(df)
#' esr_backtest_intercept(r = df$r, e = df$e1, alpha = 0.025)
#' esr_backtest_intercept(r = df$r, e = df$e2, alpha = 0.025)
#' @references Bayer & Dimitriadis (2017)
#' @export
esr_backtest_intercept <- function(r, e, alpha, B=0, avg_block_size=NULL) {
  check_inputs(r = r, e = e, alpha = alpha)

  fit0 <- esreg::esreg(r - e ~ 1, alpha = alpha, g1 = 2, g2 = 4)
  cov0 <- stats::vcov(object = fit0, sparsity = "iid", cond_var = "ind")[2, 2]
  t0 <- unname(fit0$coefficients_e / sqrt(cov0))

  # Asymptotic
  pv0_2s <- 2 * (1 - stats::pnorm(abs(t0)))
  pv0_1s <- stats::pnorm(t0)

  # Bootstrap
  if (B > 0) {
    if (is.null(avg_block_size)) {
      avg_block_size <- floor(sqrt(length(r)))
    }
    set.seed(1)
    idx <- esreg:::stationary_bootstrap_indices(n = length(r), avg_block_size = avg_block_size, B = B)

    tb <- apply(idx, 2, function(id) {
      tryCatch({
        fitb <- esreg::esreg(r[id] - e[id] ~ 1, alpha = alpha, g1 = 2, g2 = 4,
                             control = list(terminate_after=1))
        covb <- stats::vcov(fitb, sparsity="iid", cond_var="ind")[2, 2]
        sb <- fitb$coefficients_e - fit0$coefficients_e
        as.numeric(sb / sqrt(covb))
      }, error=function(e) NA)
    })
    tb <- tb[!is.na(tb)]
    pvb_2s <- mean(abs(t0) <= abs(tb))
    pvb_1s <- mean(tb <= t0)
  } else {
    pvb_2s <- pvb_1s <- NA
  }

  # Return results
  out <- rbind(c(pv0_2s, pv0_1s),
               c(pvb_2s, pvb_1s))
  rownames(out) <- c("Asymptotic", "Bootstrap")
  colnames(out) <- c("Two_Sided", "One_Sided")
  out
}


#' Expected Shortfall Regression Backtest
#'
#' Regresses the expected shortfall forecasts and an intercept term on the returns
#' and tests the coefficients for (0, 1).
#'
#' @inheritParams check_inputs
#' @inheritParams esr_backtest_intercept
#' @return Returns a 2-dim. vector with p-values
#' @examples
#' data(df)
#' esr_backtest(r = df$r, e = df$e1, alpha = 0.025)
#' esr_backtest(r = df$r, e = df$e2, alpha = 0.025)
#' @references Bayer & Dimitriadis (2017)
#' @export
esr_backtest <- function(r, e, alpha, B=0, avg_block_size=NULL) {
  check_inputs(r = r, e = e, alpha = alpha)

  fit0 <- esreg::esreg(r ~ e, alpha = alpha, g1 = 2, g2 = 1)
  s0 <- fit0$coefficients_e + c(0, -1)
  cov0 <- stats::vcov(object = fit0, sparsity = "iid", cond_var = "scl_sp")[3:4, 3:4]
  t0 <- unname(as.numeric(s0 %*% solve(cov0) %*% s0))

  # Asymptotic
  pv0 <- 1 - stats::pchisq(t0, 2)

  # Bootstrap
  if (B > 0) {
    if (is.null(avg_block_size)) {
      avg_block_size <- floor(sqrt(length(r)))
    }
    set.seed(1)
    idx <- esreg:::stationary_bootstrap_indices(n = length(r), avg_block_size = avg_block_size, B = B)

    tb <- apply(idx, 2, function(id) {
      tryCatch({
        fitb <- esreg::esreg(r[id] ~ e[id], alpha = alpha, g1 = 2, g2 = 1,
                             control = list(terminate_after=1))
        covb <- stats::vcov(fitb, sparsity="nid", cond_var="scl_sp")[3:4, 3:4]
        sb <- fitb$coefficients_e - fit0$coefficients_e
        as.numeric(sb %*% solve(covb) %*% sb)
      }, error=function(e) NA)
    })
    tb <- tb[!is.na(tb)]
    pvb <- mean(tb >= t0)
  } else {
    pvb <- NA
  }

  # Return results
  out <- c(pv0, pvb)
  names(out) <- c("Asymptotic", "Bootstrap")
  out
}

