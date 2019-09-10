p <- quantmod::getSymbols("^GSPC", auto.assign = FALSE,
                          from = "2000-01-01", to = "2018-12-31")
p <- p[,6]
r <- as.numeric(diff(log(p))[-1]) * 100
win <- 250
alpha <- 0.025

q <- e <- s <- x <- rep(NA, length(r))
for (t in (win+1):length(r)) {
  r_tmp <- r[(t-win):(t-1)]
  q[t] <- quantile(r_tmp, probs=alpha)
  e[t] <- mean(r_tmp[r_tmp < q[t]])
  s[t] <- sd(r_tmp)
}

risk_forecasts <- data.frame(r = r, q = q, e = e, s = s)[-(1:win),]
usethis::use_data(risk_forecasts, overwrite = TRUE)
