calculate_quantile_regression_CI <- function(taus, independent_var, dependent_var, data, R = 100, ci_level = 0.95) {
  # Defining the matrix X with the intercept and independent variable
  X <- cbind(1, data[[independent_var]])
  # Initializing vectors to store lower and upper bounds of ICs for each tau value
  lower_bounds <- numeric(length(taus))
  upper_bounds <- numeric(length(taus))

  # Looping over each tau value
  for (i in 1:length(taus)) {
    # Bootstrapping the coefficients
    tau <- taus[i]
    QR.b <- boot.rq(X, data[[dependent_var]], tau = tau, R = R)
    # Calculating confidence intervals
    ci_quantiles <- c((1 - ci_level) / 2, 1 - (1 - ci_level) / 2)
    IC95 <- t(apply(QR.b$coefficients, 2, quantile, prob = ci_quantiles)) %>% as.data.frame()
    # Storing the results in the vectors
    lower_bounds[i] <- IC95$`2.5%`[2]
    upper_bounds[i] <- IC95$`97.5%`[2]
  }

  # Returning a data frame with the tau values and their corresponding confidence intervals
  return(data.frame(tau = taus, lower = lower_bounds, upper = upper_bounds))
}