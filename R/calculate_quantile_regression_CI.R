#' Calculate Confidence Intervals for Quantile Regression
#'
#' Computes bootstrap confidence intervals for coefficients at specified quantiles in a quantile regression model.
#'
#' @param taus Numeric vector of quantiles at which to compute the confidence intervals.
#' @param independent_var Name of the independent variable in the model.
#' @param dependent_var Name of the dependent variable in the model.
#' @param data Data frame containing the variables specified in independent_var and dependent_var.
#' @param R Number of bootstrap replications for computing the confidence intervals (default is 100).
#' @param ci_level Confidence level for the intervals (default is 0.95).
#' @return A data frame with columns 'tau', 'lower', and 'upper', representing the quantile levels and the corresponding lower and upper bounds of the confidence intervals for the coefficient of the independent variable.
#' @export
#' @examples
#' # Example usage:
#' # taus <- c(0.25, 0.5, 0.75)
#' # data <- data.frame(y = rnorm(100), x = rnorm(100))
#' # ci_results <- calculate_quantile_regression_CI(taus, "x", "y", data)
#' # print(ci_results)

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