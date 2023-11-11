\name{calculate_quantile_regression_CI}
\alias{calculate_quantile_regression_CI}
\title{
  Calculate Confidence Intervals for Coefficients in Quantile Regression
}
\description{
  Computes bootstrap confidence intervals for coefficients at specified quantiles in a quantile regression model. This function is used to assess the precision of the estimated effects at different points in the conditional distribution of the dependent variable.
}
\usage{
calculate_quantile_regression_CI(taus, independent_var, dependent_var, data, R = 100, ci_level = 0.95)
}
\arguments{
  \item{taus}{
    A numeric vector of quantiles at which to compute the confidence intervals.
  }
  \item{independent_var}{
    A string specifying the name of the independent variable in the model.
  }
  \item{dependent_var}{
    A string specifying the name of the dependent variable in the model.
  }
  \item{data}{
    A data frame containing the variables specified in \code{independent_var} and \code{dependent_var}.
  }
  \item{R}{
    The number of bootstrap replications for computing the confidence intervals (default is 100).
  }
  \item{ci_level}{
    The confidence level for the intervals (default is 0.95).
  }
}
\details{
  The function performs bootstrap resampling to estimate the variability of the coefficient estimates at each specified quantile. It calculates the lower and upper bounds of the confidence intervals for the coefficient of the independent variable in the quantile regression model.
}
\value{
  A data frame with columns \code{tau}, \code{lower}, and \code{upper}, representing the quantile levels and the corresponding lower and upper bounds of the confidence intervals for the coefficient of the independent variable.
}
\author{
  Renaud Mabire-Yon
}
\note{
  The function uses the \code{boot.rq()} function from the \code{quantreg} package for bootstrap resampling. The confidence intervals are computed using the percentile method.
}
\seealso{
  \code{\link[quantreg]{rq}}, \code{\link{boot.rq}}, \code{\link[boot]{boot}}
}
\examples{
taus <- c(0.25, 0.5, 0.75)
data <- data.frame(y = rnorm(100), x = rnorm(100))
ci_results <- calculate_quantile_regression_CI(taus, "x", "y", data)
print(ci_results)
}
\keyword{quantile}
\keyword{regression}
\keyword{confidence}
\keyword{bootstrap}
