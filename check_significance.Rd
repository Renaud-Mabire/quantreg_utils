\name{check_significance}
\alias{check_significance}
\title{
  Check Significance of Variables in Quantile Regression Models
}
\description{
  Assesses the significance of a specified variable across different quantiles in a quantile regression model. It returns the quantiles where the variable's coefficient is not statistically significant based on a given p-value threshold.
}
\usage{
check_significance(qr_fit, variable_name, p_threshold)
}
\arguments{
  \item{qr_fit}{
    A quantile regression model object, typically a result of \code{rq()} function.
  }
  \item{variable_name}{
    A string specifying the name of the independent variable to check for significance.
  }
  \item{p_threshold}{
    A numeric value indicating the p-value threshold for significance. Coefficients with p-values higher than this threshold are considered non-significant.
  }
}
\details{
  The function iterates through each quantile regression model within the \code{qr_fit} object for different \code{tau} values. It checks the p-value of the specified \code{variable_name} at each quantile and collects those \code{tau} values where the p-value exceeds the \code{p_threshold}.
}
\value{
  A list of \code{tau} values where the specified variable's coefficients are not statistically significant according to the given p-value threshold. If all coefficients are significant, an empty list is returned.
}
\author{
  renaud Mabire-Yon
}
\note{
  Assumes that the \code{qr_fit} object contains a list of quantile regression models for different \code{tau} values, each with a coefficient matrix including p-values.
}
\seealso{
  \code{\link[quantreg]{rq}}, \code{\link{summary}}
}
\examples{
taus <- seq(from = 0.25, to = 0.75, by = 0.01)
fit_multiple <- rq(stflife ~ agea_scale, data = data, tau = taus)
non_significant_taus <- check_significance(qr_fit = fit_multiple, variable_name = "agea_scale", p_threshold = 0.05)
print(non_significant_taus)
}
\keyword{models}
\keyword{regression}
