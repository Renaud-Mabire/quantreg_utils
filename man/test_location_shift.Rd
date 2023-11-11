\name{test_location_shift}
\alias{test_location_shift}
\title{
  Test for Location Shift Across Different Quantiles
}
\description{
  Performs a location shift test for a specified dependent variable against one or more independent variables across different quantiles, optionally adjusting for additional covariates. The function fits quantile regression models for each specified quantile and then performs an ANOVA to test for location shifts.
}
\usage{
test_location_shift(data, dependent_variable, independent_variable, taus, covars = NULL)
}
\arguments{
  \item{data}{
    The data frame containing the variables to be used in the model.
  }
  \item{dependent_variable}{
    A string specifying the name of the dependent variable.
  }
  \item{independent_variable}{
    A vector of strings specifying the names of the independent variables.
  }
  \item{taus}{
    A numeric vector of quantiles at which to perform the regression.
  }
  \item{covars}{
    An optional vector of strings specifying additional covariates to include in the model. Defaults to NULL.
  }
}
\details{
  The function constructs a quantile regression model formula based on the specified dependent, independent variables, and covariates. It then fits quantile regression models for each value in \code{taus}. Finally, an ANOVA test is performed across these models to assess the location shift.
}
\value{
  A list containing two elements:
  \item{models}{
    A list of quantile regression models fitted for each value in \code{taus}.
  }
  \item{anova}{
    The result of the ANOVA test performed on the quantile regression models.
  }
}
\author{
  Renaud Mabire-Yon
}
\note{
  This function relies on the \code{rq()} function from the \code{quantreg} package and \code{anova()} for performing the analysis.
}
\seealso{
  \code{\link[quantreg]{rq}}, \code{\link{anova}}
}
\examples{
data <- data.frame(dependent_var = rnorm(100), independent_var = rnorm(100), covar1 = rnorm(100))
taus <- c(0.25, 0.5, 0.75)
result <- test_location_shift(data, "dependent_var", "independent_var", taus, covars = "covar1")
print(result)
}
\keyword{models}
\keyword{anova}
\keyword{quantile}
