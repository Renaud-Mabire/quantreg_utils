\name{easy_quantile_regression}
\alias{easy_quantile_regression}
\title{Fit, visualize and summarize quantile regression models in R}
\description{
This function fits quantile regression models on a dataset for specified quantiles between 0 and 1. It returns the model fits as well as a plot visualizing the quantile regression lines. The function can handle multiple quantile regression models for a given dependent and independent variable, and optionally includes additional covariates in the models. The output includes quantreg model fits, summaries and formulas, and visualizes the quantile regression lines on a single plot or separate plots. Model coefficients and confidence intervals are displayed in the plot legend.}
\usage{
easy_quantile_regression(
  data,
  dependent_var, 
  independent_var,
  quantiles,
  covariates = NULL,
  multiple_plots = FALSE
)}
\arguments{
  \item{data}{a data frame containing the variables}
  \item{dependent_var}{name of dependent variable as character string}
  \item{independent_var}{name of independent variable as character string}
  \item{quantiles}{numeric vector of quantiles between 0 and 1}
  \item{covariates}{optional character vector of additional covariate names}
  \item{multiple_plots}{boolean to control separate plot per quantile}
}
\value{
  A list with the following components:
  \itemize{
    \item{formulas}{list of model formulas}
    \item{fits}{list of quantreg model fits}
    \item{summaries}{list of model summaries}
    \item{plots}{plot object(s) visualizing quantile regressions}
    \item{legend_text}{model coefficients and CIs for plot legend}
  }
}
\examples{
models <- easy_quantile_regression(mydata, "y", "x", c(0.25, 0.5, 0.75))

models$plots # view regression line plots

summary(models$fits[[1]]) # view model summary for 1st quantile
}
\seealso{
\code{\link{quantreg::rq}, \link{ggplot2::ggplot2}, \link{ggplot2::grDevices}, \link{ggplot2::gridExtra}}
}
\keyword{hplot}
\keyword{regression}
