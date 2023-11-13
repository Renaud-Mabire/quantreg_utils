#' Check for Overlap of Confidence Intervals
#'
#' This function checks for the overlap of confidence intervals for each pair of taus in a quantile regression model. 
#' It can be particularly useful for analyzing the output of the `calculate_quantile_regression_CI` function, 
#' which computes bootstrap confidence intervals for quantile regression coefficients.
#'
#' @param df A data frame containing the columns 'tau', 'lower', and 'upper'.
#' This data frame can be the output from the `calculate_quantile_regression_CI` function.
#' @param show_overlap A logical value. If TRUE, shows pairs of taus with overlapping confidence intervals; 
#' if FALSE, shows pairs with non-overlapping intervals.
#'
#' @return A data frame with each pair of taus and an indication of whether their confidence intervals overlap. 
#' Returns a message if no intervals overlap (when show_overlap is TRUE) or if all intervals overlap (when show_overlap is FALSE).
#' 
#' @examples
#' # Example of using calculate_quantile_regression_CI and check_CI_overlap together
#' # taus <- c(0.25, 0.5, 0.75)
#' # data <- data.frame(y = rnorm(100), x = rnorm(100))
#' # ci_results <- calculate_quantile_regression_CI(taus, independent_var, dependent_var, data, R = 100, ci_level = 0.95)
#' # check_results <- check_CI_overlap(ci_results, show_overlap = TRUE)
#'
#' @export
check_CI_overlap <- function(df, show_overlap = TRUE) {
  # Creating a data frame to store the results
  overlap_results <- data.frame(tau1 = numeric(0), tau2 = numeric(0), overlap = logical(0))
  
  # Loop to compare each pair of taus
  for (i in 1:(nrow(df) - 1)) {
    for (j in (i + 1):nrow(df)) {
      # Checking for overlap
      overlap <- !(df$upper[i] < df$lower[j] || df$upper[j] < df$lower[i])
      
      # Add to overlap_results only if it matches the requested criterion
      if (overlap == show_overlap) {
        overlap_results <- rbind(overlap_results, c(df$tau[i], df$tau[j], overlap))
      }
    }
  }
  
  # Naming the columns of the results data frame
  names(overlap_results) <- c("tau1", "tau2", "overlap")
  
  # Checking if there are no results to return
  if (nrow(overlap_results) == 0 && show_overlap) {
    return("No overlapping intervals found.")
  } else if (nrow(overlap_results) == 0 && !show_overlap) {
    return("All intervals overlap.")
  }
  
  return(overlap_results)
}