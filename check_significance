check_significance <- function(qr_fit, variable_name, p_threshold) {
  non_significant_taus <- list()  # List to store taus with non-significant p-values
  summary_fit <- summary(qr_fit)  # Obtain the summary of the fitted model
  
  # Check if the variable name is present in the model results
  if (!(variable_name %in% rownames(summary_fit[[1]]$coefficients))) {
    stop("Variable name not found in the model summary.")
  }
  
  # Loop through models for each tau
  for (i in 1:length(summary_fit)) {
    # Get the p-value for the specified independent variable
    vi_p_value <- summary_fit[[i]]$coefficients[variable_name, "Pr(>|t|)"]
    
    # Check if the p-value is above the threshold
    if (vi_p_value > p_threshold) {
      # Add the corresponding tau to the list if the p-value is non-significant
      non_significant_taus[[length(non_significant_taus) + 1]] <- summary_fit[[i]]$tau
    }
  }
  
  # Check if the list is empty
  if (length(non_significant_taus) == 0) {
    message("All coefficients for variable '", variable_name, "' are significant at the p < ", p_threshold, " level.")
  } else {
    return(non_significant_taus)  # Return the list of taus with non-significant p-values
  }
}
