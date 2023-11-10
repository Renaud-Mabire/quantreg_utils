test_location_shift <- function(data, dependent_variable, independent_variable, taus, covars = NULL) {
  # Construct the formula
  formula_str <- paste(dependent_variable, "~", paste(independent_variable, collapse = "+"))
  if (!is.null(covars)) {
    formula_str <- paste(formula_str, "+", paste(covars, collapse = "+"))
  }
  formula_obj <- as.formula(formula_str)
  
  # Create models for each tau
  rq_models <- lapply(taus, function(t) {
    rq(formula_obj, data = data, tau = t)
  })
  
  # Perform ANOVA on the models
  anova_result <- do.call(anova, rq_models)
  
  return(list(models = rq_models, anova = anova_result))
}