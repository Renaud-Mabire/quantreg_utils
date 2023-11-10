easy_quantile_regression <- function(df, dependent_var, independent_var, quantiles, qr_summary = c("boot", "nid"), covariates=NULL, multiple_plots = FALSE, ...) {
  
   # Verifications
  if (!is.data.frame(df)) {
    stop("df must be a data frame.")
  }

  if (!is.character(dependent_var) || !(dependent_var %in% names(df))) {
    stop("dependent_var must be a character string and a column name in df.")
  }
  
  if (!is.character(independent_var) || !(independent_var %in% names(df))) {
    stop("independent_var must be a character string and a column name in df.")
  }
  
  if (!is.null(covariates)) {
    if (is.character(covariates) && length(covariates) == 1) {
      covariates <- as.character(covariates)
    }
    if (!is.character(covariates) || !all(covariates %in% names(df))) {
      stop("All elements of covariates must be character strings and column names in df.")
    }
  }
  
  if (!is.numeric(quantiles) || any(quantiles < 0 | quantiles > 1)) {
    stop("quantiles must be numeric and between 0 and 1.")
  }
  
  # List creation to store the results
  formulas <- list()
  qrs <- list()
  sum_qrs <- list()
  plots <- list()
  colors <- grDevices::rainbow(length(quantiles))
  
  # Function to determine significance stars for the coefficients of the quantile regression
  get_stars <- function(p.value){
  if(is.na(p.value)){
    return("NA")
  } else if (p.value < 0.001) {
    return("***")
  } else if (p.value < 0.01) {
    return("**")
  } else if (p.value < 0.05) {
    return("*")
  } else {
    return("ns")
  }
}
  # Plot creation 
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = independent_var, y = dependent_var)) +
    ggplot2::geom_point(alpha = 0.6, color = "grey") +
    ggplot2::labs(x = independent_var, y = dependent_var) +
    ggplot2::theme_minimal() +
    ggplot2::theme_light() +
    ggplot2::theme(legend.position = "bottom")  # place the legend at the bottom

  qr_lines <- data.frame() # data frame to store the quantile regression lines
  
  # Quantile regression for each quantile and plot creation
  for (i in 1:length(quantiles)) {
  
    formula_str <- paste(dependent_var, "~", independent_var) # formula for the quantile regression
    
    if (!is.null(covariates)) {
      formula_str <- paste(formula_str, "+", paste(covariates, collapse = " + ")) # add the covariates to the formula
    }
    
    formula <- as.formula(formula_str) # convert the formula to a formula object
    qr <- quantreg::rq(formula, data = df, tau = quantiles[i], ...) # quantile regression estimation
    sum_qr <- summary(qr, se = qr_summary) # summary of the quantile regression estimation with bootstrap standard errors
    coeff <- coef(qr) # coefficients of the quantile regression
    range_var <- seq(min(df[[independent_var]]), max(df[[independent_var]]), length.out = 100) # range of the independent variable for the quantile regression line plot
    
    qr_line <- data.frame(independent_var = range_var, 
                          dependent_var = coeff[1] + coeff[2]*range_var, 
                          group = paste0("tau = ", quantiles[i], ": Coefficient ", round(coeff[2], 3), get_stars(sum_qr$coef[2, 4]), 
                                         "\n95% CI: [", round(sum_qr$coef[2, 1], 3), ", ", round(sum_qr$coef[2, 3], 3), "]")) # data frame to store the quantile regression line
    
    if (multiple_plots) {
      df_quantile <- df
      df_quantile$quantile_group <- cut(df_quantile[[dependent_var]], breaks = quantile(df_quantile[[dependent_var]], probs = c(0, quantiles)), labels = paste0("q", quantiles*100), include.lowest = TRUE)
      df_quantile$color <- ifelse(df_quantile$quantile_group == paste0("q", quantiles[i]*100), "In quantile", "Outside quantile")
      
      p_single <- ggplot2::ggplot() +
        ggplot2::geom_point(data = df_quantile, ggplot2::aes_string(x = independent_var, y = dependent_var, color = "color"), alpha = 0.6) +
        ggplot2::geom_line(data = qr_line, ggplot2::aes_string(x = "independent_var", y = "dependent_var"), colour = colors[i], linewidth = 1) +
        ggplot2::scale_color_manual(values = c("In quantile" = colors[i], "Outside quantile" = "grey")) +
        ggplot2::labs(title = paste0("Quantile Regression: tau = ", quantiles[i]), x = independent_var, y = dependent_var) +
        ggplot2::theme_minimal() +
        ggplot2::theme_light() +
        ggplot2::annotate("text", x = min(df[[independent_var]]), y = max(df[[dependent_var]]), 
                          label = paste0("Coefficient: ", round(coeff[2], 3), get_stars(sum_qr$coef[2, 4]), 
                                         "\n95% CI: [", round(sum_qr$coef[2, 1], 3), ", ", round(sum_qr$coef[2, 3], 3), "]"), 
                          hjust = 0, vjust = 1)
      
      plots[[i]] <- p_single
    } else {
      qr_lines <- rbind(qr_lines, qr_line)
    }
    
    formulas[[i]] <- formula
    qrs[[i]] <- qr
    sum_qrs[[i]] <- sum_qr
  }
  # Plot creation for multiple quantiles in one plot
  if (!multiple_plots) {
    p <- p + ggplot2::geom_line(data = qr_lines, ggplot2::aes_string(x = "independent_var", y = "dependent_var", group = "group", color = "group"), linewidth = 1) +
      ggplot2::scale_color_manual(values = setNames(colors, unique(qr_lines$group)))
    
    result <- list(formulas = formulas, qrs = qrs, sum_qrs = sum_qrs, plots = p)
  } else {
    grid.arrange <- gridExtra::grid.arrange(grobs = plots, ncol = length(quantiles))
    result <- list(formulas = formulas, qrs = qrs, sum_qrs = sum_qrs, plots = grid.arrange)
  }
  
  # Class definition and return of the result object
  class(result) <- "quantile_plots"
  return(result)
}
