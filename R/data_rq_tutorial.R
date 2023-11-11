#' Preprocessed Data for Quantile Regression Tutorial
#'
#' This dataset is derived from the 9th round (2018) of the European Social Survey (ESS),
#' preprocessed for use in a quantile regression tutorial. The preprocessing included
#' filtering for respondents from Bulgaria (BG) and cleaning missing or special values
#' in variables `agea`, `stflife`, and `hincfel`.
#'
#' @format A data frame with 2147 observations and 573 variables.
#' @details The preprocessing steps included:
#' - Filtering data to include only respondents from Bulgaria (BG).
#' - Removing `999` values for `agea`.
#' - Removing `77`, `88`, `99` values for `stflife`.
#' - Removing `7`, `8`, `9` values for `hincfel`.
#' @source European Social Survey European Research Infrastructure (ESS ERIC). (2021). ESS9 - integrated file, edition 3.1 [Data set]. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ESS9E03_1
#' For more information, please visit [ESS9 2018](http://www.europeansocialsurvey.org/).
#' @usage data(data_rq_tutorial)
#' @name data_rq_tutorial
NULL
