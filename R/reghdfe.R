#' High dimension fixed effects regression (OLS) with fixest package.
#'
#' @description
#' `reghdfe` returns the fixest object consisting of the regression equation.
#'
#' @details
#' This function calls the fixest::feols() function, but simiplifies the typing.
#'
#' @param data tibble or dataframe.
#' @param outcome outcome of interest. Must be a string.
#' @param explanatory_vars vector of explanatory variables. Must be a string.
#' @param fixed_effects a vector of fixed effects. Must be a string. Defaults to NULL for no fixed effects.
#' @param cluster column to cluster by. Must be a string.
#'
#' @examples
#' reghdfe(mtcars, "mpg", "hp", "am", "am")
#'
#'@export reghdfe


reghdfe <- function(data, outcome, explanatory_vars, fixed_effects = NULL, cluster){
  ## this is to get the data into the call argument correctly see stackoverflow remark
  dataname <- as.symbol(deparse(substitute(data)))
  if (is.null(fixed_effects)) {
   formula <-  as.formula(paste0(
      outcome, "~",
      paste(explanatory_vars, collapse = "+")))
  }
  else{
    formula <- as.formula(paste0(
      outcome, "~",
      paste(explanatory_vars, collapse = "+"),
      paste("|"), paste(fixed_effects, collapse = "+")))
  }
  model <- feols(formula,cluster = cluster, data = data)
  ## changing the calls to match the original
  model$call$fml <- formula
  model$call$cluster <- cluster
  model$call$data <- dataname
  ## returning the model
  return(model)
}
