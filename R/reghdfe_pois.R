#' High dimension fixed effects regression (Poisson) with fixest package.
#'
#' @description
#' `reghdfe_pois` returns the fixest object consisting of the regression equation.
#'
#' @details
#' This function calls the fixest::fepois() function, but simiplifies the typing.
#'
#' @param data tibble or dataframe.
#' @param outcome outcome of interest. Must be a string.
#' @param explanatory_vars vector of explanatory variables. Must be a string.
#' @param fixed_effects a vector of fixed effects. Must be a string.
#' @param cluster column to cluster by. Must be a string.
#'
#' @examples
#' reghdfe(mtcars, "mpg", "hp", "am", "am")
#'
#'@export reghdfe_pois


reghdfe_pois <- function(data, outcome, explanatory_vars, fixed_effects, cluster){
  formula <- as.formula(paste0(
    outcome, "~",
    paste(explanatory_vars, collapse = "+"),
    paste("|"), paste(fixed_effects, collapse = "+")))
  return(fepois(formula,cluster = cluster, data = data))
}


