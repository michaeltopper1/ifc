
#' Log(. + 1) transformation of vector elements.
#'
#' @description
#' `log1` returns the inverse hyperbolic sine transformation
#'
#' @details
#' This is a generic function that transforms a vector of elements using the log(. + 1)
#' transformation. This function will only take numeric atomic types such as doubles, integers, and floats.
#'
#' @param x description a vector of numeric elements.
#'
#' @example
#' x <- c(1,2,3)
#' log1_transform(x)
#'
#' @export log1_transform
#'

log1_transform <- function(x) {
  if (!is.character(x) & !is.logical(x)) {
    log1_x <- log(x + 1)
    return(log1_x)
  }
  else {
    stop("`x` is not a numeric value")
  }
}


