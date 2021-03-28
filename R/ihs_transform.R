
#' Inverse hyperbolic sine transformation of vector elements.
#'
#' @description
#' `ihs_transform` returns the inverse hyperbolic sine transformation
#'
#' @details
#' This is a generic function that transforms a vector of elements using the inverse hyperbolic sine
#' transformation. This function will only take numeric atomic types such as doubles, integers, and floats.
#'
#' @param x description a vector of numeric elements.
#'
#' @examples
#' x <- c(1,2,3)
#' ihs_transform(x)
#'
#'@export ihs_transform

ihs_transform <- function(x) {
  if (!is.character(x) & !is.logical(x)) {
    ihs_x <- log(x + (x^2 + 1)^(0.5))
    return(ihs_x)
  }
  else {
    stop("`x` is not a numeric value")
  }
}

