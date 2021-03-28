#' Counts Number of Unique Items in a Data Frame.
#'
#' @description
#' `nschools` returns the number of distinct values in a column. In this case, schools.
#'
#' @details
#' This is a function that takes the main data on schools, and counts how many schools there are by using
#' the distinct() function.
#'
#' @param data a dataframe or tibble to pass in.
#' @param column_name the column you want to find the number of unique universities of.
#'
#' @examples
#' nschools(dataframe, columnname)
#'
#'
#' @export nschools
#'
#'
#'


nschools <- function(data, column_name) {
  number_of_schools <- data %>%
    distinct({{column_name}}) %>%
    nrow()
  return(number_of_schools)
}

