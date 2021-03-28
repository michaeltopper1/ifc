#' Finds the Average Length of IFC Moratoria.
#'
#' @description
#' `avg_closure` returns the average length of fraternity moratoria in days.
#'
#' @details
#' This function relies on dplyr, lubridate, and rlang to work. Moreover, this function
#' does a few operations. First, it selects only the columns of interest, then, it splits
#' a tibble into two: one for first set of closures, and one for second closures (third coming).
#' Next, it creates columns with the length of each of these closures. Finally, it binds together
#' the two tibbles, and takes the mean of the length.
#'
#' @param data an object/dataframe/tibble.
#' @param university the column in the object that contains the universities.
#' @param start1 the column in the object that contains the start date of the first moratorium.
#' @param end1 the column in the object that contains the  end date of the first moratorium.
#' @param start2 the column in the object that contains the start date of the second moratorium.
#' @param end2 the column in the object that contains the end date of the second moratorium.
#'
#' @examples
#' avg_closure(data, university, start1, end1, start2, end2)
#'
#' @export avg_closure
#'

avg_closure <- function(data, university, start1, end1, start2, end2) {
  first_closures <- data %>%
    select({{university}}, {{start1}}, {{end1}}) %>%
    distinct() %>%
    mutate(length = {{end1}} - {{start1}}) %>%
    rename("start" = {{start1}}, "end" = {{end1}})
  second_closures <- data %>%
    select({{university}}, {{start2}}, {{end2}}) %>%
    distinct() %>%
    filter(!is.na({{start2}})) %>%
    mutate(length = {{end2}} - {{start2}}) %>%
    rename("start" = {{start2}}, "end" = {{end2}})
  all_closures <- bind_rows(first_closures, second_closures)
  average_length <- all_closures %>%
    mutate(length = as.character(length)) %>%
    mutate(length = as.double(length)) %>%
    summarize(length = mean(length, na.rm = T)) %>%
    pull()
  return(round(average_length,2))
}

