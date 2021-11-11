#' Return a tibble of the moratorium lengths.
#'
#' @description
#' `moratorium_lengths` returns a tibble of moratorium lengths
#'
#' @details
#' This is a function that requires no input. Only returns the moratorium lengths and corresponding schools.
#' Must have the tidyverse package loaded in to do this.
#'
#'
#' @examples
#' moratorium_lengths()
#'
#'@export moratorium_lengths

moratorium_lengths <- function() {
  if (!exists("daily_crime")) {
    daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
  }
  moratorium_ids <- daily_crime %>%
    group_by(university) %>%
    mutate(treatment_na = ifelse(treatment == 0, NA, treatment)) %>%
    mutate(treatment_na = ifelse(treatment_na > 0 & (!is.na(closure_2)) & date >= closure_2, 2, treatment_na)) %>%
    mutate(treatment_na = ifelse(is.na(treatment_na), 0, treatment_na)) %>%
    ungroup() %>%
    filter(treatment_na >0) %>%
    group_by(treatment_na, university) %>%
    mutate(moratorium_id = cur_group_id()) %>%
    ungroup() %>%
    select(moratorium_id, university, treatment,date)

  daily_crime <- daily_crime %>%
    left_join(moratorium_ids) %>%
    mutate(moratorium_id = ifelse(is.na(moratorium_id), 0, moratorium_id))

  moratorium_lengths <- daily_crime %>%
    group_by(moratorium_id) %>%
    mutate(length_moratorium = sum(treatment)) %>%
    select(moratorium_id, length_moratorium, university) %>%
    ungroup() %>%
    filter(moratorium_id != 0) %>%
    distinct(length_moratorium, university)

  return(moratorium_lengths)
}
