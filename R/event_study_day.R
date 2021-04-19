
#' Creates the Event Study tibble for analysis at Daily Level Data.
#'
#' @description
#' `event_study_day` returns a tibble ready for event study analysis in units of days
#'
#' @details
#' This function creates the event study tibble needed to do analysis in event study framework.
#'
#'
#' @param data an object/dataframe/tibble.
#' @param window how big the event window is (in weeks)
#' @param reference_days a number for which year you want to be the reference week It will be paste0("treatment_minus_", reference_year)
#' @param cluster_blocks a number for how many days you want clustered together. I usually do 7 for week.
#'
#' @examples
#' event_study_day(daily_crime,  window = 8, cluster_blocks = 7, reference_days =1 )
#'
#' @export event_study_day
#'



event_study_day <- function(data, window, cluster_blocks, reference_days){
  event_study <- data %>%
    mutate(across(c(closure_1_end, closure_2_end), ~ as.character(.))) %>%
    mutate(closure_1_end = ifelse(university == "Louisiana State University and Agricultural & Mechanical College",
                                  closure_2_end, closure_1_end)) %>%
    mutate(across(c(closure_1_end, closure_2_end, closure_2), ~as.Date(.))) %>%
    mutate(treatment = ifelse(date >= closure_1 & date < closure_1_end, 1, 0)) %>%
    select(-starts_with("closure_2"))


  for (i in window) {
    closure_plus <- paste("closure_plus_", i, sep = "")
    closure_plus_end <- paste0("closure_plus_",i, "_end")
    colname_treat <- paste0("treatment_plus_", i)
    event_study <- event_study %>%
      mutate(!!sym(closure_plus) := (closure_1_end + (i-1) *days(period_blocks))) %>%
      mutate(!!sym(closure_plus_end) := closure_1_end + ((i) * days(period_blocks))) %>%
      mutate(!!sym(colname_treat) := ifelse(date >= !!sym(closure_plus) & date < !!sym(closure_plus_end), 1, 0))
  }

  for (i in window) {
    closure_minus <- paste("closure_minus_", i, sep = "")
    closure_minus_end <- paste0("closure_minus_",i, "_end")
    colname_treat <- paste0("treatment_minus_", i)
    event_study <- event_study %>%
      mutate(!!sym(closure_minus) := closure_1 -  i *days(period_blocks)) %>%
      mutate(!!sym(closure_minus_end) := closure_1 - ((i- 1) * days(period_blocks))) %>%
      mutate(!!sym(colname_treat) := ifelse(date >= !!sym(closure_minus) & date < !!sym(closure_minus_end), 1, 0))

  }


  reference_days <- paste0("treatment_minus_", reference_days)

  endpoint_plus <- paste0("treatment_plus_", length(window))
  endpoint_minus <- paste0("treatment_minus_", length(window))
  final_closure_minus <- paste0("closure_minus_", length(window) - 1)
  final_closure_plus <- paste0("closure_plus_", length(window))

  event_study <- event_study %>%
    mutate(!!sym(endpoint_minus) := ifelse(date < !!sym(final_closure_minus), 1, 0 )) %>%
    mutate(!!sym(endpoint_plus) := ifelse(date >= !!sym(final_closure_plus), 1, 0))

  event_study <- event_study %>%
    mutate(!!sym(reference_days) := 0)
  return(event_study)
}
