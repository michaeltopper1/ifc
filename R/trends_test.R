
#' Creates the data needed to perform the random trends test.
#'
#' @description
#' `trends_test` returns a tibble that is ready for the random trends test.
#'
#' @details
#' This function creates the event study tibble needed to do analysis in event study framework.
#'
#'
#' @param data an object/dataframe/tibble.
#' @param window how big the event window is (in weeks)
#' @param cluster_blocks a number for how many days you want clustered together. I usually do 7 for week.
#'
#' @examples
#' trends_test(daily_crime,  window = 8, cluster_blocks = 7)
#'
#' @export trends_test
#'



trends_test <- function(data, window, cluster_blocks) {
  window <- c(1:window)

  two_closure_schools <- data %>%
    select(university, closure_2, closure_2_end, treatment, alcohol_offense, sexual_assault,
           robbery_burglary, theft, drug_offense,  date, total_students_all,
           ftime_total_undergrad, total_undergrad_asian,
           total_undergrad_black, total_undergrad_hispanic, graduation_rate_total_cohort_) %>%
    mutate(university = ifelse(!is.na(closure_2),paste0(university, "_2"), university)) %>%
    rename(closure_1 = closure_2, closure_1_end = closure_2_end) %>%
    filter(str_detect(university, "_2$")) %>%
    filter(university != "Louisiana State University and Agricultural & Mechanical College_2")

  all_schools <- data %>%
    mutate(across(c(closure_1_end, closure_2_end), ~ as.character(.))) %>%
    mutate(closure_1_end = ifelse(university == "Louisiana State University and Agricultural & Mechanical College",
                                  closure_2_end, closure_1_end)) %>%
    mutate(across(c(closure_1_end, closure_2_end, closure_2), ~as.Date(.))) %>%
    mutate(treatment = ifelse(date >= closure_1 & date < closure_1_end, 1, 0)) %>%
    select(-starts_with("closure_2"))


  event_study <- bind_rows(all_schools, two_closure_schools)

  for (i in window) {
    closure_plus <- paste("closure_plus_", i, sep = "")
    closure_plus_end <- paste0("closure_plus_",i, "_end")
    colname_treat <- paste0("treatment_plus_", i)
    event_study <- event_study %>%
      mutate(!!sym(closure_plus) := (closure_1_end + (i-1) *days(cluster_blocks))) %>%
      mutate(!!sym(closure_plus_end) := closure_1_end + ((i) * days(cluster_blocks))) %>%
      mutate(!!sym(colname_treat) := ifelse(date >= !!sym(closure_plus) & date < !!sym(closure_plus_end), 1, 0))
  }

  for (i in window) {
    closure_minus <- paste("closure_minus_", i, sep = "")
    closure_minus_end <- paste0("closure_minus_",i, "_end")
    colname_treat <- paste0("treatment_minus_", i)
    event_study <- event_study %>%
      mutate(!!sym(closure_minus) := closure_1 -  i *days(cluster_blocks)) %>%
      mutate(!!sym(closure_minus_end) := closure_1 - ((i- 1) * days(cluster_blocks))) %>%
      mutate(!!sym(colname_treat) := ifelse(date >= !!sym(closure_minus) & date < !!sym(closure_minus_end), 1, 0))
  }

  return(event_study)
}


