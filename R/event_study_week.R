#' Creates the Event Study tibble for Analysis-Weekly Data.
#'
#' @description
#' `event_study_week` returns a tibble ready for event study analysis in units of weeks
#'
#' @details
#' This function creates the event study tibble needed to do analysis in event study framework.
#' Moreover, the month prior to treatment is the reference month (e.g. set to zero), and all months with
#' any amount of treatment are considered a treated month (e.g. 2 weeks in March = full month of March treated).
#'
#'
#' @param data an object/dataframe/tibble.
#' @param window_length the column in the object that contains the  end date of the first moratorium.
#'
#'
#' @examples
#' event_study_month(monthly_crime, 2014, 2019, 4)
#'
#' @export event_study_week
#'


event_study_week <- function(data, window_length) {
  window <- c(1:window_length)
  weekly_crime <- data %>%
    mutate(across(c(closure_1_end, closure_2_end), ~ as.character(.))) %>%
    mutate(closure_1_end = ifelse(university == "Louisiana State University and Agricultural & Mechanical College",
                                  closure_2_end, closure_1_end)) %>%
    mutate(across(c(closure_1_end, closure_2_end, closure_2), ~as.Date(.)))


  event_study_df <- weekly_crime %>%
    mutate(closure_1_start_week = floor_date(closure_1, unit = "week",
                                             week_start = getOption('lubridate.week.start', 1)
    )) %>%
    mutate(closure_1_end_week = ceiling_date(closure_1_end - days(7), unit = "week",
                                             week_start = getOption('lubridate.week.start', 1)
    )) %>%
    mutate(closure_2_start_week = floor_date(closure_2, unit = "week",
                                             week_start = getOption('lubridate.week.start', 1)
    )) %>%
    mutate(closure_2_end_week = floor_date(closure_2_end, unit = "week",
                                           week_start = getOption('lubridate.week.start', 1)
    ))  %>%
    mutate(treatment_1 = ifelse(week >= closure_1_start_week & week < closure_1_end_week, 1, 0)) %>%
    mutate(treatment_2 = ifelse(week >= closure_2_start_week & week < closure_2_end_week, 1, 0))

  two_treatment_uni <- event_study_df %>%
    filter(!is.na(closure_2)) %>%
    mutate(treatment_1 = treatment_2) %>%
    mutate(closure_1 = closure_2) %>%
    mutate(closure_1_end = closure_2_end) %>%
    mutate(closure_1_end_week = closure_2_end_week) %>%
    mutate(closure_1_start_week = closure_2_start_week) %>%
    filter(university != "Louisiana State University and Agricultural & Mechanical College") %>% ## gets rid of LSU from 2nd
    mutate(university = paste(university,"_2", sep= ""))

  event_study <- bind_rows(event_study_df, two_treatment_uni)

  for (i in window) {
    colname <- paste("treatment_plus_", i, sep = "")
    event_study <- event_study %>%
      mutate(!!sym(colname) := (closure_1_end_week - days(7)) + weeks(i))
  }

  ## creating new columns called "closure_minus_" for the event window
  for (i in window) {
    colname <- paste("treatment_minus_", i, sep = "")
    event_study <- event_study %>%
      mutate(!!sym(colname) := closure_1_start_week -  weeks(i))
  }

  ## treatment_1 is my main treatment variable I am working with - getting rid of these
  ## to make it less confusing on myself
  event_study <- event_study %>%
    select(-treatment, -starts_with("treatment_2_"), -starts_with("treatment_1_"))


  event_study <- event_study %>%
    mutate(across(c(starts_with("treatment_minus"), starts_with("treatment_plus"),
    ), ~ifelse(. == week, 1, 0)))

  event_study <- event_study %>%
    mutate(treatment_minus_1 = 0)

  colname_minus <- paste("treatment_minus_", window_length, sep = "")
  colname_plus <- paste("treatment_plus_", window_length, sep = "")

  ## this will bin up the end points
  event_study <- event_study %>%
    mutate(!!sym(colname_minus) := case_when(
      closure_1_start_week - weeks(window_length) >= week & !is.na(!!sym(colname_minus))~1,
      closure_1_start_week - weeks(window_length) <= week & !is.na(!!sym(colname_minus)) ~0)) %>%
    mutate(!!sym(colname_plus) := case_when(
      (closure_1_end_week) + weeks(window_length- 1) <= week & !is.na(!!sym(colname_plus))~1,
      (closure_1_end_week) + weeks(window_length - 1) >= week & !is.na(!!sym(colname_plus)) ~0
    )) %>%
    rename("treatment" = treatment_1) %>%
    select(-treatment_2)
  return(event_study)
}
