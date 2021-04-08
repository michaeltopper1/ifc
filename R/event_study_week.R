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
#' @param reference_week a number for which year you want to be the reference year. It will be paste0("treatment_minus_", reference_year)
#'
#' @examples
#' event_study_week(weekly_panel,  4, 1)
#'
#' @export event_study_week
#'


event_study_week <- function(data, window_length, reference_week) {
  window <- c(1:window_length)
  weekly_crime <- data %>%
    mutate(across(c(closure_1_end, closure_2_end), ~ as.character(.))) %>%
    mutate(closure_1_end = ifelse(university == "Louisiana State University and Agricultural & Mechanical College",
                                  closure_2_end, closure_1_end)) %>%
    mutate(across(c(closure_1_end, closure_2_end, closure_2), ~as.Date(.)))

  ## getting only the columns I want
  weekly_crime <- weekly_crime %>%
    select(university, starts_with("closure_1"), treatment, alcohol_offense, sexual_assault,
           robbery_burglary, theft, drug_offense, week, total_students_all,
           ftime_total_undergrad, total_undergrad_asian,
           total_undergrad_black, total_undergrad_hispanic, graduation_rate_total_cohort_)

  ## getting two closure schools
  ## rename the columns so that everything is standardized by closure_1 and closure_1_end
  two_closure_schools <- data %>%
    select(university, closure_2, closure_2_end, treatment, alcohol_offense, sexual_assault,
           robbery_burglary, theft, drug_offense,  week, total_students_all,
           ftime_total_undergrad, total_undergrad_asian,
           total_undergrad_black, total_undergrad_hispanic, graduation_rate_total_cohort_) %>%
    mutate(university = ifelse(!is.na(closure_2),paste0(university, "_2"), university)) %>%
    rename(closure_1 = closure_2, closure_1_end = closure_2_end) %>%
    filter(str_detect(university, "_2$")) %>%
    filter(university != "Louisiana State University and Agricultural & Mechanical College_2")

  ## append the two together
  event_study <- bind_rows(weekly_crime, two_closure_schools)

  event_study <- event_study %>%
    mutate(week_id = wday(week, label = T)) %>%
    mutate(closure_1_floor = floor_date(closure_1, unit = "week",
                                        week_start = getOption('lubridate.week.start', 1)
    )) %>%
    mutate(closure_1_ceiling = ceiling_date(closure_1_end, unit = "week",
                                            week_start = getOption('lubridate.week.start', 1)))
  event_study <- event_study %>%
    mutate(treatment = ifelse(week >= closure_1_floor & week < closure_1_ceiling,1, 0))

  for (i in window) {
    colname <- paste("treatment_plus_", i, sep = "")
    event_study <- event_study %>%
      mutate(!!sym(colname) := (closure_1_ceiling - weeks(1)) + weeks(i))
  }

  for (i in window) {
    colname <- paste("treatment_minus_", i, sep = "")
    event_study <- event_study %>%
      mutate(!!sym(colname) := closure_1_floor -  weeks(i))
  }

  event_study <- event_study %>%
    mutate(across(c(starts_with("treatment_minus"), starts_with("treatment_plus"),
    ), ~ifelse(. == week, 1, 0)))



  colname_minus <- paste("treatment_minus_", window_length, sep = "")
  colname_plus <- paste("treatment_plus_", window_length, sep = "")


  event_study <- event_study %>%
    mutate(!!sym(colname_minus) := case_when(
      closure_1_floor - weeks(window_length) >= week & !is.na(!!sym(colname_minus))~1,
      closure_1_floor - weeks(window_length) <= week & !is.na(!!sym(colname_minus)) ~0)) %>%
    mutate(!!sym(colname_plus) := case_when(
      (closure_1_ceiling) + weeks(window_length- 1) <= week & !is.na(!!sym(colname_plus))~1,
      (closure_1_ceiling) + weeks(window_length - 1) >= week & !is.na(!!sym(colname_plus)) ~0
    ))

  reference_week <- paste0("treatment_minus_", reference_week)

  event_study <- event_study %>%
    mutate(!!sym(reference_week) := 0)

  event_study <- event_study %>%
    mutate(across(c(sexual_assault, alcohol_offense,
                    theft, robbery_burglary, drug_offense), ~./total_students_all * 100000,
                  .names = '{.col}_per100')) %>%
    mutate(year = year(week), month = month(week)) %>%
    group_by(university, month) %>%
    mutate(uni_month = cur_group_id()) %>%
    ungroup()

  return(event_study)
}
