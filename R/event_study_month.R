#' Creates the Event Study tibble for Analysis.
#'
#' @description
#' `event_study_month` returns a tibble ready for event study analysis in units of months.
#'
#' @details
#' This function creates the event study tibble needed to do analysis in event study framework.
#' Moreover, the month prior to treatment is the reference month (e.g. set to zero), and all months with
#' any amount of treatment are considered a treated month (e.g. 2 weeks in March = full month of March treated).
#'
#'
#' @param data an object/dataframe/tibble.
#' @param year_min the column in the object that contains the universities.
#' @param year_max the column in the object that contains the start date of the first moratorium.
#' @param window_length the column in the object that contains the  end date of the first moratorium.
#'
#'
#' @examples
#' event_study_month(monthly_crime, 2014, 2019, 4)
#'
#' @export event_study_month
#'

event_study_month <- function(data, year_min, year_max, window_length) {
  window <- c(1:window_length)
  ## first need to deal with LSU - it has a weird 1 week pause inbetween moratoria - changing closure 1 date to closure 2 date
  monthly_crime <- data %>%
    mutate(across(c(closure_1_end, closure_2_end), ~ as.character(.))) %>%
    mutate(closure_1_end = ifelse(university == "Louisiana State University and Agricultural & Mechanical College",
                                  closure_2_end, closure_1_end)) %>%
    mutate(across(c(closure_1_end, closure_2_end), ~as.Date(.)))

  event_study_df <- monthly_crime %>%
    mutate(treatment_1_month = month(closure_1), treatment_1_year = year(closure_1),
           treatment_2_month = month(closure_2), treatment_2_year = year(closure_2)) %>%
    mutate(date = ymd(paste(year,"-", month, "-1", sep = ""))) %>%
    mutate(closure_1_rounded = floor_date(closure_1, "month"),
           closure_2_rounded = floor_date(closure_2, "month"),
           closure_1_end_rounded = ceiling_date(closure_1_end-1, "month"),
           closure_2_end_rounded = ceiling_date(closure_2_end-1, "month")) %>%  ## hence, the month before is the treated month
    mutate(treatment_1 = ifelse((date >= closure_1_rounded & date < closure_1_end_rounded),1,0)) %>%
    mutate(treatment_2 = ifelse((date >= closure_2_rounded & date < closure_2_end_rounded),1,0))
  two_treatment_uni <- event_study_df %>%
    filter(!is.na(closure_2)) %>%
    mutate(treatment_1 = treatment_2) %>%
    mutate(closure_1 = closure_2) %>%
    mutate(closure_1_end = closure_2_end) %>%
    mutate(closure_1_end_rounded = closure_2_end_rounded) %>%
    mutate(closure_1_rounded = closure_2_rounded) %>%
    filter(university != "Louisiana State University and Agricultural & Mechanical College") %>% ## gets rid of LSU from 2nd
    mutate(university = paste(university,"_2", sep= ""))
  ## binding the two data frames together so that now i have a combination data frame
  ## Note here I changed  the closure_2_end to a floor date. This is to get the correct
  ## addition on the next month. For instance, ending in 4/13/18 would mean floor date is 4/1/18
  ## Hence, I want to count plus one from here, not the original ceiling date which would be 5/1/18 + 1
  event_study <- bind_rows(event_study_df, two_treatment_uni)

  ## creating new columns called "closure_plus_" which are 1 month bigger than the closure date rounded
  ## note that !!sym() says to R "get rid of the quotes and run as is"
  ## the := is the only way this works- still not sure why i have to use it
  for (i in window) {
    colname <- paste("closure_plus_", i, sep = "")
    event_study <- event_study %>%
      mutate(!!sym(colname) := (closure_1_end_rounded-1) %m+% months(i))
  }

  ## creating new columns called "closure_minus_" for the event window
  for (i in window) {
    colname <- paste("closure_minus_", i, sep = "")
    event_study <- event_study %>%
      mutate(!!sym(colname) := closure_1_rounded %m-% months(i))
  }

  ## treatment_1 is my main treatment variable I am working with - getting rid of these
  ## to make it less confusing on myself
  event_study <- event_study %>%
    select(-treatment, -starts_with("treatment_2_"), -starts_with("treatment_1_"))

  ## reording the columns so I can use them in my loop with colnames(). This is absolutely necessary
  event_study <- event_study %>%
    relocate(starts_with("closure_plus_"))

  ## creating the treatment indicators for the event study
  ## note that I make the expression "if closure_plus_ matches the year and month (e.g. the date) then set it to 1, if
  ## it is within the bounds of the year min and year max, set it to 0
  ## if it is outside of the bounds, give it NA

  for (i in window) {
    colname <- paste("treatment_plus_", i, sep = "")
    event_study <- event_study %>%
      mutate(!!sym(colname) := case_when(
        year(!!sym(colnames(event_study)[i])) == year & month(!!sym(colnames(event_study)[i])) == month ~ 1,
        year(!!sym(colnames(event_study)[i])) >= year_min & year(!!sym(colnames(event_study)[i])) <= year_max ~ 0
      ))
  }

  ## this must be ran before the for-loop. the for-loop depends on ordering
  event_study <- event_study %>%
    relocate(starts_with("closure_minus_"))

  for (i in window) {
    colname <- paste("treatment_minus_", i, sep = "")
    event_study <- event_study %>%
      mutate(!!sym(colname) := case_when(
        year(!!sym(colnames(event_study)[i])) == year & month(!!sym(colnames(event_study)[i])) == month ~ 1,
        year(!!sym(colnames(event_study)[i])) >= year_min & year(!!sym(colnames(event_study)[i])) <= year_max ~ 0
      ))
  }

  event_study <- event_study %>%
    select(-starts_with("closure_minus_"), - starts_with("closure_plus_"))

  ## setting the treatment_minus_1 period to zero:
  event_study <- event_study %>%
    mutate(treatment_minus_1 = 0)


  colname_minus <- paste("treatment_minus_", window_length, sep = "")
  colname_plus <- paste("treatment_plus_", window_length, sep = "")
  ## this will bin up the end points
  event_study <- event_study %>%
    mutate(!!sym(colname_minus) := case_when(
      closure_1_rounded %m-% months(window_length) >= date & !is.na(!!sym(colname_minus))~1,
      closure_1_rounded %m-% months(window_length) <= date & !is.na(!!sym(colname_minus)) ~0)) %>%
    mutate(!!sym(colname_plus) := case_when(
      (closure_1_end_rounded) %m+% months(window_length- 1) <= date & !is.na(!!sym(colname_plus))~1,
      (closure_1_end_rounded) %m+% months(window_length - 1) >= date & !is.na(!!sym(colname_plus)) ~0
    )) %>%
    rename("treatment" = treatment_1) %>%
    select(-treatment_2)
  return(event_study)
}

