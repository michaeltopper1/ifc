#' Creates the Event Study tibble for analysis at Daily Level Data.
#'
#' @description
#' `event_study_stagger` returns a tibble ready for event study analysis in units of days with staggered adoption
#'
#' @details
#' This function creates the event study tibble needed to do analysis in event study framework.
#'
#'
#' @param data an object/dataframe/tibble (daily_crime).
#' @param leads_lags how big the event window is.
#' @param clusters a number for how many days you want clustered together.
#'
#' @examples
#' event_study_stagger(daily_crime, leads_lags = 8, bin_length =7)
#'
#' @export event_study_stagger
#'
#'
event_study_stagger<- function(data, leads_lags, clusters) {
  leead <- function(x, v){
    xx <- rep(0, length(x))
    for(i in v){
      xx <- xx + lead(x, i)
    }
    xx[is.na(xx)] <- 0
    xx
  }

  laag <- function(x, v){
    xx <- rep(0, length(x))
    for(i in v){
      xx <- xx + dplyr::lag(x, i)
    }
    xx[is.na(xx)] <- 0
    xx
  }

  ## Had to change florida's date since their closure date was outside of their academic calendar I have.
  es <- data %>%
    group_by(university) %>%
    arrange(date) %>%
    mutate(first_day_moratorium = case_when(
      closure_1 == date ~ 1,
      closure_2 == date ~ 1,
      closure_3 == date ~ 1,
      university == "Florida International University" & date == "2018-01-04" ~ 1,
      TRUE ~as.double(0)))


  leads <- leads_lags
  lags <- leads + 1 ## since we need to put in a treatment variable

  ## determining how many days you want to be in each lag and lead bin
  bin_length <- clusters


  starts <- seq(from = 1, to = leads*bin_length, by = bin_length)

  lead_counts <- seq(from = bin_length, to = leads *bin_length, by = bin_length)


  ## to accomodate the fact that I need to start with my "beta_0", I needed to add to the length of these
  ## next vectors so that my 5 lags would in fact be 1 treatment "beta_0" and 5 lags.
  ## moreover, I needed to substract by 1 since i wanted the laag function to start at the first day of moratorium
  starts_lag <- seq(from = 1, to = (lags +1)*bin_length, by = bin_length) - 1
  lag_counts <- seq(from = bin_length, to = (lags + 1)*bin_length, by = bin_length) - 1

  for (i in 1:leads){
    name <- paste0("beta_lead_", i)
    es <- es %>%
      mutate(!!sym(name) := leead(first_day_moratorium, c(starts[i]:lead_counts[i])))

  }


  for (i in 1:lags) {
    if (i == 1) {
      name <- "beta_0"
      es <- es %>%
        mutate(!!sym(name) := laag(first_day_moratorium, c(starts_lag[i]:lag_counts[i])))
    }
    else{
      name <- paste0("beta_lag_", i -1)
      es <- es %>%
        mutate(!!sym(name) := laag(first_day_moratorium, c(starts_lag[i]:lag_counts[i])))
    }
  }

  ## binning endpoints cumulatively
  last_lag <- paste0("beta_lag_", lags -1)
  es <- es %>%
    relocate(starts_with("beta_lag_"), treatment, date, university) %>%
    ungroup() %>%
    group_by(university) %>%
    arrange(date) %>%
    mutate(beta_lag_binned = cumsum(!!sym(last_lag))) %>%
    relocate(beta_lag_binned) %>%
    ungroup()


  ## binning endpoints cumulatively
  first_lead <- paste0("beta_lead_", leads)
  es <- es %>%
    relocate(starts_with("beta_lead_"), treatment, date, university) %>%
    ungroup() %>%
    group_by(university) %>%
    arrange(desc(date)) %>%
    mutate(beta_lead_binned = cumsum(!!sym(first_lead))) %>%
    relocate(beta_lead_binned) %>%
    ungroup()

  ## changing end points
  es <- es %>%
    group_by(university) %>%
    arrange(date) %>%
    mutate(across(c(beta_lead_binned, beta_lag_binned), ~case_when(
      . >= 1 & . <= bin_length ~1,
      . > bin_length & . <=bin_length *2 ~ 2,
      . > bin_length*2 ~3,
      TRUE ~as.double(0))
    )) %>%
    # mutate(across(c(beta_lead_binned, beta_lag_binned), ~case_when(
    #   . >= 1 ~1,
    #   TRUE ~as.double(0))
    # )) %>%
    ungroup()

  return(es)
}
