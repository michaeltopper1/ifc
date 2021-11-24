#' Finds the Average Length of IFC Moratoria.
#'
#' @description
#' `main_table` returns the main table broken down by subsets of fullsample/weekend/weekdays.
#'
#' @details
#' This function relies on dplyr, modelsummary, purrr, and tibble packages to work.
#'
#' @param ... fixest objects. This will be the regressions for however many fixest objects needed.
#' @param last_panel the final regression panel to put on. This will add on the fixed effects.
#'
#' @examples
#' main_table(full_sample, weekends, weekdays)
#'
#' @export main_table
#'

main_table <- function(..., last_panel){
  data <- list(...)
  gm_first <- tribble(~raw, ~clean, ~fmt,
                      "nobs", "Num.Obs", ~fmt)
  gm <- tribble(~raw, ~clean, ~fmt,
                "nobs", "Num.Obs", ~fmt,
                "FE: day_of_week","FE: Day-of-Week", ~fmt,
                "FE: semester_number", "FE: Semester-by-Year", ~fmt,
                "FE: university","FE: University", ~fmt,
                "FE: year", "FE: Year", ~fmt,
                "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
                "FE: date", "FE: Day-by-Month-by-Year", ~fmt,
                "FE: university_by_year_by_semester_number", "FE: University-by-Year-by-Semester-Number", ~fmt)
  first_panels <- map_df(data, ~modelsummary(., stars = T, output = "data.frame",
                             coef_map = c("lead_2" = "2 Weeks Before",
                                          "lead_1" = "Week Before",
                                          "treatment" = "In Moratorium",
                                          "lag_1" = "Week After",
                                          "lag_2" = "2 Weeks After"),
                             gof_map = gm_first) %>%
           mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>%
           select(matches("term|^model")))
  final_panel <-  modelsummary(last_panel, stars = T, output = "data.frame",
                            coef_map = c("lead_2" = "2 Weeks Before",
                                         "lead_1" = "Week Before",
                                         "treatment" = "Moratorium",
                                         "lag_1" = "Week After",
                                         "lag_2" = "2 Weeks After"),
                            gof_map = gm) %>%
    mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>%
    select(matches("term|^model"))
  table <- bind_rows(first_panels, final_panel)
  return(table)
}
