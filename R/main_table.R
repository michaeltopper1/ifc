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
                      "nobs", "Observations", ~fmt)
  gm <- tribble(~raw, ~clean, ~fmt,
                "nobs", "Observations", ~fmt,
                "FE: day_of_week","FE: Day of Week", ~fmt,
                "FE: holiday", "FE: Holiday", ~fmt,
                "FE: semester_number", "FE: Semester by Year", ~fmt,
                "FE: spring_semester", "FE: Semester (Spring/Fall)", ~fmt,
                "FE: university","FE: University", ~fmt,
                "FE: year", "FE: Year", ~fmt,
                "FE: academic_year", "FE: Academic Year", ~fmt,
                "FE: university_by_semester_number", "FE: University by Semester Number", ~fmt,
                "FE: date", "FE: Day by Month by Year", ~fmt,
                "FE: university_by_year_by_semester_number", "FE: University by Year by Semester Number", ~fmt,
                "FE: university_by_academic_year", "FE: University by Academic Year", ~fmt,
                "FE: day_of_week_by_month", "FE: Day of Week by Month", ~fmt,
                "FE: university_by_day_of_week_by_month", "FE: University by Day of Week by Month", ~fmt,
                "FE: university_by_academic_year_by_month_by_day_of_week", "FE: University by Academic Year by Month by Day of Week", ~fmt,
                "FE: day_of_week_by_semester_number", "FE: Day of Week by Semester by Year", ~fmt,
                "FE: university_by_month_by_academic_year", "FE: University by Month by Academic Year", ~fmt,
                "FE: semester_by_academic_year", "FE: Semester by Academic Year", ~fmt,
                "FE: university_by_academic_year_by_semester", "FE: University by Academic Year by Semester", ~fmt,
                "FE: day_of_week_by_spring_semester", "FE: Day of Week by Semester", ~fmt,
                "FE: day_of_week_by_semester_by_academic_year", "FE: Day of Week by Academic Year by Semester", ~fmt)
  first_panels <- map_df(data, ~modelsummary(., stars = T, output = "data.frame",
                             coef_map = c("week_before" = "Week Before",
                                          "treatment" = "In Moratorium",
                                          "week_after" = "Week After",
                                          "treatment:ifc_enacted" = "In Moratorium",
                                          "treatment:university_enacted" = "In Moratorium",
                                          "treatment:below_q33" = "In Moratorium",
                                          "treatment:between_q33_q66" = "In Moratorium",
                                          "treatment:above_q66" = "In Moratorium"),
                             gof_map = gm_first) %>%
           mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>%
           select(matches("term|^model")))
  final_panel <-  modelsummary(last_panel, stars = T, output = "data.frame",
                            coef_map = c("week_before" = "Week Before",
                                         "treatment" = "In Moratorium",
                                         "week_after" = "Week After",
                                         "treatment:ifc_enacted" = "In Moratorium",
                                         "treatment:university_enacted" = "In Moratorium",
                                         "treatment:below_q33" = "In Moratorium",
                                         "treatment:between_q33_q66" = "In Moratorium",
                                         "treatment:above_q66" = "In Moratorium"),
                            gof_map = gm) %>%
    mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>%
    select(matches("term|^model"))
  table <- bind_rows(first_panels, final_panel)
  return(table)
}
