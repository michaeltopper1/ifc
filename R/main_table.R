#' Gives an output dataframe ready for kableExtra
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
  options(modelsummary_model_labels="model")
  gm_first <- tribble(~raw, ~clean, ~fmt,
                      "nobs", "Observations", 0)
  gm <- tribble(~raw, ~clean, ~fmt,
                "nobs", "Observations", 0,
                "FE: day_of_week","FE: Day of Week", 3,
                "FE: holiday", "FE: Holiday", 3,
                "FE: semester_number", "FE: Semester by Year", 3,
                "FE: game_occurred", "FE: Game Day", 3,
                "FE: spring_semester", "FE: Semester (Spring/Fall)", 3,
                "FE: university","FE: University", 3,
                "FE: ori", "FE: Agency", 3,
                "FE: year", "FE: Year", 3,
                "FE: academic_year", "FE: Academic Year", 3,
                "FE: university_by_semester_number", "FE: University by Semester Number", 3,
                "FE: date", "FE: Day by Month by Year", 3,
                "FE: university_by_year_by_semester_number", "FE: University by Year by Semester Number", 3,
                "FE: university_by_academic_year", "FE: University by Academic Year", 3,
                "FE: day_of_week_by_month", "FE: Day of Week by Month", 3,
                "FE: university_by_day_of_week_by_month", "FE: University by Day of Week by Month", 3,
                "FE: university_by_academic_year_by_month_by_day_of_week", "FE: University by Academic Year by Month by Day of Week", 3,
                "FE: day_of_week_by_semester_number", "FE: Day of Week by Semester by Year", 3,
                "FE: university_by_month_by_academic_year", "FE: University by Month by Academic Year", 3,
                "FE: semester_by_academic_year", "FE: Semester by Academic Year", 3,
                "FE: university_by_academic_year_by_semester", "FE: University by Academic Year by Semester", 3,
                "FE: day_of_week_by_spring_semester", "FE: Day of Week by Semester", 3,
                "FE: day_of_week_by_semester_by_academic_year", "FE: Day of Week by Academic Year by Semester", 3,
                "FE: university_by_month", "FE: University by Month", 3,
                "FE: university_by_week", "FE: University by Week", 3,
                "FE: university_by_year", "FE: University by Year", 3,
                "FE: university_by_month_by_year", "FE: University by Month by Year", 3,
                "FE: university_by_year_by_week", "FE: University by Year by Week", 3,
                "FE: university_by_week_by_year", "FE: University by Week by Year", 3,
                "FE: ori_by_month", "FE: Agency by Month", 3,
                "FE: ori_by_week", "FE: Agency by Week", 3,
                "FE: ori_by_month_by_week", "FE: Agency by Month by Week", 3,
                "FE: ori_by_academic_year", "FE: Agency by Academic Year", 3,
                "FE: ori_by_academic_year_by_semester", "FE: Agency by Semester by Academic Year", 3)
  first_panels <- map_df(data, ~modelsummary(., stars = c('*' = .1, '**' = .05, '***' = .01), output = "data.frame",
                             coef_map = c("week_before" = "Week Before",
                                          "treatment" = "In Moratorium",
                                          "week_after" = "Week After",
                                          "game_occurred" = "Game Day",
                                          "home_game" = "Home Game",
                                          "game_occurred:home_game" = "Game Day x Home Game",
                                          "treatment:ifc_enacted" = "In Moratorium",
                                          "treatment:university_enacted" = "In Moratorium",
                                          "treatment:below_q33" = "In Moratorium",
                                          "treatment:between_q33_q66" = "In Moratorium",
                                          "treatment:above_q66" = "In Moratorium",
                                          "treatment:game_occurred" = "Moratorium x Game Day",
                                          "game_occurred:treatment" = "Moratorium x Game Day",
                                          "treatment:ifc_frac_first_quant" = "In Moratorium",
                                          "treatment:ifc_frac_second_quant" = "In Moratorium",
                                          "treatment:ifc_frac_third_quant" = "In Moratorium"),
                             gof_map = gm_first) %>%
             dplyr::mutate(term = ifelse(statistic == "std.error", "", term))  %>%
             select(-part, -statistic))
  final_panel <-  modelsummary(last_panel, stars = c('*' = .1, '**' = .05, '***' = .01), output = "data.frame",
                            coef_map = c("week_before" = "Week Before",
                                         "treatment" = "In Moratorium",
                                         "week_after" = "Week After",
                                         "game_occurred" = "Game Day",
                                         "home_game" = "Home Game",
                                         "game_occurred:home_game" = "Game Day x Home Game",
                                         "treatment:ifc_enacted" = "In Moratorium",
                                         "treatment:university_enacted" = "In Moratorium",
                                         "treatment:below_q33" = "In Moratorium",
                                         "treatment:between_q33_q66" = "In Moratorium",
                                         "treatment:above_q66" = "In Moratorium",
                                         "treatment:game_occurred" = "Moratorium x Game Day",
                                         "game_occurred:treatment" = "Moratorium x Game Day",
                                         "treatment:ifc_frac_first_quant" = "In Moratorium",
                                         "treatment:ifc_frac_second_quant" = "In Moratorium",
                                         "treatment:ifc_frac_third_quant" = "In Moratorium"),
                            gof_map = gm) %>%
    dplyr::mutate(term = ifelse(statistic == "std.error", "", term)) %>%
    select(-part, -statistic)
  table <- bind_rows(first_panels, final_panel)
  return(table)
}
