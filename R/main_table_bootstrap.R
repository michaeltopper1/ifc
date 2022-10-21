#' Gives an output dataframe ready for kableExtra when using fwildclusterboot standard errors
#'
#' @description
#' `main_table_bootstrap` returns the main table when using fwildcluster boot standard errors
#'
#' @details
#' This function relies on dplyr, modelsummary, purrr, and tibble packages to work.
#'
#' @param ... fixest objects. This will be the regressions for however many fixest objects needed.
#' @param last_panel the final regression panel to put on. This will add on the fixed effects.
#' @param display_fe Whether or not to display the fixed effects chart in the table
#'
#' @examples
#' main_table_bootstrap(list(boostrap_1, boostrap_2), last_panel = list(boostrap_1, boostrap_2), display_fe = F)
#'
#' @export main_table_bootstrap
#'

main_table_bootstrap <- function(first_panel, last_panel, display_fe = T){
  gm_first <- tribble(~raw, ~clean, ~fmt,
                      "nobs", "Observations", ~fmt)
  gm <- tribble(~raw, ~clean, ~fmt,
                "nobs", "Observations", ~fmt,
                "FE: day_of_week","FE: Day of Week", ~fmt,
                "FE: holiday", "FE: Holiday", ~fmt,
                "FE: semester_number", "FE: Semester by Year", ~fmt,
                "FE: game_occurred", "FE: Game Day", ~fmt,
                "FE: spring_semester", "FE: Semester (Spring/Fall)", ~fmt,
                "FE: university","FE: University", ~fmt,
                "FE: ori", "FE: Agency", ~fmt,
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
                "FE: day_of_week_by_semester_by_academic_year", "FE: Day of Week by Academic Year by Semester", ~fmt,
                "FE: university_by_month", "FE: University by Month", ~fmt,
                "FE: university_by_week", "FE: University by Week", ~fmt,
                "FE: university_by_year", "FE: University by Year", ~fmt,
                "FE: university_by_month_by_year", "FE: University by Month by Year", ~fmt,
                "FE: university_by_year_by_week", "FE: University by Year by Week", ~fmt,
                "FE: university_by_week_by_year", "FE: University by Week by Year", ~fmt,
                "FE: ori_by_month", "FE: Agency by Month", ~fmt,
                "FE: ori_by_week", "FE: Agency by Week", ~fmt,
                "FE: ori_by_month_by_week", "FE: Agency by Month by Week", ~fmt,
                "FE: ori_by_academic_year", "FE: Agency by Academic Year", ~fmt,
                "FE: ori_by_academic_year_by_semester", "FE: Agency by Semester by Academic Year", ~fmt)
  first_panels <- modelsummary(first_panel,estimate = "{estimate}{stars}",
                                             statistic = "[{conf.low}, {conf.high}]",
                                             stars = c('*' = .1, '**' = .05, '***' = .01), output = "data.frame",
                                             coef_map = c("week_before" = "Week Before",
                                                          "1*treatment = 0" = "In Moratorium",
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
                           mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>%
                           select(-part, -statistic)
  final_panel <-  modelsummary(last_panel,  estimate = "{estimate}{stars}",
                               statistic = "[{conf.low}, {conf.high}]",
                               stars = c('*' = .1, '**' = .05, '***' = .01), output = "data.frame",
                               coef_map = c("week_before" = "Week Before",
                                            "1*treatment = 0" = "In Moratorium",
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
    mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>%
    select(-part, -statistic)
  table <- bind_rows(first_panels, final_panel)
  return(table)
}
