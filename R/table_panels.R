#' Gets a dataframe of modelsummary tables pasted.
#'
#' @description
#' `table_panels` returns a table of pasted together data frames.
#'
#' @details
#' This function relies on dplyr, modelsummary, and tibble packages to work.
#'
#' @param ... as many models as you like!
#'
#' @examples
#' table_panels(full_sample, weekends, weekdays)
#'
#' @export table_panels
#'

table_panels <- function(...) {
  data <- list(...)
  gm <- tribble(~raw, ~clean, ~fmt,
                "nobs", "Observations", ~fmt,
                "FE: day_of_week","FE: Day of Week", ~fmt,
                "FE: semester_number", "FE: Semester by Year", ~fmt,
                "FE: university","FE: University", ~fmt,
                "FE: year", "FE: Year", ~fmt,
                "FE: university_by_semester_number", "FE: University by Semester Number", ~fmt,
                "FE: date", "FE: Day by Month by Year", ~fmt,
                "FE: university_by_year_by_semester_number", "FE: University by Year by Semester Number", ~fmt,
                "FE: university_by_academic_year", "FE: University by Academic Year", ~fmt,
                "FE: day_of_week_by_month", "FE: Day of Week by Month", ~fmt,
                "FE: university_by_day_of_week_by_month", "FE: University by Day of Week by Month", ~fmt,
                "FE: university_by_academic_year_by_month_by_day_of_week", "FE: University by Academic Year by Month by Day of Week", ~fmt,
                "FE: academic_year", "FE: Academic Year", ~fmt,
                "FE: spring_semester", "FE: Semester (Spring/Fall)", ~fmt,
                "FE: day_of_week_by_semester_number", "FE: Day of Week by Semester by Year", ~fmt,
                "FE: university_by_month_by_academic_year", "FE: University by Month by Academic Year", ~fmt,
                "FE: semester_by_academic_year", "FE: Semester by Academic Year", ~fmt,
                "FE: university_by_academic_year_by_semester", "FE: University by Academic Year by Semester", ~fmt,
                "FE: day_of_week_by_spring_semester", "FE: Day of Week by Semester", ~fmt,
                "FE: day_of_week_by_semester_by_academic_year", "FE: Day of Week by Academic Year by Semester", ~fmt)
  full <- map(data, ~modelsummary(..., stars = T, output = "data.frame",
                       coef_map = c("lead_2" = "2 Weeks Before",
                                    "lead_1" = "Week Before",
                                    "treatment" = "In Moratorium",
                                    "lag_1" = "Week After",
                                    "lag_2" = "2 Weeks After"),
                       gof_map = gm) %>%
    mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>%
    select(matches("term|^model"))) %>%
    reduce(bind_rows)
  return(full)
}





