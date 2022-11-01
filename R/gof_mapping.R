#' Returns tibble of the goodness of fit mappings for tables.
#'
#' @description
#' `gof_mapping` returns a tibble of goodness of fit mappings for the tables.
#'
#' @details
#' This function simply returns a tibble. Does not rely on anything besides having the tibble package installed.
#'
#' @examples
#' gof_mapping()
#'
#' @export gof_mapping
#'

gof_mapping <- function(){
  gof_mapping <-  tibble::tribble(~raw, ~clean, ~fmt,
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
  return(gof_mapping)
}
