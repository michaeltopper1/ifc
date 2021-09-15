#' Returns list of universities which underwent a fraternity death in 2014-2019 but no moratorium.
#'
#' @description
#' `death_untreated_universities` returns a list of universities in which a moratorium has occurred due to death in addition to never-treated schools.
#'
#' @details
#' This function simply returns a list of schools.
#'
#' @examples
#' death_universities()
#'
#' @export death_untreated_universities
#'

death_untreated_universities <- function() {
  untreated_universities <- c("University of Kentucky",
                              "Hampden-Sydney College",
                              "University of Nevada-Reno",
                              "University of Nebraska-Lincoln",
                              "Texas Christian University",
                              "Texas Tech University",
                              "University of South Carolina-Columbia",
                              "Ferrum College",
                              "Texas A & M University-College Station",
                              "University of California-Riverside",
                              "Delaware State University",
                              "University of California-Irvine",
                              "California State University-Fullerton",
                              "SUNY at Albany",
                              "University of Louisiana at Lafayette",
                              "University of Southern California",
                              "University of California-Santa Cruz",
                              "The University of Texas at Austin",
                              "Bloomsburg University of Pennsylvania")
  return(untreated_universities)
}

