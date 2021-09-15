#' Returns list of universities which underwent a fraternity death in 2014-2019.
#'
#' @description
#' `death_universities` returns a list of universities in which a moratorium has occurred due to death in addition to never-treated (but had death) schools.
#'
#' @details
#' This function simply returns a list of universities. Does not rely on anything.
#'
#' @examples
#' death_universities()
#'
#' @export death_universities
#'

death_universities <- function() {
  death_universities <- c("Florida State University",
                          "Louisiana State University and Agricultural & Mechanical College",
                          "Murray State University",
                          "Texas State University",
                          "San Diego State University",
                          "University at Buffalo",
                          "University of Iowa",
                          "University of Vermont",
                          "Washington State University",
                          "West Virginia University",
                          "University of Kentucky",
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
  return(death_universities)
}


