#' Returns list of universities which were never treated and never had a fraternity death from 2014-2019.
#'
#' @description
#' `never_treated_no_death` returns a list of universities in which are on the top 50 greek schools but did not have a death nor a moratorium.
#'
#' @details
#' This function simply returns a list of universities. Does not rely on anything.
#'
#' @examples
#' never_treated_no_death()
#'
#' @export never_treated_no_death
#'


never_treated_no_death <- function() {
  never_treated_nd <- c("Albany State University",
                        "University of Arizona",
                        "California State University-Chico",
                        "DePauw University",
                        "James Madison University",
                        "Michigan State University",
                        "University of Mississippi",
                        "North Carolina Central University",
                        "Prairie View A & M University",
                        "Southern Methodist University",
                        "University of Arkansas",
                        "University of California-Santa Barbara",
                        "University of Delaware",
                        "University of Illinois Urbana-Champaign",
                        "University of Wisconsin-Madison")
  return(never_treated_nd)
}
