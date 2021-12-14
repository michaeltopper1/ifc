#' Returns list of universities which underwent a moratorium from 2014-2019
#'
#' @description
#' `moratorium_schools` returns a list of universities in which a moratorium has occurred due to death in addition to never-treated (but had death) schools.
#'
#' @details
#' This function simply returns a list of universities that went through moratorium. Does not rely on anything.
#'
#' @examples
#' moratorium_schools()
#'
#' @export moratorium_schools
#'


moratorium_schools <- function() {
  m_schools <- c('Arkansas State University-Main Campus',
                 'Ball State University',
                 'California Polytechnic State University-San Luis Obispo',
                 'Clemson University',
                 'College of Charleston',
                 'East Carolina University',
                 'Emory University',
                 'Florida Atlantic University',
                 'Florida International University',
                 'Florida State University',
                 'Indiana University-Bloomington',
                 'Louisiana State University and Agricultural & Mechanical College',
                 'Marshall University',
                 'Monmouth University',
                 'Murray State University',
                 'North Carolina State University at Raleigh',
                 'Ohio State University-Main Campus',
                 'Ohio University-Main Campus',
                 'Rollins College',
                 'Rutgers University-New Brunswick',
                 'San Diego State University',
                 'Syracuse University',
                 'Texas State University',
                 'Tufts University',
                 'University at Buffalo',
                 'University of California-Berkeley',
                 'University of Central Florida',
                 'University of Idaho',
                 'University of Iowa',
                 'University of Kansas',
                 'University of Michigan-Ann Arbor',
                 'University of Missouri-Columbia',
                 'University of New Mexico-Main Campus',
                 'University of Pittsburgh-Pittsburgh Campus',
                 'University of Vermont',
                 'University of Virginia-Main Campus',
                 'Washington State University',
                 'West Virginia University')
  return(m_schools)
}



