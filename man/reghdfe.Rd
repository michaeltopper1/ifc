% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reghdfe.R
\name{reghdfe}
\alias{reghdfe}
\title{High dimension fixed effects regression (OLS) with fixest package.}
\usage{
reghdfe(
  data,
  outcome,
  explanatory_vars,
  fixed_effects = NULL,
  cluster,
  weights = NULL
)
}
\arguments{
\item{data}{tibble or dataframe.}

\item{outcome}{outcome of interest. Must be a string.}

\item{explanatory_vars}{vector of explanatory variables. Must be a string.}

\item{fixed_effects}{a vector of fixed effects. Must be a string. Defaults to NULL for no fixed effects.}

\item{cluster}{column to cluster by. Must be a string.}

\item{weights}{column to weight by.}
}
\description{
\code{reghdfe} returns the fixest object consisting of the regression equation.
}
\details{
This function calls the fixest::feols() function, but simiplifies the typing.
}
\examples{
reghdfe(mtcars, "mpg", "hp", "am", "am")

}
