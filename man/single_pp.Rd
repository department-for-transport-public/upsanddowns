% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_pp.R
\name{single_pp}
\alias{single_pp}
\title{Return a commentary snippet for any decimal value.
Formats the number as a percentage point difference
and provides simple up/down commentary}
\usage{
single_pp(x, description = "up", abbr = TRUE, unchanged_limit, ...)
}
\arguments{
\item{x}{a numeric value, as a decimal value.
E.g. 10pp would be passed as 0.1.}

\item{description}{code of descriptive words used in commentary.
Defaults to "up"; up/down/unchanged.}

\item{abbr}{logical, whether you would like the units abbreviated to pp.
Defaults to TRUE.}

\item{unchanged_limit}{numeric, the value below which you consider
the parameter to not represent a change in either direction. Defaults to 0.01.}

\item{...}{additional arguments to pass to the round function}
}
\value{
Returns a brief commentary (including the figure
for increases or decreases) as a string
}
\description{
Return a commentary snippet for any decimal value.
Formats the number as a percentage point difference
and provides simple up/down commentary
}
