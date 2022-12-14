% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_change.R
\name{pp_change}
\alias{pp_change}
\title{Return a commentary snippet for one or more decimal values.
Formats the numbers as percentage points and provides
simple up/down commentary}
\usage{
pp_change(x, description = "up", abbr = TRUE, ...)
}
\arguments{
\item{x}{a vector of percentage values, as a decimal
E.g. 10pp would be passed as 0.1.}

\item{description}{code of descriptive words used in commentary.
Defaults to "up"; up/down/unchanged.}

\item{abbr}{logical, whether you would like the units abbreviated to pp.
Defaults to TRUE.}

\item{...}{additional arguments to pass to the scales::percent function}
}
\value{
Returns a brief commentary (including the figure for
increases or decreases) as a string
}
\description{
Return a commentary snippet for one or more decimal values.
Formats the numbers as percentage points and provides
simple up/down commentary
}
