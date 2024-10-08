% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percent_change.R
\name{percent_change}
\alias{percent_change}
\title{Return a commentary snippet for one or more decimal values.
Formats the numbers as a percentage and provides simple up/down commentary}
\usage{
percent_change(x, description = "up", unchanged_limit = 0.01, ...)
}
\arguments{
\item{x}{a vector of percentage values, as a decimal
E.g. 10\% would be passed as 0.1.}

\item{description}{code of descriptive words used in commentary.
Defaults to "up"; up/down/unchanged.}

\item{unchanged_limit}{numeric, the value below which you consider
the parameter to not represent a change in either direction. Defaults to 0.01 (equal to 1\%).}

\item{...}{additional arguments to pass to the scales::percent function}
}
\value{
Returns a brief commentary
(including the figure for increases or decreases)
as a string
}
\description{
Return a commentary snippet for one or more decimal values.
Formats the numbers as a percentage and provides simple up/down commentary
}
