% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_percent.R
\name{single_percent}
\alias{single_percent}
\title{Return a commentary snippet for any decimal value.
Formats the number as a percentage and provides simple up/down commentary}
\usage{
single_percent(x, description = "up", unchanged_limit, ...)
}
\arguments{
\item{x}{a numeric value, as a decimal value.
E.g. 10\% would be passed as 0.1.}

\item{description}{code of descriptive words used in commentary.
Defaults to "up"; up/down/unchanged.}

\item{unchanged_limit}{numeric, the value below which you consider
the parameter to not represent a change in either direction. Defaults to 0.01.}

\item{...}{additional arguments to pass to the scales::percent function}
}
\value{
Returns a brief commentary (including the figure for
increases or decreases) as a string
}
\description{
Return a commentary snippet for any decimal value.
Formats the number as a percentage and provides simple up/down commentary
}
