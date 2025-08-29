#' Paste together objects based on how many of them there are
#' Uses commas between everything except the last value
#'
#' @name smart_paste
#'
#' @param x a vector of two or more objects
#'
#'
smart_paste <- function(x) {
  if (length(x) < 2) {
    x <- x
  } else {
    ## Join everything except the last value with a comma
    comma_x <- paste(x[1:length(x) - 1],
      collapse = ", "
    )
    # Join the last value with an and

    x <- paste(comma_x, x[length(x)], sep = " and ")
  }

  return(x)
}

#' Return a data table of the description codes and examples of their use
#'
#' @name show_descriptions
#'
#' @export
#'
show_format <- function() {
  words[, c("code", "description words", "example")]
}

#' Round numbers according to statistical principles rather than data science ones
#' i.e. numbers ending in a 5 will always be rounded up, regardless of preceeding number
#
#' @param x a numeric value
#' @param digits integer indicating the number of decimal places.
#' Negative values are allowed; Rounding to a negative number of digits means rounding to a power of ten, so for example round(x, digits = -2) rounds to the nearest hundred.
#'
#' @name round
#' @export

round <- function(x, digits = 0){

  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg

}
