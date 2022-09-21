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
