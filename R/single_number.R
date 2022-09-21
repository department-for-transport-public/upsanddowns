#' Return a commentary snippet for any number value.
#' Formats the number with comma separation
#' and provides simple up/down commentary
#'
#' @name single_number
#'
#' @param x a numeric value.
#' @param description code of descriptive words used in commentary.
#' Defaults to "up"; up/down/unchanged.
#' @param unchanged_limit numeric, the value below which you
#' consider the parameter to not represent a change in either direction.
#' Defaults to 100.
#' @param ... additional arguments to pass to the scales::comma function
#'
#' @importFrom stringr str_trim
#' @importFrom scales comma
#'
#' @return Returns a brief commentary (including the figure for
#' increases or decreases) as a string
#'
single_number <- function(x, description = "up", unchanged_limit = 100, ...) {

  ## pick the words we're using
  words <- words[words$code == description, ]

  # Stop if not a number
  if (!is.numeric(x)) {
    stop("Value provided is not numeric")
  }

  # Check number against specified limit
  if (x >= unchanged_limit) {
    comm <- paste(
      words$up_pre,
      scales::comma(x, ...),
      words$up_post
    )
  } else if (x <= (unchanged_limit * - 1)) {
    comm <- paste(
      words$down_pre,
      scales::comma(abs(x), ...),
      words$down_post
    )
  } else {
    comm <- paste(words$same)
  }

  return(stringr::str_trim(comm))
}
