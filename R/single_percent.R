#' Return a commentary snippet for any decimal value.
#' Formats the number as a percentage and provides simple up/down commentary
#'
#' @name single_percent
#'
#' @param x a numeric value, as a decimal value.
#' E.g. 10\% would be passed as 0.1.
#' @param description code of descriptive words used in commentary.
#' Defaults to "up"; up/down/unchanged.
#' @param ... additional arguments to pass to the scales::percent function
#'
#' @importFrom scales percent
#' @importFrom stringr str_trim
#'
#' @return Returns a brief commentary (including the figure for
#' increases or decreases) as a string
#'
single_percent <- function(x, description = "up", ...) {

  ## pick the words we're using
  words <- words[words$code == description, ]

  # Stop if not a number
  if (!is.numeric(x)) {
    stop("Value provided is not numeric")
  }

  if (x >= 0.01) {
    comm <- paste(
      words$up_pre,
      scales::percent(x, ...),
      words$up_post
    )
  } else if (x <= - 0.01) {
    comm <- paste(
      words$down_pre,
      scales::percent(abs(x), ...),
      words$down_post
    )
  } else {
    comm <- paste(words$same)
  }

  return(stringr::str_trim(comm))
}
