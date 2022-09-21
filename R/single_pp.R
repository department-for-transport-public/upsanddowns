#' Return a commentary snippet for any decimal value.
#' Formats the number as a percentage point difference
#' and provides simple up/down commentary
#'
#' @name single_pp
#'
#' @param x a numeric value, as a decimal value.
#' E.g. 10pp would be passed as 0.1.
#' @param description code of descriptive words used in commentary.
#' Defaults to "up"; up/down/unchanged.
#' @param abbr logical, whether you would like the units abbreviated to pp.
#' Defaults to TRUE.
#' @param ... additional arguments to pass to the round function
#'
#' @importFrom stringr str_trim
#' @importFrom dftutils round
#'
#' @return Returns a brief commentary (including the figure
#' for increases or decreases) as a string
#'
single_pp <- function(x, description = "up", abbr = TRUE, ...) {

  ## pick the words we're using
  words <- words[words$code == description, ]

  ## Choose what percentage points are written as
  unit <- if (abbr) {
    "pp"
  } else if (round(abs(x * 100)) == 1) {
    " percentage point"
  } else {
    " percentage points"
  }

  # Stop if not a number
  if (!is.numeric(x)) {
    stop("Value provided is not numeric")
  }


  if (x >= 0.01) {
    comm <- paste(
      words$up_pre,
      paste0(
        dftutils::round(x * 100, ...), unit
      ),
      words$up_post
    )
  } else if (x <= - 0.01) {
    comm <- paste(
      words$down_pre,
      paste0(abs(
        dftutils::round(x * 100, ...)
      ), unit),
      words$down_post
    )
  } else {
    comm <- paste(words$same)
  }

  return(stringr::str_trim(comm))
}
