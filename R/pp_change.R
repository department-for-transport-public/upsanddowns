#' Return a commentary snippet for one or more decimal values.
#' Formats the numbers as percentage points and provides
#' simple up/down commentary
#'
#' @name pp_change
#' @export
#'
#' @param x a vector of percentage values, as a decimal
#' E.g. 10pp would be passed as 0.1.
#' @param description code of descriptive words used in commentary.
#' Defaults to "up"; up/down/unchanged.
#' @param abbr logical, whether you would like the units abbreviated to pp.
#' Defaults to TRUE.
#' @param unchanged_limit numeric, the value below which you consider
#' the parameter to not represent a change in either direction. Defaults to 0.01.
#' @param ... additional arguments to pass to the scales::percent function
#'
#' @importFrom stringr str_trim
#' @importFrom dftutils round
#' @importFrom purrr map
#'
#' @return Returns a brief commentary (including the figure for
#' increases or decreases) as a string
#'
pp_change <- function(x, description = "up", abbr = TRUE, unchanged_limit = 0.01, ...) {
  if (!description %in% words$code) {
    stop(
      paste0(
        "Description identifier not found! Possible description codes are ",
        paste(words$code, collapse = ", "),
        "Run show_format() for more details."
      )
    )
  }

  ## pick the words we're using
  words <- words[words$code == description, ]

  unit <- if (abbr) {
    "pp"
  } else {
    " percentage points"
  }

  # Stop if not a number
  if (!is.numeric(x)) {
    stop("Values provided are not numeric")
  }

  ## If there's only one value, just do single change on it
  if (length(x) == 1) {
    single_pp(x, description = description, 
              abbr = abbr,
              unchanged_limit = unchanged_limit,
              ...
              )
  } else {

    ## If all numbers are the same, we're only doing one lot of commentary
    if (all(x >= unchanged_limit)) {
      comm <- paste(
        words$up_pre,
        smart_paste(
          paste0(
            dftutils::round(x * 100, ...), unit
          )
        ),
        words$up_post,
        "respectively"
      )
    } else if (all(x <= - unchanged_limit)) {
      comm <- paste(
        words$down_pre,
        smart_paste(
          paste0(abs(
            dftutils::round(x * 100, ...)
          ), unit)
        ),
        words$down_post,
        "respectively"
      )
      # Else just crack out two or more percent_change statements
    } else if (all(!(x > unchanged_limit | x <= - unchanged_limit))) {
      comm <- paste("both", words$same)
    } else {
      comm <- paste(smart_paste(purrr::map(x,
        single_pp,
        description = description,
        unchanged_limit = unchanged_limit,
        abbr = abbr,
        ...
      )), "respectively")
    }


    return(
      stringr::str_trim(
        gsub("  ", " ", comm)
      )
    )
  }
}
