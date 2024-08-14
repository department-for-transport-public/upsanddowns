#' Return a commentary snippet for one or more values.
#' Formats the numbers as a comma separated numeric and provides
#' simple up/down commentary
#' @name number_change
#' @export
#' @param x a vector of numeric values.
#' @param description code of descriptive words used in commentary.
#' Defaults to "up"; up/down/unchanged.
#' @param unchanged_limit numeric, the value below which you consider
#' the parameter to not represent a change in either direction. Defaults to 100.
#' @param ... additional arguments to pass to the scales::comma function
#' @importFrom scales comma
#' @importFrom stringr str_trim
#' @importFrom purrr map
#' @return Returns a brief commentary (including the figure
#' for increases or decreases) as a string

number_change <- function(x, description = "up", unchanged_limit = 100, ...) {
  if (!description %in% words$code) {
    stop(
      paste0(
        "Description identifier not found!",
        "Permitted description codes are ",
        paste(words$code, collapse = ", "),
        "Run show_format() for more details."
      )
    )
  }

  ## pick the words we're using
  words <- words[words$code == description, ]

  # Stop if not a number
  if (!is.numeric(x)) {
    stop("Values provided are not numeric")
  }

  ## If there's only one value, just do single change on it
  if (length(x) == 1) {
    single_number(x,
      description = description,
      unchanged_limit = unchanged_limit, ...
    )
  } else {

    ## If all numbers are the same, we're only doing one lot of commentary
    if (all(x >= unchanged_limit)) {
      comm <- paste(
        words$up_pre,
        smart_paste(
          scales::comma(x, ...)
        ),
        words$up_post,
        "respectively"
      )
    } else if (all(x <= (unchanged_limit * - 1))) {
      comm <- paste(
        words$down_pre,
        smart_paste(
          scales::comma(
            abs(x), ...
          )
        ),
        words$down_post,
        "respectively"
      )
      # Else just crack out two or more percent_change statements
    } else if (all(!(x > unchanged_limit | x <= - unchanged_limit)) & length(x) > 2) {
      comm <- paste("all", words$same)
    } else if (all(!(x > unchanged_limit | x <= - unchanged_limit)) & length(x) == 2) {
      comm <- paste("both", words$same)
    } else {
      comm <- paste(smart_paste(purrr::map(x,
        single_number,
        description = description,
        unchanged_limit = unchanged_limit,
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
