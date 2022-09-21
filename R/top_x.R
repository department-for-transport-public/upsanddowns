#' From a data frame, returns the top n values from a named column
#' based on the values in another column, and pastes them together in a 
#' grammatically correct string. Ideal for commentary which aims to describe
#' e.g. "the top 3 values of X were for the following regions:"
#'
#' @name top_x
#'
#' @param df a dataframe of value-name pairs.
#' @param n the number of values/names of interest; e.g. n = 3 will return
#' a commentary discussing the top 3 values
#' @param values column name of the column containing numerical values to 
#' sort top n values on. Defaults to "value".
#' @param names column name of the column containing name values used to 
#' describe the top n values. Defaults to "name"
#'
#' @export
#' 
#' @import dplyr
#' @importFrom utils head
#'
#' @return Returns a string of name values for the top n values.

top_x <- function(df, 
                  n = 1, 
                  values = "value", 
                  names = "name"){
  
  #Stop if the column not found
  if(!(values %in% names(df))){
    stop(paste("Column", values, "not found in data provided"))
  }
  
  if(!(names %in% names(df))){
    stop(paste("Column", names, "not found in data provided"))
  }
  
  values <- dplyr::ensym(values)
  names <- dplyr::ensym(names)
  
  ##Retrieve n values in the specified column
  return <- df %>%
    dplyr::arrange(desc({{values}})) %>%
    head(n) 

  return_names <- return %>%
    dplyr::pull(names)
  
  return_vals <- return %>%
    dplyr::pull(values)
  
  ##Return a warning if there are other rows with the same values
  other_vals <- df %>%
    dplyr::filter(!{{names}} %in% return_names) %>%
    dplyr::filter({{values}} %in% return_vals)

  if(nrow(other_vals) != 0){
    warning(paste(nrow(other_vals), "additional rows have values",
                  "matching the top", n, "values"))
  }
  
  ##Paste it together in a sentence
  smart_paste(return_names)
  
}
