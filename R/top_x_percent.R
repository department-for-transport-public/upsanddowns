#' From a data frame, returns the top n values from a named column
#' based on the values in another column, and pastes them together in a 
#' grammatically correct string. Ideal for commentary which aims to describe
#' e.g. "the top 3 values of X were for the following regions:"
#'
#' @name top_x_percent
#'
#' @param df a dataframe of value-name pairs.
#' @param n the number of values/names of interest; e.g. n = 3 will return
#' a commentary discussing the top 3 values
#' @param values column name of the column containing numerical values to 
#' sort top n values on. Defaults to "value".
#' @param names column name of the column containing name values used to 
#' describe the top n values. Defaults to "name"
#' @param sum_percentage logical. Do you want a single percentage of the total 
#' to be returned for all n number of values? Defaults to TRUE. If false, 
#' returns an individual percentage for each value.
#'
#' @export
#' 
#' @import dplyr
#' @importFrom scales percent
#' @importFrom utils head
#'
#' @return Returns a string of name values for the top n values.

top_x_percent <- function(df, 
                  n = 1, 
                  values = "value", 
                  names = "name",
                  sum_percentage = TRUE){
  
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
  
  ##Calculate return values as a percentage of total vals
  #Decide if we're doing this for the total or for each
  if(sum_percentage){
    of_total <- paste(
      scales::percent(sum(return_vals, na.rm = TRUE) / 
                      sum(df[[values]], na.rm = TRUE), accuracy = 1),
      "of total")
  } else if(sum_percentage == FALSE){
    
    of_total <- paste(
      smart_paste(
      scales::percent(return_vals / 
                        sum(df[[values]], na.rm = TRUE), accuracy = 1)),
      "of total respectively")
  }
  
  ##Return a warning if there are other rows with the same values
  other_vals <- df %>%
    dplyr::filter(!{{names}} %in% return_names) %>%
    dplyr::filter({{values}} %in% return_vals)
  
  if(nrow(other_vals) != 0){
    warning(paste(nrow(other_vals), "additional rows have values",
                  "matching the top", n, "values"))
  }
  
  ##Paste it together in a sentence
  paste0(smart_paste(return_names), " (", of_total, ")")
  
}
