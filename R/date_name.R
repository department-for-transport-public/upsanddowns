#' Format a date into a consistent human-readable format
#'
#' @name date_name
#' @export
#'
#' @param x a date in date format.
#' @param abbr_day logical. Whether or not you would like the name of the day
#' of the week to be abbreviated. Defaults to TRUE.
#' @param abbr_month logical. Whether or not you would like the name of the month
#' to be abbreviated. Defaults to TRUE.
#' @param include_year logical. Whether you would like the date to include the year.
#' Defaults to FALSE.
#'
#' @importFrom lubridate day wday month year
#'
#' @return Returns a brief commentary (including the figure for
#' increases or decreases) as a string
date_name <- function(x, 
                      abbr_day = TRUE, 
                      abbr_month = TRUE, 
                      include_year = FALSE) {
  
  ##Extract day number from date
  day_number <- lubridate::day(x)
  suff <- dplyr::case_when(day_number %in% c(11:13) ~ paste0(day_number, "th"),
                           day_number %% 10 == 1 ~ paste0(day_number, 'st'),
                           day_number %% 10 == 2 ~ paste0(day_number, 'nd'),
                           day_number %% 10 == 3 ~ paste0(day_number, 'rd'),
                           TRUE ~ paste0(day_number, "th"))
  
  ##Paste together everything except year
  date_format <- 
    paste(lubridate::wday(x, 
                           label = TRUE, 
                           abbr = abbr_day),
          suff, 
          lubridate::month(x,
                           label = TRUE, 
                           abbr = abbr_month))
  
  #Add year if required
  if(include_year){
    
    date_format <- paste(date_format, lubridate::year(x))
  }
  
  return(date_format)

}
