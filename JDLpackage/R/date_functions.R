# next_pub_date
#
#' This function produces the second thursday of the next quarter
#' @param date_val A number
#' @param month_str A word in full word fomat
#' @param year_val A number
#' @export
#' @examples
#' next_pub_date(1,"July",2017) # returns "12th October 2017"
#' 


next_pub_date = function(date_val, month_str, year_val){
  
  tryCatch({
    
    if  (is.null(date_val)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to next_pub_date is NULL", call. = FALSE)
      
    } else if (is.na(date_val)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to next_pub_date is NA", call. = FALSE)
      
    } else if (date_val > 31) {
      
      # Check that input in the allowed range
      
      stop("Date input is larger than it is allowed to be", call. = FALSE)
      
    } else if ( (month_str %in% (month.name)) == FALSE)
      
      # Check that input is a valid month name
      
      stop("Your input into month is not a valid one", call. = FALSE)
    
    else {
      
      # BODY --------------------------------------------------------------------
      
      pub_date <- lubridate::dmy(paste(date_val, month_str, year_val))
      
      pub_date_3_mon <- pub_date %m+% months(3) # Adds 3 months to original 
      
      lubridate::day(pub_date_3_mon) <- 1
      
      first_day_of_month <- lubridate::wday(pub_date_3_mon, week_start = 1)
      
      next_pub_day <- pub_date_3_mon + (4 - first_day_of_month) %% 7 + 7
      
      pub_date_str <- paste0(lubridate::day(next_pub_day), 
                             
                             JDLpackage:::date_ending(day(next_pub_day)), " ",
                             
                             month.name[lubridate::month(next_pub_day)], " ",
                             
                             lubridate::year(next_pub_day))
      return(pub_date_str)
      
    } 
  })
}




#' final_period_date
#
#' This function produces the second thursday of the next quarter
#' @param date_val A number
#' @param month_str A word in full word fomat
#' @param year_val A number
#' @export
#' @examples
#' 


final_period_date = function(date_val, month_str, year_val){
  
  tryCatch({
    
    if  (is.null(date_val)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to next_final_date is NULL", call. = FALSE)
      
    } else if (is.na(date_val)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to next_final_date is NA", call. = FALSE)
      
    } else if (date_val > 31) {
      
      # Check that input in the allowed range
      
      stop("Date input is larger than it is allowed to be", call. = FALSE)
      
    } else if ( (month_str %in% (month.name)) == FALSE)
      
      # Check that input is a valid month name
      
      stop("Your input into month is not a valid one", call. = FALSE)
    
    else {
      
      # BODY --------------------------------------------------------------------
      
      final_date <- lubridate::dmy(paste(date_val, month_str, year_val))
      
      lubridate::day(final_date) <- 1
      
      final_date <- final_date - 1
      
      final_date_str <- paste0(lubridate::day(final_date),  " ",
                               
                               month.name[lubridate::month(final_date)], " ",
                               
                               lubridate::year(final_date))
      return(final_date_str)
      
    } 
  })
}



# end_jdl_period
#
#' This function produces the last day of the month before publication
#' @param date_val A number
#' @param month_str A word in full word fomat
#' @param year_val A number
#' @export
#' @examples
#' next_pub_date(1,"July",2017) # returns "30th June 2017"
#' 


end_jdl_period = function(date_val, month_str, year_val){
  
  tryCatch({
    
    if  (is.null(date_val)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to next_pub_date is NULL", call. = FALSE)
      
    } else if (is.na(date_val)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to next_pub_date is NA", call. = FALSE)
      
    } else if (date_val > 31) {
      
      # Check that input in the allowed range
      
      stop("Date input is larger than it is allowed to be", call. = FALSE)
      
    } else if ( (month_str %in% (month.name)) == FALSE)
      
      # Check that input is a valid month name
      
      stop("Your input into month is not a valid one", call. = FALSE)
    
    else {
      
      # BODY --------------------------------------------------------------------
      
      
      first_day <- dmy(paste(1, month_str, year_val))
      
      end_period <- first_day - 1
      
      
      
      end_period_str <- paste0(day(end_period), 
                               
                               JDLpackage:::date_ending(day(end_period)), " ",
                               
                               month.name[month(end_period)], " ",
                               
                               year(end_period))
      
      return(end_period_str)
      
    } 
  })
}





