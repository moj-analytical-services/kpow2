# date_ending
#
#' This function produces the correct ordinal indicator for the date
#' @param date A number
#' @export
#' @examples
#' date_ending(1) # returns "st"
#' day(3) # returns "rd"
#' 

date_ending = function(date){
  
  tryCatch({
    
    if  (is.null(date)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to date_ending is NULL", call. = FALSE)
      
    } else if (is.na(date)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to date_ending is NA", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (date %% 10 == 1 && date != 11){
        output <- 'st'
      }
      
      else if (date %% 10 == 2  && date != 12){
        output <- 'nd'
      }
      
      else if (date %% 10 == 3 && date != 13){
        output <- 'rd'
      }
      
      else {
        output <- 'th'
      }
      
      output <- stringr::str_c("<sup>", output, "</sup>")
      
      return(output)
    }
  })
}
