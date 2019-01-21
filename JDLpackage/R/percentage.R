# percentage
#
#' Takes two numbers and returns the percentage of the second number compared to the first one
#' If the rounded number equals 0, it will return with "<1\%"
#' @param no1 A number
#' @param no2 A number
#' @examples
#' percentage(20,5) #returns "25%"
#' percentage(100,1) #returns "1%"


percentage = function(no1, no2){
  
  
  tryCatch({
    
    if  (is.null(no1) || is.null(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to percentage is NULL", call. = FALSE)
      
    } else if (is.na(no1) || is.na(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to percentage is NA", call. = FALSE)
      
    } else if (is.character(no1) || is.character(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input is not a number", call. = FALSE)
      
    } else if (no1 < 0 || no2 < 0) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input is negative", call. = FALSE)
      
    } else if (no1 < no2) {
      
      # Impossible that first number is smaller than the second one
      
      stop("First number is smaller than second one", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (round(no2/no1*100)==0) {
        "<1%"
      }
      else if(no2==0) {
        "0"
      }
      else{
        paste(round(no2/no1*100),"%",sep="")
      }
    }
  })
}
