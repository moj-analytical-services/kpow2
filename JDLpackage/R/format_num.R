# format_num
#
#' Formats numbers with a comma delimiter and values of less than 10 are represented as words
#' @param number A number
#' @param digits A number representing number of decimal places
#' @param word A logical word showing whether the number is to be repsented as a word
#' @param capital A logical word showing whether the start of the word should be capatalised
#' @param allow_neg A logical word showing whether the number is allowed to be displayed as negative
#' @param p_value A logical word showing whether the number is a p-value
#' @examples
#' format_num("1000") # returns "1,000"
#' format_num("1", word = TRUE) # returns 'one'
#' @export

  format_num <- function(number,
                       digits = 0,
                       word = FALSE,
                       capital = FALSE,
                       allow_neg = FALSE,
                       p_value = FALSE) {
  
  tryCatch({
    
    # Check that only one value is passed to format_perc() at a time and raise
    # an error otherwise.
    
    if (length(number) > 1) {
      
      stop(
        "Input to format_num is not a single value. ",
        "Most likely you have tried to pass a vector, ",
        "list, or df to format_num()",
        call. = FALSE
      )
      
    } else if  (is.null(number)) {
      
      # Check that number is not null, and raise an error if it is
      
      stop("Input to number_perc is NULL", call. = FALSE)
      
    } else if (is.na(number)) {
      
      # Check that number is not null, and raise and error if it is
      
      stop("Input to number_perc is NA", call. = FALSE)
      
    } else {
      
      # If checks of function pass, then run the main body of the function, and
      # return and numberput.
      
      if (allow_neg == FALSE){
      
      number_format <- formatC(abs(as.numeric(number)),
                               big.mark = ",", 
                               digits = digits, 
                               format = "f")
      
      } else {
        
        number_format <- formatC(as.numeric(number),
                                 big.mark = ",", 
                                 digits = digits, 
                                 format = "f")
        
      }
      
      if(word == TRUE){
        
        number_word <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
                         "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
        
        number_format = number_word[number]
        
        if(capital == TRUE){
          number_format = paste(toupper(substring(number_format, 1, 1)), 
                                substring(number_format, 2),
                                sep="", collapse=" ")
        }
      } else if (round(number, digits) == 0 & digits == 2 & p_value == TRUE){
        
        number_format = "<0.01"
      }
      
      return(number_format)
    }
    
    
  })
  
}


