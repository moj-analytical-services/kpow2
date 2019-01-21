# was_were
#
#' Produces sigular or plural version of was
#' @param number A number
#' @param capital A lagical word showing whether the start of the word should be capatalised
#' @examples
#' format_num("2") # returns "were"
#' format_num("1", capital = TRUE) # returns "Was"
#' @export 
#' 

was_were <- function(number,
                     capital = FALSE) {
  
  tryCatch({
    
    # Check that only one value is passed to was_were() at a time and raise
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
      
      stop("Input to was_were is NULL", call. = FALSE)
      
    } else if (is.na(number)) {
      
      # Check that number is not null, and raise and error if it is
      
      stop("Input to was_were is NA", call. = FALSE)
      
    } else if  (number <= 0) {
      
      # Check that number is not null, and raise an error if it is
      
      stop("Input to was_were shouldn't be negative", call. = FALSE)
      
    } else {
      
      # If checks of function pass, then run the main body of the function, and
      # return and numberput.
      
      if (number == 1){
        
        word <- "was"
        
      } else {
        
        word <- "were"
        
      }
      
      
      if(capital == TRUE){
        
        word = toupper(word)
        
      }
      
      return(word)
      
    }
    
    
  })
  
}


