# sig_or_non_sig
#
#' This function outputs either more or less depending on the difference between the two numbers
#' @param p_value A number
#' @export
#' @examples
#' more_or_less(0.01) #returns "**Significant difference between groups**"
#' more_or_less(0.5) #returns "**Non-significant difference between groups**"

sig_or_non_sig = function (p_value){  
  
  tryCatch({
    
    if  (is.null(p_value)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to more_or_less is NULL", call. = FALSE)
      
    } else if (is.na(p_value)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to sig_or_non_sig is NA", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (p_value < 0.05){
        "Significant difference between groups"
      }
      else{
        "Non-significant difference between groups"
      }
    }
  })
}