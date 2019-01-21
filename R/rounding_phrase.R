# rounding_phrase
#
#' Takes a p-value the difference, and outputs an rounding_phrase that indicates whether it is an increase or
#'  decrease and whether it is significant or not in the number and frequency of reoffences 
#' @param values the numbers imputted into function to test whether they round to 100
#' @param wanted_num the numbers imputted into function to test whether they round to 100
#' @param type either "Difference" or "Sum"
#' @export
#' @examples
#' rounding_phrase()
#' 
#' 
rounding_phrase <- function (values, wanted_num, type){
  
  tryCatch({
    
    if  (is.null(wanted_num)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to rounding_phrase is NULL", call. = FALSE)
      
    } else if (is.na(wanted_num)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to rounding_phrase is NA", call. = FALSE)
      
    } else {
      
      #Turns inputs into numeric type
      
      values = sapply(values, as.numeric)
      
      wanted_num = as.numeric(wanted_num)
      
      
      if(type == "Difference"){ # if you want to take the difference of the vector values
        
        rounded_vals = abs(diff(values))
        
        if (rounded_vals != wanted_num){
          
          box = TRUE
        } else {
          box = FALSE
        }
        
      } else {
        
        summed_vals = sum(values)
        
        if (summed_vals != wanted_num){
          box = TRUE
        } else {
          box = FALSE
        }
        
        
      }
      
      return(box)
    }
    
  })
}
