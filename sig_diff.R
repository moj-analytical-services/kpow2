# sig_diff
#
#' Takes p-value and outputs yes or no depending on significance with a significance level of 0.05
#' @param pvalue A number
#' @export
#' @examples
#' sig_diff()


sig_diff = function(pvalue){  
  
  tryCatch({
    
    if  (is.null(pvalue)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to sig_diff is NULL", call. = FALSE)
      
    } else if (is.na(pvalue)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to sig_diff is NA", call. = FALSE)
      
    } else if (pvalue < 0 || pvalue > 1) {
      
      # Check that input is not negative
      
      stop("P-value is outside of the range it can technically lie in", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if(pvalue <= 0.05){
        "Yes"
      }
      
      else{
        "No"
      }
    }
  })
}
    
