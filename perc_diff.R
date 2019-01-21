# perc_diff
#
#' Takes two numbers and states the difference of change from the first to the second 
#' @param no1 A number
#' @param no2 A number
#' @param treat_no A number
#' @param comp_no A number
#' @export
#' @examples
#' percentage()

perc_diff = function (no1,no2,treat_no,comp_no){  
  
  tryCatch({
    
    if  (is.null(no1) || is.null(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to perc_diff is NULL", call. = FALSE)
      
    } else if (is.na(no1) || is.na(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to perc_diff is NA", call. = FALSE)
      
    } else if (no1 > no2) {
      
      # Check that no1 is not bigger than no2, and raises an error if it is
      
      stop("Input to no1 is larger than no2", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      difference=abs(treat_no - comp_no) #works out the difference between the two number
      
      if ((no1>0 && no2>0) || (no1<0 && no2<0)){
        paste(" by ",format(round(difference/treat_no,digits=1),big.mark=","),"%.",sep="")
      }
      
      else {
        "."
      }
    }
  })
}