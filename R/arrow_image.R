#' Chooses arrow image for first page of bulletin
#' @param x Direction of arrow (up, down, nochange)
#
#' @examples 
#' arrow_image("up")
#' 
#' @export

arrow_image <- function(x){
  
  if (tolower(x) %in% c("up", "down", "nochange")){
    
    stringr::str_c("![](images/", tolower(x), "front.png){ width=50% }")
    
  } else {
    
    print("Incorrect input, please enter 'up', 'down' or 'nochange'.")
    
  }
  
}
