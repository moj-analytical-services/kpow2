# icon
#
#' Takes a p-value the difference, and outputs an icon that indicates whether it is an increase or
#'  decrease and whether it is significant or not in the number and frequency of reoffences 
#' @param pvalue A number
#' @param diff A number
#' @param stat Shows what variable the icon is representing
#' @export
#' @examples
#' icon()
#' 
#' 
icon <- function (pvalue, diff, stat){
  
  tryCatch({
    
    if  ((is.null(pvalue) || is.null(diff))) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to icon is NULL", call. = FALSE)
      
    } else if (is.na(pvalue) || is.na(diff)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to icon is NA", call. = FALSE)
      
    } else if ((stat %in% c('reoff_freq','reoff_rate','time'))==FALSE) {
      
      # Check that stat is a recognised stat, and raises an error if it is not
      
      stop("Input to icon stat is not an accepted form of stat", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if ( stat == "time") {
        
        if (diff < 0){
          
          if (pvalue <= 0.05){
            
            "<span style = 'color: rgb(240, 25, 34) !important; font-size: 16pt;'>&#129147;</span>" #down red arrow
          }
          else {
            
            "<span style = 'color: rgb(150, 150, 150) !important; font-size: 16pt;'>&#129147;</span>" #down grey arrow
            
          }
        }
        else{
          if (pvalue <= 0.05){
            
            "<span style = 'color: rgb(31, 175, 79) !important; font-size: 16pt;'>&#129145;</span>" #up green arrow
            
          }
          else {
            
            "<span style = 'color: rgb(150, 150, 150) !important; font-size: 16pt;'>&#129145;</span>" #up grey arrow
            
          }
        }
        
      }
      
      else {
        
        if (diff < 0){
          
          if (pvalue <= 0.05){
            "<span style = 'color: rgb(31, 175, 79) !important; font-size: 16pt;'>&#129147;</span>" #down green arrow
            
          }
          
          else {
            "<span style = 'color: rgb(150, 150, 150) !important; font-size: 16pt;'>&#129147;</span>" #down grey arrow
            
          }
        }
        
        else{
          if (pvalue <= 0.05){
            "<span style = 'color: rgb(240, 25, 34) !important; font-size: 16pt;'>&#129145;</span>" #up red arrow
            
          }
          
          else {
            "<span style = 'color: rgb(150, 150, 150) !important; font-size: 16pt;'>&#129145;</span>" #up grey arrow
            
          }
        }
        
      }
      
    }
  })
}
