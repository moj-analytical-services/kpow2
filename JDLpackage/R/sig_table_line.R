# sig_table_line                        
#
#' This function outputs either more or less depending on the difference between the two numbers
#' @param p_value The p-vlaue realated to the variables being showned on that table line
#' @param bracket Type of HTML table bracket to be applied to the line
#' @param class CSS class to apply to line
#' @param ... The test to be put in each cell in the line
#' @export  
#' @examples
#' more_or_less(0.01) #returns "**Significant difference between groups**"
#' more_or_less(0.5) #returns "**Non-significant difference between groups**"
#' 


sig_table_line = function(p_value, 
                          bracket, 
                          class,
                          ...){
  if(p_value > 0.05){
    
    vec_string <- stringr::str_c(...,
                                 sep = str_c("</",bracket,">","<",bracket," class = '",class,"'>"))
    
    row_string <- stringr::str_c("<",bracket," class = '",class,"'>",
                                 vec_string,
                                 "</",bracket,">")
    
    return(row_string)
    
  } else { 
    
    vec_string <- stringr::str_c(...,
                                 sep = str_c("</",bracket,">",
                                             "<",bracket," style = 'background-color: rgb(197, 217, 241) !important;' class = '", class, "'>"))
    
    row_string <- stringr::str_c("<",bracket," style = 'background-color: rgb(197, 217, 241) !important;' class = '", class, "'>",
                                 vec_string,
                                 "</",bracket,">")
    
    return(row_string)
    
    
  }
  
}
