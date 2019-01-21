# more_or_fewer
#
#' This function outputs either more or fewer depending on the difference between the two numbers
#' @param no1 A number
#' @param no2 A number
#' @export
#' @examples
#' more_or_fewer(1,3) #returns "more"
#' more_or_fewer(-3,1) #returns "fewer"

more_or_fewer = function (no1,no2){  
  
  tryCatch({
    
    if  (is.null(no1) || is.null(no2)) {
      
      # Check that input is not null, and raise an error if it is
      
      stop("Input to more_or_fewer is NULL", call. = FALSE)
      
    } else if (is.na(no1) || is.na(no2)) {
      
      # Check that input is not null, and raise an error if it is
      
      stop("Input to more_or_fewer is NA", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (no1>no2){
        "more"
      }
      else{
        "fewer"
      }
    }
  })
}

# more_or_less
#
#' This function outputs either more or less depending on the difference between the two numbers
#' @param no1 A number
#' @param no2 A number
#' @export
#' @examples
#' more_or_less(1,3) #returns "more"
#' more_or_less(-3,1) #returns "less"

more_or_less = function (no1,no2){  
  
  tryCatch({
    
    if  (is.null(no1) || is.null(no2)) {
      
      # Check that input is not null, and raise an error if it is
      
      stop("Input to more_or_less is NULL", call. = FALSE)
      
    } else if (is.na(no1) || is.na(no2)) {
      
      # Check that input is not null, and raise an error if it is
      
      stop("Input to more_or_less is NA", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (no1>no2){
        "more"
      }
      else{
        "less"
      }
    }
  })
}

# earlier_or_later
#
#' Outputs earlier or later depending on CI that is inputted
#' @param no1 A number
#' @param no2 A number
#' @examples
#' format_num(5,10) # returns "earlier"
#' format_num(5,-10) # returns "later"
#


earlier_or_later = function (no1,no2){
  
  tryCatch({
    
    if  ((is.null(no1) || is.null(no2))) {
      
      # Check that input is not null, and raise an error if it is
      
      stop("Input to earlier_or_later is NULL", call. = FALSE)
      
    } else if (is.na(no1) || is.na(no2)) {
      
      # Check that input is not null, and raise an error if it is
      
      stop("Input to earlier_or_later is NA", call. = FALSE)
      
    } else if (is.numeric(no1)==FALSE || is.numeric(no2)==FALSE) {
      
      # Check that no1 is not bigger than no2, and raise an error if it is
      
      stop("Input to no1 or no2 is not numeric", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (no1>no2){
        "later"
      }
      
      else{
        "earlier"
      }
    }
  })
  
}

# incrs_or_decrs
#
#' Outputs a decriptive word or phrase that decribes the CI that is inputted
#' @param no1 A number
#' @param type Shows what type of stat is being inputted
#' @examples
#' format_num(5,10) # returns "increases"
#' format_num(-5,10) # returns "increases/decreases/has no effect on"
#


incrs_or_decrs = function (no1, no2, type){ 
  
  tryCatch({
    
    if  (is.null(no1) || is.null(no2)) {
      
      # Check that input is not null, and raise an error if it is
      
      stop("Input to incrs_or_decrs is NULL", call. = FALSE)
      
    } else if (is.na(no1) || is.na(no2)) {
      
      # Check that input is not null, and raise an error if it is
      
      stop("Input to incrs_or_decrs is NA", call. = FALSE)
      
    } else if ((type %in% c('days','offences','men','women','people'))==FALSE) {
      
      # Check that is a valid type
      
      stop("incrs_or_decrs type is not an accepted form of type", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if(type %in% c('men','women','people')){
        
        joining_phrase <- " the reoffending rate of its participants."
        
      } else if ( type == 'offences'){
        
        joining_phrase <- " the number of reoffences committed by its participants."
        
      } else {
        
        joining_phrase <- " the average time to first reoffence for its participants." 
        
      }
      
      
      if (no1 >= 0 && no2 >= 0){
        paste0("decreases", if (no1>0 && no2>0) "/has no effect on",
               joining_phrase)
      }
      
      else if (no1 < 0 && no2 < 0){
        
        paste0("increases", if (no1<0 && no2<0) "/has no effect on",
               joining_phrase)
        
      }
      
      else {
        paste0("increases/decreases/has no effect on",
               joining_phrase)
      }
      
    }
  })
}
