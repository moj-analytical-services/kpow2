# incr_or_decr_phrase
#
#' This function produces a sentence that outputs increase, decrease or not change depending on the relationship between the numbers
#' @param no1 A number
#' @param no2 A number
#' @param type A word
#' @param gender A word
#' @export
#' @examples
#' incr_or_decr_CI(1234, 2345, "days", "men") #returns "increase the number of proven reoffenders during a one-year period by between 1234 and 2345 people."
#' incr_or_decr_CI(-1234,2345, "offences", "people") #returns "decrease the number of proven reoffences during a one-year period by up to 1234 reoffences, or increase it by up to 2345 reoffences." 

incr_or_decr_phrase = function (no1, no2, type, gender){ 
  
  tryCatch({
    
    if  (is.null(no1) || is.null(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to incr_or_decr_phrase is NULL", call. = FALSE)
      
    } else if (is.na(no1) || is.na(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to incr_or_decr_phrase is NA", call. = FALSE)
      
    } else if ((type %in% c('days','offences','men','women','people'))==FALSE) {
      
      # Check that is a valid type
      
      stop("incr_or_decr_phrase type is not an accepted form of type", call. = FALSE)
      
    } else if ((gender %in% c('men','women','people'))==FALSE) {
      
      # Check that input is a valid gender input
      
      stop("incr_or_decr_phrase gender is not an accepted form of gender", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if(type %in% c('men','women','people')){
        
        joining_phrase <- "number of proven reoffenders during a one-year period"
        incr_word <- "increase"
        decr_word <- "decrease"
        
      } else if ( type == 'offences'){
        
        joining_phrase <- "number of proven reoffences during a one-year period"
        incr_word <- "increase"
        decr_word <- "decrease"
        
      } else {
        
        joining_phrase <- "average time to first proven reoffence"
        incr_word <- "lengthen"
        decr_word <- "shorten"
        
      }
      
    }
    
    
    if (no1>0 && no2>0){
      
      paste(incr_word,
            "s the ",
            joining_phrase,
            " by between ",
            format(round(no1),big.mark=","), 
            " and ", 
            format(round(no2),big.mark=","),
            " ",
            type,
            ".", 
            
            sep = "")
    }
    
    else if (no1<0 && no2<0){
      
      paste(decr_word,
            "s the ",
            joining_phrase,
            " by between ",
            format(round(abs(no2)),big.mark=","), 
            " and ", 
            format(round(abs(no1)),big.mark = ","),
            " ",
            type,
            ".",
            
            sep = "")
    }
    
    else {
      paste("may ",
            decr_word,
            " the ",
            joining_phrase,
            " by up to ",
            format(round(abs(no1)),big.mark=","), 
            " ",
            if (round(abs(no1))!=1) type else if (type=="days") "day" else if (type=="offences") "offence" else JDLpackage:::people_func(1,gender),
            ", or may ",
            incr_word,
            " it by up to ", 
            format(round(abs(no2)),big.mark = ","),
            " ",
            if (round(abs(no2))!=1) type else if (type=="days") "day" else if (type=="offences") "offence" else JDLpackage:::people_func(1,gender),
            ".",
            
            sep = "")
    }
    
  })
}

