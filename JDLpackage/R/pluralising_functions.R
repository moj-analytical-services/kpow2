# plural_analysis
#
#' Formats the word 'analysis' as being plural and have a capital if needed
#' @param number A number
#' @param capital A lagical word showing whether the start of the word should be capatalised
#' @examples
#' format_num("2") # returns "analyses"
#' format_num("1", capital = TRUE) # returns "Analysis"
#' 
#' 

plural_analysis <- function(number,
                            capital = FALSE) {
  
  tryCatch({
    
    # Check that only one value is passed to plural_analysis() at a time and raise
    # an error otherwise.
    
    if (length(number) > 1) {
      
      stop(
        "Input to plural_analysis is not a single value. ",
        "Most likely you have tried to pass a vector, list, or df to plural_analysis()",
        call. = FALSE
      )
      
    } else if  (is.null(number)) {
      
      # Check that number is not null, and raise an error if it is
      
      stop("Input to plural_analysis is NULL", call. = FALSE)
      
    } else if (is.na(number)) {
      
      # Check that number is not null, and raise and error if it is
      
      stop("Input to plural_analysis is NA", call. = FALSE)
      
    } else if  (number <= 0) {
      
      # Check that number is not null, and raise an error if it is
      
      stop("Input to plural_analysis shouldn't be negative", call. = FALSE)
      
    } else {
      
      
      if (number == 1){
        
        word <- "analysis"
        
      } else {
        
        word <- "analyses"
        
      }
      
      
      if(capital == TRUE){
        
        word = toupper(word)
        
      }
      
      return(word)
      
    }
    
    
  })
  
}



# organisation_func
#
#' This function produces the correct singular or plural form of the word day according to the number that is inputted
#' @param no A number
#' @export
#' @examples
#' organisation_func(1) # returns organisation
#' organisation_func(3) # returns organisations

organisation_func = function (no){ 
  
  tryCatch({
    
    if  (is.null(no)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to organisation_func is NULL", call. = FALSE)
      
    } else if (is.na(no)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to organisation_func is NA", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      
      if (no !=1) {"organisations"}
      
      else {"organisation"}
    }
  })
}




# offence_func
#
#' This function outputs the correct singular or plural version of offence 
#' @param no A number
#' @export
#' @examples
#' offence_func(1) #returns "offence"
#' offence_func(-3) #returns "offences"

offence_func = function (no){  
  
  tryCatch({
    
    if  (is.null(no)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to offence_func is NULL", call. = FALSE)
      
    } else if (is.na(no)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to offence_func is NA", call. = FALSE)
      
    } else if (no < 0) {
      
      # Check that input is not negative
      
      stop("Input to offence_func is negative", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (no ==1) {
        "offence"
      }
      
      else {
        "offences"
      }
    }
  })
}


# people_func
#
#' This function outputs the correct singular or plural version of person, man or woman 
#' @param value A number
#' @param gender A word
#' @export
#' @examples
#' people_func(1,'people') #returns "person"
#' people_func(3,'female') #returns "women"

people_func = function (value,gender) {  
  
  tryCatch({
    
    value <- as.numeric(value)
    gender <- tolower(gender)
    
    if  (is.null(value)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to people_func is NULL", call. = FALSE)
      
    } else if (is.na(value)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to people_func is NA", call. = FALSE)
      
    } else if (value < 0) {
      
      # Check that input is not negative
      
      stop("Input to people_func is negative", call. = FALSE)
      
    } else if (!gender %in% c("people", "male", "female")) {
      
      # Check that value is in the allowed set of words
      
      stop("Gender input to people_func must be either \"people\", \"male\" or \"female\"", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (gender == "people") {
        
        if (value == 1) {
          "person"
        }
        
        else (
          "people"
        )
      }
      
      else if (gender == "male"){
        
        if (value == 1) {
          "man"
        }
        
        else (
          "men"
        )
      }
      else if (gender == "female"){
        
        if (value == 1) 
        {"woman"
        }
        
        else (
          "women"
        )
      }
    } 
  })
}

# day_func
#
#' This function produces the correct singular or plural form of the word day according to the number that is inputted
#' @param no A number
#' @export
#' @examples
#' day(1) # returns day
#' day(3) # returns days

day_func = function (no){ 
  
  tryCatch({
    
    if  (is.null(no)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to day_func is NULL", call. = FALSE)
      
    } else if (is.na(no)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to day_func is NA", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      
      if (no !=1) {"days"}
      
      else {"day"}
    }
  })
}
