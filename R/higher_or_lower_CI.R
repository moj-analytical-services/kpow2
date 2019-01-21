# higher_or_lower_CI
#
#' This function produces a sentence that decribes a confidence interval in terms of people, offences or days
#' @param no1 A number
#' @param no2 A number
#' @param type A word
#' @param gender A word
#' @export
#' @examples
#' higher_or_lower_CI(1234,2345,"days",JDLpackage:::people_func(2,"male")) #returns higher by between 1,234 and 2,345 days. <b>This is a statistically significant result.</b>
#' higher_or_lower_CI(-1234,2345,"offences",JDLpackage:::people_func(2,"female")) #returns lower by as many as 1,234 offences, or higher by as many as 2,345 offences. More women would need to be analysed in order to determine the direction of this difference.

higher_or_lower_CI = function (no1, no2, type, gender){ 
  
  tryCatch({
    
    if  (is.null(no1) || is.null(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to higher_or_lower_CI is NULL", call. = FALSE)
      
    } else if (is.na(no1) || is.na(no2)) {
      
      # Check that input is not null, and raises an error if it is
      
      stop("Input to higher_or_lower_CI is NA", call. = FALSE)
      
    } else if (no1 > no2) {
      
      # Check that no1 is not bigger than no2, and raises an error if it is
      
      stop("Input to no1 is larger than no2", call. = FALSE)
      
    } else if ((type %in% c('days','offences','men','women','people'))==FALSE) {
      
      # Check that is a valid type
      
      stop("higher_or_lower_CI type is not an accepted form of type", call. = FALSE)
      
    } else if ((gender %in% c('male','female','people'))==FALSE) {
      
      # Check that input is a valid gender input
      
      stop("higher_or_lower_CI gender is not an accepted form of gender", call. = FALSE)
      
    } else {
      
      # BODY --------------------------------------------------------------------
      
      if (no1>0 && no2>0){
        
        paste('<div class = "b_blue">higher by between ',format(round(no1),big.mark=","), " and ", format(round(no2),big.mark=",")," ", if (type %in% c('days','offences')) type else JDLpackage:::people_func(2,gender),
              ".</div> <b>This is a statistically significant result.</b>",sep="")
      }
      
      else if (no1<0 && no2<0){
        
        paste('<div class = "b_blue">lower by between ',format(round(abs(no2)),big.mark=","), " and ", format(round(abs(no1)),big.mark = ",")," ", if (type %in% c('days','offences')) type else JDLpackage:::people_func(2,gender),
              ".</div> <b>This is a statistically significant result.</b>",sep="")
      }
      
      else {
        paste('<div class = "b_blue">lower by as many as ', format(round(abs(no1)),big.mark=","), " ", if (type=='days') day_func(round(abs(no1))) else if (type=='offences') offence_func(round(abs(no1))) else JDLpackage:::people_func(round(abs(no1)),gender), ", or higher by as many as ", format(round(abs(abs(no2))),big.mark=","), 
              " ", if (type=='days') day_func(round(abs(no2))) else if (type=='offences') offence_func(round(abs(no2))) else JDLpackage:::people_func(round(abs(no2)),gender), ".</div> More ", JDLpackage:::people_func(2,gender), " would need to be available for analysis in order to determine the direction of this difference.",sep="")
      }
      
    }
  })
}
