#' Returns whether a variable has increased or decreased over a given period.
#' @param df CREST dataset
#' @param x A variable (disposals, receipts or outstanding).
#' @param y A number greater than 0. This is the number of quarters ago,
#' that you want to calculate change from.
#' @export

change_desc <- function(df, x, y){
  if(change(df, x, y) > 0){
    "increased"
  }
  else if(change(df, x, y) < 0){
    "decreased"
  }
  else if(change(df, x, y) > 0){
    "not changed"
  }
  }
