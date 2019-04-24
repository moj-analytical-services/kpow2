#' Tidy CREST data ready for stats creation.
#' @param x untidy CREST dataframe
#' @return Tidy CREST dataframe
#' @examples
#' tidyCrest(df)
#' @export

tidyCrest <- function(x){
  tidyr::gather(x, variable, value, all_receipts:all_outstanding)
  }

