#' Returns value of chosen variable at chosen time
#' @param df CREST dataset
#' @param x A variable (disposals, receipts or outstanding).
#' @param y A number to indicate time (0 = latest, 1 = one quarter ago, etc.)
#' @export

crown <- function(df, x, y=0){
  df %>%
    filter(variable == x) %>%
    filter(row_number() == n()-y) %>%
    .$value
  }