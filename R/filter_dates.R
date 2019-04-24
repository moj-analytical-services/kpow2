#' Filters out data after publication date in bulletin
#' @param data CREST RDOS dataset
#' @param publication_year Year of publication (integer)
#' @param publication_quarter Quarter of publication (integer)
#' @export

filter_dates <- function(data, publication_year, publication_quarter){
  data <- data %>%
    dplyr::filter(year < publication_year | year == publication_year & quarter <= publication_quarter)
  return(data)
}
