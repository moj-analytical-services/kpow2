#' Prepares Timeliness data for Section 4 of bulletin
#' @param data Simplified Crest Flatfile
#' @param publication_year Year of publication, integer
#' @param publication_quarter Quarter of Publication, integer
#' @import data.table
#' @export

timeliness_data <- function(data, publication_year, publication_quarter){
  
  data <- data %>%
    dplyr::filter(!is.na(disposal_date) & !is.na(first_hearing_date)) %>%
    dplyr::mutate(#mags_to_crown_days = receipt_date_crown - receipt_date_mags,    ###pseudo code for when mags data is joined
      receipt_to_hearing_days = first_hearing_date - receipt_date,
      hearing_to_completion_days = disposal_date - first_hearing_date,
      year = lubridate::year(disposal_date),
      quarter = lubridate::quarter(disposal_date)
    ) %>%
    dplyr::select(year, quarter, receipt_to_hearing_days, hearing_to_completion_days) %>%
    reshape2::melt(id.vars = c("year", "quarter")) %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(year, quarter, variable) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup() %>%
    #dplyr::filter(year > start_year| year == start_year & quarter >= start_quarter) %>%
    courtstats:::filter_dates(publication_year, publication_quarter)

  data <- data.table::as.data.table(data) 
  data <- data[, period := paste0(year, " Q", quarter)] # could do this with dplyr, but took too long to run
  return(data)
  
}