#' Filters out data after publication date in bulletin
#' @param data CREST RDOS dataset
#' @param publication_year Year of publication (integer)
#' @param publication_quarter Quarter of publication (integer)
#' @export

fig3_data <- function (data, publication_year, publication_quarter) {
  data %>%
    dplyr::filter(year == publication_year, quarter == publication_quarter) %>%
    dplyr::group_by(year, quarter, offence_group_desc, rdos_type) %>%
    dplyr::summarise(value = sum(N)) %>%
    dplyr::arrange(-value) %>%
    dplyr::ungroup() 
}