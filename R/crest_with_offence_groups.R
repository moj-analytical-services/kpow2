#' Transforms RDOS into tidy format, including offence groups
#' @param data CREST RDOS dataset
#' @export

crest_with_offence_groups <- function(data){
  data %>%
  dplyr::mutate(quarter = paste("Q", quarter, sep = "")) %>%
  dplyr::mutate(period = paste(year, quarter, sep = " ")) %>%
  dplyr::select(-(year: quarter)) %>%
  dplyr::group_by(period, offence_group_desc, rdos_type) %>%
  dplyr::summarise(value = sum(N)) %>%
  dplyr::rename(variable = rdos_type)
}
