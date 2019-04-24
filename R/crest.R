#' Transforms RDOS into tidy format, including offence groups
#' @param data crest_with_offence_groups dataset
#' @export

crest <- function(data){
  data %>%
  dplyr::group_by(period, variable) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()
}