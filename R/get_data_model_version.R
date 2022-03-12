#' get_data_model_version
#'
#' get the data model version from the metadata secton
#'
#' @param xmlData
#'
#' @export
get_data_model_version <- function(xmlData){
  unlist(lapply(xmlData,xml2::as_list))
}
