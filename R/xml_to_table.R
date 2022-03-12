#' xml_to_table
#'
#' convert xml to a table
#'
#' This is probably heavily dependent on the version of the data file format and could need regular
#' ammending it. The dataitems and parentlevel parameters are an attempt to make this flexible but
#' possibly will not be sufficient.
#'
#' @param xmlData this is the clinical_data list element produced from the read_rsntl() function
#' @param dataitems this is a vector of items that are wanted the default
#' @param parentlevel this is the common parent level in the xml below which the dataitems are located
#'
#' @return A data.table with the columns representing the data items
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @examples
#' xml_data <- read_rsntl(system.file("extdata","2013_B_07-01-2021_13-49-43.RSNTL",package="sentinelreadr"))
#' tables <- lapply(xml_data$clinical_data,clinical_xml_to_table)
#'
#' @export
#'
xml_to_table <- function(xmlData, dataItems,dataTypes,parentLevel){
  res <- list()
  for(iwk in seq(1,length(dataItems))){
    path=paste0(".",parentLevel,dataItems[iwk])
    vec <- unlist(xml2::as_list(xml2::xml_find_all(xmlData,xpath=path)))
    if(dataTypes[iwk]!="c"){
      vec <- as.numeric(vec)
    }
    res[[dataItems[[iwk]]]] <- vec
  }
  tab <- tibble::as_tibble(res)
  return(tab)
}
