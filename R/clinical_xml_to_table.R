#' clinical_xml_to_table
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
clinical_xml_to_table <- function(xmlData,
  data_items=c("Time/Day","Time/Hour","Time/Minute","Time/Second","ReadingStatus",
               "ErrorStatus","Systolic","Diastolic","MeanArterialPressure","HeartRate",
               "PulsePressure","CoefficientOfVariability","DiaryEventAnnotation","Excluded"),
  data_types=c("i","i","i","i","c","c","i","i","i","i","i","i","c","c"),
  parent_level="//Measurements/Measurement/"){
  res <- list()
  for(iwk in seq(1,length(data_items))){
    path=paste0(parent_level,data_items[iwk])
    vec <- XML::xpathSApply(doc=xmlData,path=path,XML::xmlValue)
    if(data_types[iwk]!="c"){
      vec <- as.numeric(vec)
    }
    res[[data_items[[iwk]]]] <- vec
  }
  tab <- tibble::as_tibble(res)
  colnames(tab) <- gsub("/","_",colnames(tab))
  tab <- tab %>% mutate(Time=(24*60*60*Time_Day)+(60*60*Time_Hour)+(60*Time_Minute)+Time_Second)
  return(tab)
  tab
}
