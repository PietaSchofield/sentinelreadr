#' clinical_xml_to_table
#'
#' convert xml to a table
#'
#' This is probably heavily dependent on the version of the data file format and could need regular
#' ammending it. The dataitems and parentlevel parameters are an attempt to make this flexible but
#' possibly will not be sufficient.
#'
#' @param xml_data this is the clinical_data list element produced from the read_rsntl() function
#' @param data_items this is a vector of items that are wanted the default
#' @param data_types this is a vector of data types for items that are wanted
#' @param parent_level this is the common parent level in the xml below which the dataitems are located
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
clinical_xml_to_table <- function(xml_data,
    data_items=c("Time/Day","Time/Hour","Time/Minute","Time/Second","ReadingStatus",
               "ErrorStatus","Systolic","Diastolic","MeanArterialPressure","HeartRate",
               "PulsePressure","CoefficientOfVariability","DiaryEventAnnotation","Excluded"),
    data_types=c("i","i","i","i","c","c","i","i","i","i","i","i","c","c"),
    parent_level="//Measurements/Measurement/"){

  tab <- xml_to_table(xmlData=xml_data,dataItems=data_items,dataTypes=data_types,parentLevel=parent_level)
  colnames(tab) <- gsub("/","_",colnames(tab))
  tab <- tab %>% mutate(Time=(24*60*60*Time_Day)+(60*60*Time_Hour)+(60*Time_Minute)+Time_Second)
  return(tab)

}
