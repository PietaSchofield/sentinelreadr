#' xml_to_table
#'
#' convert xml to a table
#'
#' This is probably heavily dependent on the version of the data file format and could need regular
#' ammending it. The dataitems and parentlevel parameters are an attempt to make this flexible but
#' possibly will not be sufficient.
#'
#' @param xmldata
#' @param dataitems
#' @param parentlevel
#'
#' @export
clinical_xml_to_table <- function( xmldata=XML::xmlParse(xml2$clinical_data),
                         dataitems=c("Time/Day","Time/Hour","Time/Minute","Time/Second","ReadingStatus",
                          "ErrorStatus","Systolic","Diastolic","MeanArterialPressure","HeartRate",
                          "PulsePressure","CoefficientOfVariability","DiaryEventAnnotation","Excluded"),
                         parentlevel="//Measurements/Measurement/"){
  res <- list()
  for(di in dataitems){
    path=paste0(parentlevel,di)
    res[[di]] <- XML::xpathSApply(doc=xmldata,path=path,xmlValue)
  }
  lapply(res,length)
  tab <- do.call(cbind,res)
  colnames(tab) <- gsub("/","_",colnames(tab))
  tab <- tab %>% tidyfst::mutate_dt(Time=(24.60*60*as.integer(Time_Day))+
                                   (60*60*as.integer(Time_Hour))+
                                   (60*as.integer(Time_Minute)+as.integer(Time_Second)))
  return(tab)
}
