#' Process a directory of files
#'
#' Produce two files one with patient detail one with all test results from a directory of files
#'
#' @param directory full path to the directory containing RSNTL files
#' @param extention the file extension of the sentinel files
#' @param pattabefile the full pathname of the csv file to take the patient data (1 record per patient)
#' @param restablefile the full pathname of the csv file to take the clinical results values from the
#' monitor 
#' @param silent (currently unused)
#' @param db debug flag for development purpose return the data in a list if set to true
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @export
process_files <- function(directory,extention="RSNTL",pattablefile=NULL,restablefile=NULL,silent=F,
                          db=F){
  if(db){
    directory <- directory
    extention <- "RSNTL"
    pattablefile <- "patient.csv"
    restablefile <- "readings.csv"
    silent <- F
    db=F
  }
  files <- list.files(directory,pattern=paste0(".*",extention,"$"),full=T) 
  names(files) <- gsub(paste0("[.]",extention),"",basename(files))
  patdata <- lapply(files,process_file_patient_details) %>%
    plyr::ldply() %>% tibble::tibble() %>% dplyr::rename(filename=.id)
  if(!is.null(pattablefile))  write_csv(patdata,file=pattablefile)
  resdata <- lapply(files,process_file_clinical_details) %>% 
    plyr::ldply() %>% tibble::tibble() %>% dplyr::rename(filename=.id)
  if(!is.null(restablefile))  write_csv(resdata,file=restablefile)
  ret <- NULL
  if(db){
    list(patienttable=patdata,resultsdata=resdata)
  }
}

#' process the patient details 
#'
#' process the patient details from an RSNTL file and return a single record
#' 
#' @param fn the filename
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @export
process_file_patient_details <- function(fn,db=F){
  if(db){
    n <- 5
    fn <- files[n]
  }
  xml <- NULL
  xml <- read_rsntl(fn,chkDMV=T,stopOnWrongVersion=F)
  if(!is.null(xml)){
    ret <- lapply(xml$test_details,testdetails_xml_to_table,trans=T) %>% plyr::ldply()
  }else{
    logr::log_print(paste("Error returned for file ",fn))
    ret <- NULL
  }
  return(ret)
}

#' process the clinical details
#'
#' process the clinical details from an RSNTL file and return a record set 1 record per reading
#' 
#' @param fn the filename
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @export
process_file_clinical_details <- function(fn,db=F){
  if(db){
    n <- 5
    fn <- files[n]
  }
  xml <- NULL
  xml <- read_rsntl(fn,chkDMV=T,stopOnWrongVersion=T)
  if(!is.null(xml)){
    pat <- lapply(xml$test_details,testdetails_xml_to_table) %>% plyr::ldply() %>%
      dplyr::filter(itemname=="Patient PatientNumber") %>% dplyr::pull(values) %>% unique()
    ret <- lapply(xml$clinical_data,clinical_xml_to_table) %>% plyr::ldply() %>%
      dplyr::mutate(PatientNumber=pat)
  }else{
    logr::log_print(paste("Error returned for file ",fn))
    ret <- NULL
  }
  return(ret)
}

