#' read_rsntl
#'
#' read a sentinel file extract the xml sections from the binary file size data. The test type clinical data tag is #' an item in the PackageTestDetails <TestTypeName> so it could also be possible to check that the file holds the #' #' write test type
#'
#' @param filename file name of the RSNTL file
#' @param chkDMV check data model version
#' @param version expected Data Model Version currently defaults to "9" (character)
#' @param showWarning warn if Data Model version mismatch
#' @param stopOnWrongVersion stop if Data Model version mismatch
#'
#' @return a list of xml elements for each xml sections of the file
#'
#'  metadata = PackageMetadata
#'
#'  testdetails = PackageTestDetails
#'
#'  clincial_data_files = ClinicalDataItems
#'
#'  clinical_data = ABP
#'
#'  report = Report
#'
#' where there are multiple xml elements in a section this will be a list of of list
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @examples
#' xml_data <- read_rsntl(system.file("extdata","2013_B_07-01-2021_13-49-43.RSNTL",package="sentinelreadr"))
#'
#' @export
read_rsntl <- function(filename,chkDMV=F,version="9",stopOnWrongVersion=T,db=F){
  if(db){
    chkDMV <- F
    version <- "9"
    stopOnWrongVersion <- F
    n <- 5
    filename <- files[n]
  }
  fh <- file(filename,"rb")
  file_size <- file.info(filename)[,"size"]
  file_content <- readBin(fh,raw(),file_size)
  file_text <- rawToChar(file_content,multiple=T)
  file_text <- paste(file_text,collapse="")
  close(fh)

  tags <- list(
    metadata=c(stag="<PackageMetadata xmlns",etag="</PackageMetadata>"),
    test_details=c(stag="<PackageTestDetails xmlns",etag="</PackageTestDetails>"),
    clinical_data_files=c(stag="<ClinicalDataItem xmlns",etag="</ClinicalDataItem>"),
    clinical_data=c(stag="<ABP xmlns",etag="</ABP>"),
    report=c(stag="<Report xmlns",etag="</Report>")
  )
  
  res <- NULL
  tryCatch(
    {
      res <- lapply(tags,ext_xml_section,file_text)
      if(!is.null(res$metadata)){
        if(chkDMV){
          data_model <- get_data_model_version(res$metadata)
          if(data_model!=version){
            logr::log_print(paste("Warning: Expected data model version",version,
                        "this file contains data model version ",data_model))
            if(stopOnWrongVersion){
              stop()
            }
          }
        }
      }
    },
    error=function(e){
      logr::log_print(paste0(filename," Error: ",print(e)))
    },
    warning=function(w){
      logr::log_print(paste0(filename," Warning: ",print(w)))
    }
  )
  return(res)
}

ext_xml_section <- function(tag,fulltext,db=F){
  if(db){
    tn <- 2
    tag <- tags[[tn]]
    fulltext <- file_text
  }
  err=T
  tryCatch(
    {
      slocs <- stringr::str_locate_all(fulltext,pattern=tag["stag"])[[1]][,"start"]
      err=F
    },
    error=function(e){
      logr::log_print(paste0("Error: No ",tag["etag"]))
    },
    warning=function(w){
      logr::log_print(paste0("Warning: Problem ",tag["etag"])) 
    }
  )
  tryCatch(
    {
      elocs <- stringr::str_locate_all(fulltext,pattern=tag["etag"])[[1]][,"end"]
      err=F
    },
    error=function(e){
      logr::log_print(paste0("Error: No ",tag["etag"]))
      err=T
    },
    warning=function(w){
      logr::log_print(paste0("Warning: Problem ",tag["etag"])) 
    }
  )
  pos <- c()
  if(!err){
    locs <- cbind(start_tag=slocs,end_tag=elocs)
    tryCatch(
      {
        pos <- apply(locs,1,function(tag){
          xml2::read_xml(stringr::str_sub(fulltext,tag["start_tag"],tag["end_tag"]))
        })
      },
      error=function(e){
        logr::log_print(paste0(filename," ",print(e)))
      },
      warning=function(w){
        logr::log_print(paste0(filename," ",print(w)))
      }
    )
  }
  unname(pos)
}

