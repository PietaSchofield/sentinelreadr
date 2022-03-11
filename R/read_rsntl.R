#' read_rsntl
#'
#' read a sentinel file extract the xml sections from the binary file size data
#'
#' @param filename
#'
#' @export
read_rsntl <- function(filename){
  fh <- file(input_filename,"rb")
  file_size <- file.info(input_filename)[,"size"]
  file_content <- readBin(fh,raw(),file_size)
  file_text <- rawToChar(file_content,multiple=T)
  file_text <- paste(file_text,collapse="")
  close(fh)

  tags <- list(
    metadata=c(stag="<PackageMetadata xmlns",etag="</PackageMetadata>"),
    test_details=c(stag="<PackageTestDetails xmlns",etag="</PackageTestDetails>"),
    clinical_data_files=c(stag="<ClinicalDataItem xmlns",etag="</ClinicalDataItem"),
    clinical_data=c(stag="<ABP xmlns",etag="</ABP>"),
    report=c(stag="<Report xmlns",etag="</Report>")
  )

  res <- lapply(tags,ext_xml_section,fulltext)
  return(res)
}

ext_xml_section <- function(tag,fulltext){
  slocs <- stringr::str_locate_all(fulltext,pattern=tag["stag"])[[1]][,"start"]
  elocs <- stringr::str_locate_all(fulltext,pattern=tag["etag"])[[1]][,"end"]
  locs <- cbind(start_tag=slocs,end_tag=elocs)
  pos <- apply(locs,1,function(tag){
    str_sub(fulltext,tag["start_tag"],tag["end_tag"])
  })
  unname(pos)
}

