#' patient test details to table
#'
#' convert
#'
#' @param xmlData the xml data to parse
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom xml2 xml_text xml_name xml_child xml_children
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#'
#' @export
testdetails_xml_to_table <- function(xmlData,trans=F){
  if(F){
    xmlData <- xml$test_details[[1]]
    trans <- T
  }
  patient <- tibble::as_tibble(
    list(
      itemname = paste("Patient",xml2::xml_name(xml2::xml_children(xml2::xml_child(x=xmlData,"Patient")))),
      values = xml2::xml_text(xml2::xml_children(xml2::xml_child(x=xmlData,"Patient")))
    )
  )
  staff_node <- xml2::xml_child(xmlData,"StaffMembers")
  staff <- tibble::as_tibble(
    list(
      itemname = paste("StaffMember", xml2::xml_name(xml2::xml_children(xml2::xml_children(staff_node)))),
      values = xml2::xml_text(xml2::xml_children(xml2::xml_children(staff_node)))
    )
  )
  facilities_node <- xml2::xml_child(xmlData,"Facilities")
  facilities <- tibble::as_tibble(
    list(
      itemname = paste("Practice", xml2::xml_name(xml2::xml_children(xml2::xml_children(facilities_node)))),
      values = xml2::xml_text(xml2::xml_children(xml2::xml_children(facilities_node)))
    )
  )
  recorder <- tibble::as_tibble(
    list(
      itemname = paste("Recorder",xml2::xml_name(xml2::xml_children(xml2::xml_child(x=xmlData,"RecorderDetails")))),
      values = xml2::xml_text(xml2::xml_children(xml2::xml_child(x=xmlData,"RecorderDetails")))
    )
  )
  other <- tibble::as_tibble(
    list(
      itemname = xml2::xml_name(xml2::xml_children(x=xmlData)),
      values = xml2::xml_text(xml2::xml_children(x=xmlData))
    )
  )
  other <- other %>% dplyr::filter(!itemname%in%c("Patient","StaffMembers","Facilities","RecorderDetails"))
  ret  <- dplyr::bind_rows(patient,staff,facilities,recorder,other) 
  if(trans){
    col_names <- ret %>% dplyr::pull(itemname)
    ret <- ret %>% dplyr::select(values) %>% t()
    colnames(ret) <- col_names
    rownames(ret) <- NULL
  }
  return(ret)
}
