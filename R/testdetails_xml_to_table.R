#' patient test details to table
#'
#' convert
#'
#' @param xmlData the xml data to parse
#'
#' @importFrom dplyr filter bind_rows
#'
#' @export
testdetails_xml_to_table <- function(xmlData){
  patient <- tibble::as_tibble(
    list(
      itemname = paste("Patient",xml2::xml_name(xml2::xml_children(xml2::xml_child(x=xmlData,"Patient")))),
      values = xml2::xml_text(xml2::xml_children(xml2::xml_child(x=xmlData,"Patient")))
    )
  )
  facilities <- tibble::as_tibble(
    list(
      itemname = paste("Facilities",
        xml2::xml_name(xml2::xml_children(xml2::xml_children(xml2::xml_child(xmlData,"Facilities"))))),
      values = xml2::xml_name(xml2::xml_children(xml2::xml_children(xml2::xml_child(xmlData,"Facilities"))))
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
  other <- other %>% dplyr::filter(!itemname%in%c("Patient","Facilities","RecorderDetails"))
  dplyr::bind_rows(patient,facilities,recorder,other)
}
