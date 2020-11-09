#' Add attribute annotation to eml document
#'
#' @param d eml document
#' @param attribute character - attributeName
#' @param identifier character - a unique identifier
#' @param label character - the value semantic annotation label
#' @param valueURI character- value URI
#'
#' @return
#' @export
#'
#' @examples
annotate_att <- function(d, attribute, identifier, label, valueURI){
  att <- eml_get_simple(d$dataset$dataTable, "attributeName")
  j <- which(att == attribute)

  #identifier
  d$dataset$dataTable$attributeList$attribute[[j]]$id <- identifier

  d$dataset$dataTable$attributeList$attribute[[j]]$annotation$propertyURI <- list(label = "contains measurements of type",
                                                                                  propertyURI = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType")
  d$dataset$dataTable$attributeList$attribute[[j]]$annotation$valueURI <- list(label = label,
                                                                               valueURI = valueURI)

  return(d)
}
