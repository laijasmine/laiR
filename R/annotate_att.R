#' Add attribute annotation to eml document
#'
#' @param d eml document
#' @param index integer a number
#' @param attribute character - attributeName
#' @param identifier character - a unique identifier
#' @param label character - the value semantic annotation label
#' @param valueURI character- value URI
#'
#' @return
#' @export
#'
#' @examples
annotate_att <- function(d, index = NULL, attribute, identifier, label, valueURI){
  att <- arcticdatautils::eml_get_simple(d$dataset$dataTable, "attributeName")
  j <- which(att == attribute)

  #identifier
  if(index == 1){ #if there is only one
    d$dataset$dataTable$attributeList$attribute[[j]]$id <- identifier

    d$dataset$dataTable$attributeList$attribute[[j]]$annotation$propertyURI <- list(label = "contains measurements of type",
                                                                                    propertyURI = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType")
    d$dataset$dataTable$attributeList$attribute[[j]]$annotation$valueURI <- list(label = label,
                                                                                 valueURI = valueURI)
  } else{
    d$dataset$dataTable[[index]]$attributeList$attribute[[j]]$id <- identifier

    d$dataset$dataTable[[index]]$attributeList$attribute[[j]]$annotation$propertyURI <- list(label = "contains measurements of type",
                                                                                    propertyURI = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType")
    d$dataset$dataTable[[index]]$attributeList$attribute[[j]]$annotation$valueURI <- list(label = label,
                                                                                 valueURI = valueURI)
  }


  return(d)
}
