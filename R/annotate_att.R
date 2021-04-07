#' Constructs an EML annotation for attributes
#'
#' @param label character
#' @param valueURI URI
#'
#' @return EML annotation
#' @export
#'
#' @examples
annotate <- function(label, valueURI){
  annotated <- list(propertyURI = list(label = "contains measurements of type",
                                       propertyURI = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType"),
                    valueURI = list(label = label,
                                    valueURI = valueURI))

  return(annotated)
}

#' Add attribute annotation to eml document
#'
#' @param d eml document
#' @param index integer a number
#' @param attribute character - attributeName
#' @param identifier character - a unique identifier (other wise defaults to attribute name)
#' @param label character - the value semantic annotation label
#' @param valueURI character- value URI
#'
#' @return
#' @export
#'
#' @examples
annotate_att <- function (d, index = NULL, attribute, identifier = NULL, label, valueURI, type = "dataTable"){
  stopifnot((type %in% c("dataTable", "spatialVector", "spatialRaster")))

  if (is.null(index)) {
    if(type == "dataTable"){
      att <- arcticdatautils::eml_get_simple(d$dataset$dataTable,
                                             "attributeName")
      j <- which(att == attribute)
      d$dataset$dataTable$attributeList$attribute[[j]]$id <- identifier
      d$dataset$dataTable$attributeList$attribute[[j]]$annotation <- annotate(label, valueURI)
    } else if (type == "spatialVector"){
      att <- arcticdatautils::eml_get_simple(d$dataset$spatialVector,
                                             "attributeName")
      j <- which(att == attribute)

      if(length(att) == 1){
        d$dataset$spatialVector$attributeList$attribute$id <- identifier
        d$dataset$spatialVector$attributeList$attribute$annotation <- annotate(label, valueURI)
      } else {
        d$dataset$spatialVector$attributeList$attribute[[j]]$id <- identifier
        d$dataset$spatialVector$attributeList$attribute[[j]]$annotation <- annotate(label, valueURI)
      }

    } else if (type == "spatialRaster"){
      att <- arcticdatautils::eml_get_simple(d$dataset$spatialRaster,
                                             "attributeName")
      j <- which(att == attribute)

      if(length(att) == 1){
        d$dataset$spatialRaster$attributeList$attribute$id <- identifier
        d$dataset$spatialRaster$attributeList$attribute$annotation <- annotate(label, valueURI)
      }else{
        d$dataset$spatialRaster$attributeList$attribute[[j]]$id <- identifier
        d$dataset$spatialRaster$attributeList$attribute[[j]]$annotation <- annotate(label, valueURI)
      }

    }

  } else {
    if(type == "dataTable"){
      att <- arcticdatautils::eml_get_simple(d$dataset$dataTable[[index]],
                                             "attributeName")
      j <- which(att == attribute)

      d$dataset$dataTable[[index]]$attributeList$attribute[[j]]$id <- identifier
      d$dataset$dataTable[[index]]$attributeList$attribute[[j]]$annotation <- annotate(label, valueURI)
    }  else if (type == "spatialVector"){
      att <- arcticdatautils::eml_get_simple(d$dataset$spatialVector[[index]],
                                             "attributeName")
      j <- which(att == attribute)

      if(length(att) == 1){
        d$dataset$spatialVector[[index]]$attributeList$attribute$id <- identifier
        d$dataset$spatialVector[[index]]$attributeList$attribute$annotation <- annotate(label, valueURI)
      } else {
        d$dataset$spatialVector[[index]]$attributeList$attribute[[j]]$id <- identifier
        d$dataset$spatialVector[[index]]$attributeList$attribute[[j]]$annotation <- annotate(label, valueURI)
      }

    } else if (type == "spatialRaster"){
      att <- arcticdatautils::eml_get_simple(d$dataset$spatialRaster[[index]],
                                             "attributeName")
      j <- which(att == attribute)

      if(length(att) == 1){
        d$dataset$spatialRaster[[index]]$attributeList$attribute$id <- identifier
        d$dataset$spatialRaster[[index]]$attributeList$attribute$annotation <- annotate(label, valueURI)
      }else{
        d$dataset$spatialRaster[[index]]$attributeList$attribute[[j]]$id <- identifier
        d$dataset$spatialRaster[[index]]$attributeList$attribute[[j]]$annotation <- annotate(label, valueURI)
      }
    }}

  return(d)
}

