#' Get existing formatIDs from Dataone
#'
#' @return
#' @export
#'
#' @examples
getFormatList <- function(){
  cn <- CNode()
  formats <- dataone::listFormats(cn)
  unique(formats[formats$Type == "DATA",c(1,5)])

}

#' Create new dataObjects based on directory
#'
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
createDataObject <- function(file_name){
  #get all the names straight
  ext <- stringr::str_split(file_name, "\\.")
  fn <- paste0(folder_path, "/", file_name)

  path <- stringr::str_replace(folder_path, pattern, "")
  fp <- paste0(path, "/", file_name)

  #guess the format
  formats <- getFormatList()
  n <- which(formats$Extension == ext[[1]][2])

  if(length(n) > 1){
    warning(paste0("Format for ", file_name, " is ambiguous. Using application/octet-stream instead"))
    obj <- new("DataObject", id=paste0("urn:uuid:", UUIDgenerate()), format="application/octet-stream",
               filename= fn, targetPath = fp)
  } else if(length(formats$ID[n]) > 0){
    obj <- new("DataObject", id=paste0("urn:uuid:", UUIDgenerate()), format=formats$ID[n],
               filename= fn, targetPath = fp)
  }else{
    warning(paste0("Format ", file_name, " not found using application/octet-stream"))
    obj <- new("DataObject", id=paste0("urn:uuid:", UUIDgenerate()), format="application/octet-stream",
               filename= fn, targetPath = fp)
  }

  pkg <- addMember(pkg, obj, metadataId) #add data

}

#add an existing data object
#' Recursively adding an entire folder to a data package
#'
#' @param folder_path (character) where the files are
#' @param pattern (character) the extension to remove
#' @param pkg data package
#' @param metadataId the metadata idenfitier
#'
#' @return
#' @export
#'
#' @examples
addHierarchicalDataObject <- function(folder_path, pattern = folder_path, pkg, metadataId) {
  #take in to account if the file path contains a / at the end
  if(stringr::str_detect(folder_path, "/$")){
    folder_path <- stringr::str_remove(folder_path, "/$")
  }

  files <- dir(folder_path, recursive = T)

  lapply(files, createDataObject)

  return(pkg)
}
