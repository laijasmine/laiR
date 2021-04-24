#' Create new dataObjects based on directory
#'
#' @param file_name (character)
#'
#' @return
#' @export
#'
#' @examples
createDataObject <- function(file_name, folder_path, pattern, pkg, metadataId){

    fn <- paste0(folder_path, "/", file_name)

    path <- stringr::str_replace(folder_path, pattern, "")
    fp <- paste0(path, "/", file_name)

    #guess the format
    formatId <- arcticdatautils::guess_format_id(fn)

    obj <- new("DataObject", id=paste0("urn:uuid:", UUIDgenerate()), format=formatId,
               filename= fn, targetPath = fp)

    pkg <- addMember(pkg, obj, metadataId) #add data

    return(pkg)
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

  all <- lapply(files, createDataObject, folder_path =  folder_path, pattern = pattern, pkg = pkg, metadataId = metadataId)

  pkg <- all[length(all)]

  return(pkg[[1]])
}
