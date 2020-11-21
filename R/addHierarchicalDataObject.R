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
addHierarchicalDataObject <- function(folder_path, pattern, pkg, metadataId) {
  #take in to account if the file path contains a / at the end
  if(stringr::str_detect(folder_path, "/$")){
    folder_path <- stringr::str_remove(string, "/$")
  }

  files <- dir(folder_path, recursive = T)
  progressBar <- utils::txtProgressBar(0, length(files),
                                       style = 3)

  # Files in data
  for(i in 1:length(files)){
    #get all the names straight
    file_name <- files[[i]]
    ext <- stringr::str_split(file_name, "\\.")
    fn <- paste0(folder_path, "/", file_name)

    path <- stringr::str_replace(folder_path, pattern, "")
    fp <- paste0(path, "/", file_name)

    #guess the format
    formats <- getFormatList()
    n <- which(formats$Extension == ext[[1]][2])

    if(length(formats$ID[n]) > 0){
      obj <- new("DataObject", id=paste0("urn:uuid", UUIDgenerate()), format=formats$ID[n],
                 filename= fn, targetPath = fp)
    }else{
      print(paste0("Format", file_name, " not found using application/octet-stream"))
      obj <- new("DataObject", id=paste0("urn:uuid", UUIDgenerate()), format="application/octet-stream",
                 filename= fn, targetPath = fp)
    }

    pkg <- addMember(pkg, obj, metadataId) #add data
    utils::setTxtProgressBar(progressBar, i)
  }
  return(pkg)
}
