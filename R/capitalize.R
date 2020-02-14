#' Capitalizes Habitat
#'
#' Capitalizes the first word in the herbarium spreadsheet column habitat
#'
#' @param ds dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
capitalize <- function(ds){
  split <- str_split(ds$Habitat," ",n = 2)

  cap <- map(split,~str_to_sentence(.x[1]))

  ds$Habitat <- map2(split,cap,~c(.y,.x[2]) %>%
                       paste(.,collapse = " "))

  ds$Habitat <- gsub("NA","",ds$Habitat)

  return(ds)
}
