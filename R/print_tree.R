#' Print tree with number of files
#'
#' modified from the function [dir_tree](https://fs.r-lib.org/reference/dir_tree.html) from the [fs package](https://fs.r-lib.org/index.html)
#'
#' @param path the file path
#' @param level number of folder levels recursively to go down
#'
#' @return
#' @export
#'
#' @examples
print_tree <- function(path, level = 1){
  #get the folder information
  files <- fs::dir_ls(path, recurse = level)
  by_dir <- split(files, fs::path_dir(files))
  ch <- fs:::box_chars()

  #helper function
  get_coloured_name <- function(x) {
    coloured <- fs:::colourise_fs_path(x)
    sub(x, fs:::path_file(x), coloured, fixed = TRUE)
  }

  #Modified original fs::dir_tree to print number of files in folder
  print_leaf <- function(x, indent) {
    leafs <- by_dir[[x]]
    for (i in seq_along(leafs)) {
      if (i == length(leafs)) { #first and last in each node
        cat(indent, fs:::pc(ch$l, ch$h, ch$h, " "),
            get_coloured_name(leafs[[i]]), #file/folder name
            " (", length(dir(leafs[[i]])), ")", #number of files
            "\n", sep = "")
        print_leaf(leafs[[i]], paste0(indent, "    "))
      }
      else { #everything else
        cat(indent, fs:::pc(ch$j, ch$h, ch$h, " "),
            get_coloured_name(leafs[[i]]), #file /folder  name
            " (", length(dir(leafs[[i]])), ")", #number of files
            "\n", sep = "")
        print_leaf(leafs[[i]], paste0(indent, fs:::pc(ch$v,
                                                      "   ")))
      }
    }
  }
  cat(fs:::colourise_fs_path(path), "\n", sep = "")
  print_leaf(fs::path_expand(path), "")

}

