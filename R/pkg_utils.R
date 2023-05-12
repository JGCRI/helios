#' pkg_example
#'
#' list example file paths
#'
#' @param path Default = NULL. Path to example files
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

pkg_example <- function(path = NULL) {

  if (is.null(path)) {
    dir(system.file('extras', package = 'helios'))
  } else {
    system.file('extras', path, package = 'helios', mustWork = TRUE)
  }

}


#' create_name
#'
#' create file names
#'
#' @param str_vec Default = NULL. vector of strings within the name
#' @param file_ext Default = NULL. extension name of the file. If NULL, will be a folder name
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @export

create_name <- function(str_vec = NULL, file_ext = NULL) {
  new_vec <- str_vec[nzchar(str_vec)]

  if(is.null(file_ext)){
    name <- paste(new_vec, collapse = '_')
  } else {
    name <- paste0(paste(new_vec, collapse = '_'), '.', file_ext)
  }

  return(name)
}
