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
    dir(system.file("extras", package = "helios"))
  } else {
    system.file("extras", path, package = "helios", mustWork = TRUE)
  }

}
