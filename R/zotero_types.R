#' @title List with empty zotero-items
#' @description Each tibble in the list represents a zotero-item
#' @param type Type of zotero-item, Default: NULL
#' @param names return only column names if set to TRUE, Default: TRUE
#' @format A list with 36 tibbles with zero rows and various columns
#' @return Either list of zotero-items or specified item
#' @details Used to create Zotero-items from list of metadata
#' @examples
#' # All zotero-items
#' names(ZoteroTypes())
#' # Column names of item-type `book`
#' ZoteroTypes("book")
#' @rdname ZoteroTypes
#' @export
ZoteroTypes <- function(type = NULL, names = TRUE) {

  # Get the full file path from the package installation directory
  data.file <- system.file("extdata", "zotero_types.rda", package = "c2z")
  if (data.file == "") {
    stop("The zotero_types.rda file was not found in the package extdata folder.")
  }

  # Create a temporary environment to load the data
  temp.env <- new.env()
  load(data.file, envir = temp.env)

  # Assume the .rda file contains an object named `zotero.types`
  zotero.types <- temp.env$zotero.types

  # Set specified zotero-item if type is provided
  if (!is.null(type)) {
    if (! type %in% names(zotero.types)) {
      stop("Invalid zotero-item type specified.")
    }
    zotero.types <- zotero.types[[type]]
    # Return only names if names is TRUE
    if (names) {
      zotero.types <- names(zotero.types)
    }
  }

  return(zotero.types)
}
