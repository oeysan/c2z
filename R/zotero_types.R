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
ZoteroTypes <- \(type = NULL, names = TRUE) {

  # Visible bindings
  zotero.types <- zotero.types

  # Set specified zotero-item if type is specified
  if (!is.null(type)) {
    zotero.types <- zotero.types[[type]]
    # Return only names if names is TRUE
    if (names) zotero.types <- names(zotero.types)
  }

  return (zotero.types)

}
