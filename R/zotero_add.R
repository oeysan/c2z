#' @title Add items to Zotero list
#' @description Use identifiers or predefined data to add to the Zotero list
#' @param zotero A list with information on the specified Zotero library (e.g.,
#' id, API key, collections, and items)
#' @param items Predefined metadata (as tibble), Default: NULL
#' @param doi Use \code{\link{ZoteroDoi}} to fetch DOI metadata, Default: NULL
#' @param isbn Use \code{\link{ZoteroIsbn}} to fetch ISBN metadata, Default: NULL
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return Will add data to the Zotero list
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Add items from `ZoteroIsbn` to the default Zotero list
#'     example <- ZoteroAdd(Zotero(), isbn = "978-1529797138")
#'     # Use `ZoteroIndex` to print
#'     ZoteroIndex(example$items)$name
#'   }
#' }
#' @rdname ZoteroAdd
#' @export
ZoteroAdd <- \(zotero,
               items = NULL,
               doi = NULL,
               isbn = NULL,
               silent = FALSE) {

  # Add-append any DOI
  if (!is.null(doi)) {

    zotero$log <- LogCat(
      sprintf("Searching %s using DOI",
              Pluralis(length(doi), "item", "items")),
      log = zotero$log,
      silent = silent
    )

    items <- AddAppend(
      ZoteroDoi(doi),
      items
    )
  }
  # Add-append any ISBN
  if (!is.null(isbn)) {

    zotero$log <- LogCat(
      sprintf("Searching %s using ISBN",
              Pluralis(length(isbn), "item", "items")),
      log = zotero$log,
      silent = silent
    )

    items <- AddAppend(
      ZoteroIsbn(isbn),
      items
    )
  }

  # Add-append any items to zotero list
  if (!is.null(items)) {

    zotero$log <- LogCat(
      sprintf("Adding %s formated %s to Zotero list",
              nrow(items),
              Pluralis(nrow(items), "item", "items", FALSE)),
      log = zotero$log,
      silent = silent
    )

    # Extract all zotero collection keys
    keys <- GoFish(zotero$collections$key, NULL)
    # Add any keys from zotero collection
    if (!is.null(keys)) {
      items$collections <- lapply(1:nrow(items), \(i) {
        # Find existing collections in item
        key <- GoFish(items[i,]$collections[[1]], NULL)
        # Add if item is not an attachment, or a note
        if (!grepl('attachment|note', items[i,]$itemType)) {
          unique(c(key, keys))
        }
      })
    }

    # Add items to zotero list
    zotero$items <- AddAppend(items, zotero$items)

  }

  return (zotero)

}
