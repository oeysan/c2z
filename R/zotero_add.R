#' @title Add items to Zotero list
#' @description Use identifiers or predefined data to add to the Zotero list
#' @param zotero A list with information on the specified Zotero library (e.g.,
#'   id, API key, collections, and items)
#' @param metadata Predefined metadata in Zoter-format, Default: NULL
#' @param doi Use \code{\link{ZoteroDoi}} to fetch DOI metadata, Default: NULL
#' @param isbn Use \code{\link{ZoteroIsbn}} to fetch ISBN metadata, Default:
#'   NULL
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return Will add data to the Zotero list
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Add items from `ZoteroIsbn` to the default Zotero list
#'   example <- ZoteroAdd(
#'     Zotero(
#'       id = "9913421",
#'       api = "RqlAmlH5l1KPghfCseAq1sQ1"
#'     ),
#'     isbn = "978-1529797138"
#'   )
#'   # Print index using `ZoteroIndex`
#'   if (any(nrow(example$items))) {
#'     ZoteroIndex(example$items) |>
#'       dplyr::select(name) |>
#'       print(width = 80)
#'   }
#' }
#' @rdname ZoteroAdd
#' @export
ZoteroAdd <- \(zotero,
               metadata = NULL,
               doi = NULL,
               isbn = NULL,
               silent = FALSE) {

  # Add-append any DOI
  if (!is.null(doi)) {

    zotero$log <- LogCat(
      sprintf("Searching %s using DOI",
              Numerus(length(doi), "item")),
      log = zotero$log,
      silent = silent
    )

    # Find isbn itesm
    doi <- ZoteroDoi(doi)
    # Add to log
    zotero$log <- doi$log

    # Add items
    metadata <- AddAppend(doi$data, metadata)

  }
  # Add-append any ISBN
  if (!is.null(isbn)) {

    zotero$log <- LogCat(
      sprintf("Searching %s using ISBN",
              Numerus(length(isbn), "item")),
      log = zotero$log,
      silent = silent
    )

    # Find isbn items
    isbn <- ZoteroIsbn(isbn)
    # Add to log
    zotero$log <- isbn$log

    # Add items
    metadata <- AddAppend(isbn$data, metadata)
  }

  # Add-append any items to zotero list
  if (!is.null(metadata)) {

    zotero$log <- LogCat(
      sprintf("Adding %s formated %s to Zotero list",
              nrow(metadata),
              Numerus(nrow(metadata), "item", prefix = FALSE)),
      log = zotero$log,
      silent = TRUE
    )

    # Extract all zotero collection keys
    keys <- GoFish(zotero$collections$key, NULL)
    # Add any keys from zotero collection
    if (!is.null(keys)) {
      metadata$collections <- lapply(seq_len(nrow(metadata)), \(i) {
        # Find existing collections in item
        key <- GoFish(metadata[i,]$collections[[1]], NULL)
        # Add if item is not an attachment, or a note
        if (!grepl('attachment|note', metadata[i,]$itemType)) {
          unique(c(key, keys))
        }
      })
    }

    # Add items to zotero list
    zotero$items <- AddAppend(metadata, zotero$items)

  }

  return (zotero)

}
