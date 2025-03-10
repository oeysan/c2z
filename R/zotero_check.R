#' @title Check Zotero library for duplicates
#' @description Remove references that are not modified since last added to
#'   Zotero
#' @param data Tibble containing metadata (from Cristin)
#' @param id column containing identifier (e.g., cristin_result_id)
#' @param id.type Type of metadata as found in the Zotero extra field ("e.g.,
#'   Cristin)
#' @param created column containing creation date in UNIX timestamp format
#'   (e.g., created)
#' @param last.modified column containing modification date in UNIX timestamp
#'   format (e.g., last_modified)
#' @param items Items in library to check for
#' @param remove.duplicates Remove duplicates if TRUE, Default: TRUE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @param log A list for storing log elements, Default: list()
#' @return Returns non-duplicated data in a Zotero-type matrix (tibble)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Simple `Cristin` search by id
#'   cristin.data <- Cristin(
#'     id = "840998",
#'     zotero.import = FALSE
#'   )
#'
#'   # Simple `ZoteroCheck`
#'   example <- ZoteroCheck(
#'     data = cristin.data$result,
#'     id = "cristin_result_id",
#'     id.type = "Cristin",
#'     created = "created",
#'     last.modified = "last_modified",
#'     items = Zotero(
#'       user = FALSE,
#'       id = "4827927",
#'       api = "RqlAmlH5l1KPghfCseAq1sQ1",
#'       library = TRUE,
#'       silent = TRUE
#'     )$items
#'   )
#' }
#' @seealso
#'  \code{\link[dplyr]{arrange}},
#'  \code{\link[dplyr]{coalesce}},
#'  \code{\link[dplyr]{bind_rows}},
#'  \code{\link[dplyr]{filter}}
#' @rdname ZoteroCheck
#' @export
ZoteroCheck <- \(data,
                 id,
                 id.type,
                 created,
                 last.modified,
                 items,
                 remove.duplicates = TRUE,
                 silent = FALSE,
                 log = list()) {

  # Visible bindings
  extra <- NULL

  # Checking references message
  log <-  LogCat(
    "Checking whether references exist in library",
    silent = silent,
    log = log
  )

  # Find result ids
  data.ids <- data[, id][[1]]

  # Fetch ids from zotero extras
  zotero.ids <- ZoteroId(id.type, items$extra)

  # Find unique items
  unique.data <- data |>
    dplyr::filter(!data.ids %in% zotero.ids)

  # Check for modified data if data exists in Zotero library
  if (nrow(unique.data) < nrow(data)) {

    # Find duplicate items in new data
    data.duplicates <- data |>
      dplyr::filter(data.ids %in% zotero.ids)

    # Find duplicate items in zotero library
    zotero.duplicates <- items |>
      dplyr::filter(zotero.ids %in% data.ids) |>
      dplyr::arrange(match(ZoteroId(id.type, extra), data.ids)) |>
      dplyr::distinct(extra, .keep_all = TRUE)

    # Find modified date of items
    data.modified <- dplyr::coalesce(
      data.duplicates[, last.modified][[1]],
      data.duplicates[, created][[1]]
    )

    # Check if data is modified since added to zotero
    if (remove.duplicates) {
      modified <- data.modified > zotero.duplicates$dateModified
    } else {
      modified <- TRUE
    }

    # Add zotero key, version and collection to modified data
    desired.cols <- c("key", "version", "collections")

    # Determine which desired columns exist in zotero.duplicates
    cols.to.copy <- intersect(desired.cols, names(zotero.duplicates))

    # Copy the matching columns from zotero.duplicates to data.duplicates
    data.duplicates[modified, cols.to.copy] <-
      zotero.duplicates[modified, cols.to.copy]

    # Remove duplicates
    if (remove.duplicates) {
      unique.data <- dplyr::bind_rows(unique.data, data.duplicates[modified, ])
    } else {
      unique.data <- dplyr::bind_rows(unique.data, data.duplicates)
    }

    # Send message
    log <-  LogCat(sprintf(
      "Removed %s",
      Numerus(nrow(data) - nrow(unique.data), "duplicate")
    ),
    silent = silent,
    log = log)

    if (any(modified)) {
      # Send message
      log <-  LogCat(sprintf(
        "Modified %s",
        Numerus(sum(modified), "item")
      ),
      silent = silent,
      log = log)
    }

  }

  return (list(data = unique.data, log = log))

}
