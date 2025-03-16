#' @title Check Zotero Library for Duplicates
#' @description Removes references from the input dataset that are duplicates
#' already added to Zotero,
#' preserving only those references that have been modified since their addition.
#' @param data A tibble containing metadata (e.g., from Cristin)
#' @param id A string specifying the column name in \code{data} that contains
#' the unique identifier (e.g., \code{"cristin_result_id"})
#' @param id.type A string specifying the type of metadata as stored in the
#' Zotero extra field (e.g., "Cristin")
#' @param created A string specifying the column name in \code{data} containing
#' the creation date in UNIX timestamp format (e.g., \code{"created"})
#' @param last.modified A string specifying the column name in \code{data}
#' containing the last modification date in UNIX timestamp format
#' (e.g., \code{"last_modified"})
#' @param items A data frame or tibble representing items in the Zotero library
#'  to check against
#' @param remove.duplicates Logical. If \code{TRUE}, duplicates that have not
#' been modified are removed. Default is \code{TRUE}.
#' @param silent Logical. If \code{TRUE}, suppresses verbose output. Default
#' is \code{FALSE}.
#' @param log A list for storing log messages. Default is an empty list.
#' @return A list with two elements: \code{data}, a tibble containing
#' non-duplicated items, and \code{log}, the updated log list.
#' @details For more details,
#' see \href{https://oeysan.github.io/c2z/}{c2z documentation}.
#' @examples
#' \dontrun{
#'   # Simple Cristin search by id
#'   cristin.data <- Cristin(
#'     id = "840998",
#'     zotero.import = FALSE
#'   )
#'
#'   # Checking Zotero library for duplicates
#'   result <- ZoteroCheck(
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
#'   \code{\link[dplyr]{arrange}},
#'   \code{\link[dplyr]{coalesce}},
#'   \code{\link[dplyr]{bind_rows}},
#'   \code{\link[dplyr]{filter}}
#' @export
ZoteroCheck <- function(data,
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

  # Ensure that the required identifier column exists
  if (!id %in% names(data)) {
    stop(sprintf("Column '%s' not found in data.", id))
  }

  # Log the checking process
  log <- LogCat(
    "Checking whether references exist in library",
    silent = silent,
    log = log
  )

  # Extract the identifier column from data using tidy evaluation
  data.ids <- dplyr::pull(data, !!rlang::sym(id))

  # Retrieve IDs from the 'extra' field in items
  zotero.ids <- ZoteroId(id.type, items$extra)

  # Identify records not present in the Zotero library
  unique.data <- dplyr::filter(data, !data.ids %in% zotero.ids)

  # If duplicates exist, process them further
  if (nrow(unique.data) < nrow(data)) {

    # Filter duplicate records from the input data
    data.duplicates <- dplyr::filter(data, data.ids %in% zotero.ids) |>
      AddMissing(
        missing.names = c(created, last.modified),
        na.type = NA_character_,
        location = NULL
      )

    # Retrieve corresponding duplicate records from the Zotero library
    zotero.duplicates <- items |>
      dplyr::filter(zotero.ids %in% data.ids) |>
      dplyr::arrange(match(ZoteroId(id.type, extra), data.ids)) |>
      dplyr::distinct(extra, .keep_all = TRUE) |>
      AddMissing(
        missing.names = c("dateAdded", "dateModified"),
        na.type = NA_character_,
        location = NULL
      )

    # Get modification dates, using 'last.modified' with a fallback to 'created'
    data.modified <- dplyr::coalesce(
      dplyr::pull(data.duplicates, !!rlang::sym(last.modified)),
      dplyr::pull(data.duplicates, !!rlang::sym(created))
    )

    zotero.modified <- dplyr::coalesce(
      dplyr::pull(zotero.duplicates, "dateModified"),
      dplyr::pull(zotero.duplicates, "dateAdded")
    )

    # Determine which duplicates have been modified since addition to Zotero
    if (remove.duplicates) {
      modified <- data.modified > zotero.modified
      modified[is.na(modified)] <- FALSE
    } else {
      modified <- rep(TRUE, length(data.modified))
    }

    # Copy desired Zotero columns (if available) to the duplicates in data
    desired.cols <- c("key", "version", "collections")
    cols.to.copy <- intersect(desired.cols, names(zotero.duplicates))

    if (length(cols.to.copy) > 0) {
      data.duplicates[modified, cols.to.copy] <-
        zotero.duplicates[modified, cols.to.copy]
    }

    # Combine unique data with the (modified) duplicate records
    if (remove.duplicates) {
      unique.data <- dplyr::bind_rows(unique.data, data.duplicates[modified, ])
    } else {
      unique.data <- dplyr::bind_rows(unique.data, data.duplicates)
    }

    names(unique.data)

    # Log the number of duplicates removed
    removed.count <- nrow(data) - nrow(unique.data)
    log <- LogCat(
      sprintf("Removed %s", Numerus(removed.count, "duplicate")),
      silent = silent,
      log = log
    )

    # Log how many items were modified
    if (any(modified)) {
      log <- LogCat(
        sprintf("Modified %s", Numerus(sum(modified), "item")),
        silent = silent,
        log = log
      )
    }

  }

  return(list(data = unique.data, log = log))
}
