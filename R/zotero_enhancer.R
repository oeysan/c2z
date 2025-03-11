#' Enhance Zotero Records with External Metadata via Zotero Lookups
#'
#' This function iterates over a data frame (or tibble) of Zotero records and attempts to enhance
#' each record by retrieving additional metadata from external sources. It uses DOI or ISBN information,
#' via the \code{c2z} package, to fetch external metadata and then merges these updates into the original
#' Zotero record. Special handling is provided for records representing book sections.
#'
#' @param zotero.data A data frame (or tibble) containing Zotero records. Each row represents a record
#'   that may include fields such as \code{abstractNote}, \code{DOI}, \code{url}, \code{ISBN},
#'   \code{key}, \code{itemType}, \code{pages}, and \code{creators}.
#' @param external.data Optional. A data frame or list containing pre-fetched external metadata.
#'   If provided and non-empty, the function will use this external metadata to update the Zotero records
#'   instead of querying the external APIs. Defaults to \code{NULL}.
#' @param use.doi Logical; if \code{TRUE} (default), the function will attempt to retrieve external metadata
#'   using DOI information. Defaults to \code{TRUE}.
#' @param use.semantic Logical; if \code{TRUE} (default), the function will attempt to retrieve external metadata
#'   from Semantic Scholar using DOI information. Defaults to \code{TRUE}.
#' @param use.isbn Logical; if \code{TRUE} (default), the function will attempt to retrieve external metadata
#'   using ISBN information when DOI retrieval is not successful. Defaults to \code{TRUE}.
#'
#' @return A data frame containing the enhanced Zotero records with external metadata merged into the original records.
#'
#' @details
#' For each record in \code{zotero.data}, the function performs the following steps:
#'
#' \enumerate{
#'   \item If \code{external.data} is provided and non-empty, that data is used to update the record.
#'   \item Otherwise, the function skips processing the record if an abstract already exists.
#'   \item It then attempts to extract a DOI from the \code{DOI} field, or if missing, from the \code{url} field.
#'   \item The \code{ISBN} field is processed by removing spaces and extracting the first value (in case of comma separation).
#'   \item If a valid DOI is found and \code{use.doi} is \code{TRUE}, external metadata is retrieved using \code{c2z::ZoteroDoi()}.
#'         Otherwise, if an ISBN is available and \code{use.isbn} is \code{TRUE}, metadata is retrieved using \code{c2z::ZoteroIsbn()}.
#'   \item Once external metadata is successfully retrieved, an internal helper function \code{UpdateTibble} is called.
#'         This helper function merges the external metadata into the original record. For book sections, additional
#'         adjustments are made to handle creator information, pages, and edition fields.
#'   \item Finally, the enhanced record is merged back into the original data frame using \code{UpdateInsert()}.
#' }
#'
#' @examples
#' \dontrun{
#'   # Enhance Zotero records using DOI/ISBN lookup:
#'   example <- Cristin("762394", silent = TRUE)$results
#'   print(example$libraryCatalog)
#'   example$DOI <- "https://doi.org/10.22358/jafs/74164/2007"
#'   enhanced.data <- ZoteroEnhancer(example)
#'   print(enhanced.data$libraryCatalog)
#' }
#'
#' @export
ZoteroEnhancer <- \(zotero.data,
                    external.data = NULL,
                    use.doi = TRUE,
                    use.semantic = TRUE,
                    use.isbn = TRUE) {

  if (!any(nrow(GoFish(zotero.data))) || !any(nrow(GoFish(zotero.data)))) {
    return (zotero.data)
  }

  # Internal helper to merge a single Zotero record with external metadata.
  UpdateTibble <- \(x, external.data) {

    # Visible bindings
    creatorType <- NULL

    # Update the external metadata with essentials from the original data
    external.data$key <- GoFish(x$key)
    external.data$version <- GoFish(x$version)
    external.data$collections <- GoFish(x$collections)
    external.data$extra <- GoFish(x$extra)

    # Determine if the record is a book section by checking if the original record's
    # itemType is "bookSection" while the external metadata indicates a "book".
    book.section <- GoFish(
      x$itemType == "bookSection" & external.data$itemType == "book",
      FALSE
    )

    if (book.section) {
      external.data$pages <- GoFish(x$pages)
      external.data$bookTitle <- GoFish(external.data$title)
      external.data$title <- GoFish(x$title)

      if (any(nrow(GoFish(x$creators[[1]])))) {
        x$creators[[1]] <- x$creators[[1]] |>
          dplyr::filter(creatorType != "editor")
      }
      if (any(nrow(external.data$creators[[1]]))) {
        external.data$creators[[1]] <- external.data$creators[[1]] |>
          dplyr::mutate(creatorType = "editor") |>
          dplyr::bind_rows(x$creators[[1]])
      }
    }

    # If the external metadata's creator list is shorter than the Zotero record's,
    # then use the longer list from the Zotero record.
    if (any(nrow(GoFish(x$creators[[1]])) &&
            any(nrow(GoFish(external.data$creators[[1]]))))) {
      if (nrow(external.data$creators[[1]]) < nrow(x$creators[[1]])) {
        external.data$creators[[1]] <- x$creators[[1]]
      }
    }

    # Fix edition and page count values if needed.
    external.data$edition <- GoFish(FixEdition(external.data$edition))
    external.data$numPages <- GoFish(FixEdition(external.data$numPages))

    # Merge the external metadata with the original record.
    new.zotero.data <- UpdateInsert(x, external.data, check.missing = TRUE)

    # Preserve the book section itemType if applicable.
    if (book.section) {
      new.zotero.data$itemType <- "bookSection"
    }

    return(new.zotero.data)
  }

  # If external.data is provided and non-empty, use it directly.
  if (any(nrow(external.data))) {
    # Assume x is a placeholder record to be updated.
    zotero.data <- UpdateTibble(zotero.data, external.data)
    return(zotero.data)
  }

  for (i in seq_len(nrow(zotero.data))) {
    x <- zotero.data[i, ]

    # Skip processing if an abstract already exists.
    if (!any(is.na(GoFish(x$abstractNote)))) next

    # Attempt to extract DOI from the DOI field, or if not present, from the URL.
    doi <- CheckDoi(x$DOI)
    if (is.null(doi)) doi <- CheckDoi(x$url)

    # Process the ISBN field by removing spaces and selecting the first value.
    isbn <- CheckDoi(x$ISBN)

    # If an abstract is already present (redundant check), skip the record.
    if (!any(is.na(GoFish(x$abstractNote)))) {
      next
    }

    # Check if item has isbn
    if (!is.null(isbn) && use.isbn) {
      external.data <- c2z::ZoteroIsbn(isbn)$data
    }

    # Check if item has doi and external data is still empty
    if (is.null(external.data) && (!is.null(doi) && use.doi)) {
      external.data <- c2z::ZoteroDoi(doi, use.semantic = use.semantic)$data
    }

    # If no external metadata was retrieved, skip the record.
    if (is.null(external.data)) next

    # Merge the external metadata into the Zotero record.
    new.zotero.data <- UpdateTibble(x, external.data)

    # Replace the original record with the enhanced record.
    zotero.data <- UpdateInsert(zotero.data, new.zotero.data)
  }

  # Ensure that the tibble has the correct format
  zotero.data <- ZoteroFormat(zotero.data, check.structure = TRUE)
  zotero.data$creators[[1]] <- (FixCreators(zotero.data$creators[[1]]))

  return(zotero.data)
}

################################################################################
################################################################################
################################Helper Functions################################
################################################################################
################################################################################


#' Upsert and Update Data Frames
#'
#' Combines two data frames by updating rows in \code{x} with values from \code{y} based on a common key,
#' and inserting new rows from \code{y} that are not present in \code{x}. The function first harmonizes the
#' column structures of both data frames by adding missing columns and coercing types as necessary.
#'
#' @param x A data frame to be updated.
#' @param y A data frame containing new values to update \code{x}. Must include the column specified by \code{key}.
#' @param key A character string specifying the unique key column used for matching rows. Defaults to \code{"key"}.
#' @param check.missing Logical; if \code{TRUE}, performs a cell-by-cell update only when the new value is not missing.
#'   Missing values are defined as \code{NA} for atomic types or an empty list for list columns. If \code{FALSE},
#'   a standard upsert is performed using \code{dplyr::rows_upsert}. Defaults to \code{FALSE}.
#'
#' @return A data frame resulting from updating \code{x} with values from \code{y}.
#'
#' @details
#' The function works in several steps:
#'
#' \enumerate{
#'   \item It computes the union of all column names from \code{x} and \code{y} and adds any missing columns
#'         to both data frames using the internal helper function \code{AddColumns}. Missing columns are filled
#'         with an appropriate \code{NA} value based on their type.
#'   \item Both \code{x} and \code{y} are reordered to have the same column order.
#'   \item For each common column (excluding the key), if \code{x}'s column is entirely \code{NA} or if the
#'         data types differ, coercion is performed to ensure compatibility between \code{x} and \code{y}.
#'   \item When \code{check.missing} is \code{TRUE}, the function iterates over each common key and updates each
#'         cell in \code{x} only if the corresponding cell in \code{y} is not missing. Otherwise, it uses
#'         \code{dplyr::rows_upsert} to perform a standard upsert.
#'   \item New rows present in \code{y} but not in \code{x} are appended.
#' }
#'
#' @examples
#' \dontrun{
#'   # Example data frames:
#'   df1 <- data.frame(
#'     key = 1:3,
#'     a = c(NA, 2, NA),
#'     b = c("x", NA, "z"),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   df2 <- data.frame(
#'     key = c(2, 3, 4),
#'     a = c(5, 6, 7),
#'     b = c("y", "w", "v"),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   # Standard upsert (check.missing = FALSE):
#'   result <- UpdateInsert(df1, df2, key = "key", check.missing = FALSE)
#'
#'   # Cell-by-cell update (check.missing = TRUE):
#'   result <- UpdateInsert(df1, df2, key = "key", check.missing = TRUE)
#' }
#'
#' @importFrom dplyr mutate select rows_upsert bind_rows filter
#' @export

UpdateInsert <- function(x, y, key = "key", check.missing = FALSE) {

  AddColumns <- function(x, y) {
    missing.cols <- setdiff(names(y), names(x))
    for (col in missing.cols) {
      new.val <- if (is.factor(y[[col]])) {
        factor(NA, levels = levels(y[[col]]))
      } else if (is.integer(y[[col]])) {
        NA_integer_
      } else if (is.numeric(y[[col]])) {
        NA_real_
      } else if (is.character(y[[col]])) {
        NA_character_
      } else if (is.logical(y[[col]])) {
        NA
      } else {
        NA
      }
      x <- dplyr::mutate(x, !!col := new.val)
    }
    return(x)
  }

  CoerceNa <- \(x, y) {
    n <- length(x)
    if (is.factor(y)) {
      return(factor(rep(NA, n), levels = levels(y)))
    } else if (is.character(y)) {
      return(rep(NA_character_, n))
    } else if (is.integer(y)) {
      return(rep(NA_integer_, n))
    } else if (is.numeric(y)) {
      return(rep(NA_real_, n))
    } else if (is.logical(y)) {
      return(rep(NA, n))
    } else if (is.list(y)) {
      return(vector("list", n))
    } else {
      return(rep(NA, n))
    }
  }

  if (!any(nrow(y))) return(x)
  if (!any(nrow(x))) return(y)

  # Compute the union of all column names.
  all.columns <- union(names(x), names(y))

  # Ensure both x and y have all columns by adding missing ones.
  x <- AddColumns(x, y)
  y <- AddColumns(y, x)

  # Reorder both data frames to have the same column order.
  x <- dplyr::select(x, dplyr::all_of(all.columns))
  y <- dplyr::select(y, dplyr::all_of(all.columns))

  # Get common columns.
  common.cols <- intersect(names(x), names(y))

  # For each common column (except key), if x's column is entirely NA,
  # reinitialize it with an NA vector having the same type as y's column.
  for (col in setdiff(common.cols, key)) {
    if (all(is.na(x[[col]]))) {
      x[[col]] <- CoerceNa(x[[col]], y[[col]])
    } else if (!identical(class(x[[col]]), class(y[[col]]))) {
      # If there are valid values in x, then convert y's column to x's type.
      if (is.logical(x[[col]])) {
        y[[col]] <- as.logical(y[[col]])
      } else if (is.numeric(x[[col]])) {
        y[[col]] <- as.numeric(y[[col]])
      } else if (is.integer(x[[col]])) {
        y[[col]] <- as.integer(y[[col]])
      } else if (is.character(x[[col]])) {
        y[[col]] <- as.character(y[[col]])
      } else if (is.factor(x[[col]])) {
        y[[col]] <- factor(y[[col]], levels = levels(x[[col]]))
      } else if (is.list(x[[col]])) {
        y[[col]] <- as.list(y[[col]])
      }
    }
  }

  if (check.missing) {
    # Helper to determine if a cell value is "missing":
    # For list columns: if the cell is a one-element list whose element is empty.
    # For atomic types: if the value is NA.
    is.missing <- function(val) {
      if (is.list(val)) {
        if (length(val) == 1 && length(val[[1]]) == 0) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        return(is.na(val))
      }
    }

    # Get common keys between x and y.
    common.keys <- intersect(x[[key]], y[[key]])

    # For each matching key, update cell-by-cell.
    for (k in common.keys) {
      ix <- which(x[[key]] == k)
      iy <- which(y[[key]] == k)
      for (col in setdiff(common.cols, key)) {
        new.val <- y[[col]][iy]
        # If y's value is valid (i.e. not missing), overwrite x's value.
        if (!is.missing(new.val)) {
          x[[col]][ix] <- new.val
        }
      }
    }

    # Append rows from y that do not exist in x.
    new.keys <- setdiff(y[[key]], x[[key]])
    if (length(new.keys) > 0) {
      x <- dplyr::bind_rows(x, dplyr::filter(y, !!rlang::sym(key) %in% new.keys))
    }
  } else {
    # Standard upsert: update matching rows and insert new rows.
    x <- dplyr::rows_upsert(x, y, by = key)
  }

  return(x)
}
