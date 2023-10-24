#' @title Make a Zotero friendly format of input data
#' @description Make a Zotero friendly format of input data, will add a zotero
#' key.
#' @param data A list of metadata or something else, Default: NULL
#' @param format The format of the input data (e.g., 'JSON', 'versions', 'keys).
#' , Default: NULL
#' @param prefix Add a prefix to the metadata (e.g., user/userID), Default: NULL
#' @param check.structure Check that the structure of a data frame is correct
#' according to the Zotero type, Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: TRUE
#' @return A zotero friendly tibble if requested otherwise format the data
#' according to format.
#' @examples
#' \donttest{
#'   ZoteroFormat(
#'     list(title = "This is a test", itemType = "document")
#'   )
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}, \code{\link[tibble]{as_tibble}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[dplyr]{mutate_all}}, \code{\link[dplyr]{na_if}},
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}},
#'  \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#' @rdname ZoteroFormat
#' @export
ZoteroFormat <- \(data = NULL,
                  format = NULL,
                  prefix = NULL,
                  check.structure = FALSE,
                  silent = FALSE) {

  # Run if not empty
  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  # Return as tibble if format is keys/versions
  if (any(format %in% c("keys", "versions"))) {

    # If string of characters (e.g., keys only)
    if (format == "keys") {

      data <- strsplit(data, "\n") |>
        unlist() |>
        (\(x) tibble::tibble(key = x))()

      # Else if keys and versions
    } else {

      data <- jsonlite::fromJSON(data) |>
        (\(x) tibble::tibble(key = names(x), version = unlist(x)))()

    }

    return (data)

  }

  # Visible bindings
  creators <- NULL

  multiline.items <- c("tags",
                       "extra",
                       "abstractNote",
                       "note",
                       "creators",
                       "relations")
  double.items <- c("version", "mtime")
  list.items <- c("collections", "relations", "tags")

  # Check if metadata and not in a data frame
  if (!is.data.frame(data) &
      (any(format == "json", is.null(format)))) {

    # Check that first element of data is a list
    if (is.list(data[[1]])) data <-  unlist(data, recursive = FALSE)

    # Check all element in the meta list
    data.list <- lapply(seq_along(data), \(i) {

      # Define data
      x <- data[[i]]
      names <- names(data[i])

      # Make certain fields not in multiline are strings
      if (!names %in% multiline.items) x <- ToString(x)

      # Add to list if element is a data frame
      ## Make certain list.items is in a list
      if (is.data.frame(x) | names %in% list.items) {
        x <- if (all(is.na(GoFish(as.character(unlist(x)))))) NA else list(x)

        # Make certain double.items are double
      } else if (names %in% double.items) {
        x <- as.double(x)
        # Else make certain remaining items are character
      } else {
        x <- as.character(x)
      }

      return (x)

    })
    # Name elements
    names(data.list) <- names(data)
    # Keep number of columns fixed for simple conversion to tibble/JSON
    ## Replace empty elements with NA
    data.list[lengths(data.list) == 0] <- NA
    # Set key and initial version is missing
    if (!"key" %in% names(data.list)) {
      data.list <- c(key = ZoteroKey(), version = 0, data.list)
    }
    # Remove elements not in category if item
    if (!"parentCollection" %in% names(data.list)) {
      data.list <- data.list[
        names(data.list) %in%
          c("key", "version", ZoteroTypes(data.list$itemType))]
    }

    # Format as tibble and remove empty elements
    data <- tibble::as_tibble(data.list[lengths(data.list) != 0])

  }

  # Set data as tibble if data frame
  if (is.data.frame(data)) {

    data <- tibble::as_tibble(data) |>
      # Replace empty string with NA
      dplyr::mutate_if(is.character, list(~dplyr::na_if(., ""))) |>
      dplyr::mutate(
        # Make certain tags is a data.frame within a list
        dplyr::across(
          dplyr::any_of("tags"), ~ purrr::map(tags, ~ {
            if (all(!is.na(.x))) as.data.frame(.x)
          })
        ),
        # Make certain that parentCollection is a character
        dplyr::across(dplyr::any_of("parentCollection"), as.character),
        # Add prefix if defined
        prefix = GoFish(prefix),
        # Fix creators
        creators = GoFish(purrr::map(creators, FixCreators))
      ) |>
      # Remove empty columns
      dplyr::select(dplyr::where(~sum(!is.na(.x)) > 0))

    # Check that each entry has the correct structure
    if (check.structure) {

      data <- seq_len(nrow(data)) |>
        purrr::map(
          \(i) ZoteroFormat(as.list(data[i, ])),
          .progress = if (!silent) "Checking Zotero structure" else FALSE
        ) |>
        bind_rows()

    }

    # Else convert to string
  } else {

    data <- ToString(data, "\n")

  }

  return (data)

}
