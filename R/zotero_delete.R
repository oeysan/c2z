#' @title Delete collections and items from a Zotero library
#' @description Cleaning the Zotero library
#' @param zotero A list with information on the specified Zotero library (e.g.,
#'   id, API key, collections, and items)
#' @param delete.collections Try to delete specified collections, Default: TRUE
#' @param delete.items Try to delete specified items?, Default: TRUE
#' @param delete.limit Number of collections/items to delete per request (max
#'   50), Default: 50
#' @param force Force is seldom wise, but sometimes..., Default: FALSE
#' @param ragnarok Delete EVERYTHING in the specified library, Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return A list with information on the specified Zotero library (e.g.,
#'   deleted collections and items)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Delete everything in a group
#'   example <- ZoteroDelete(
#'     Zotero(
#'       user = FALSE,
#'       id = "4988497",
#'       api = "RqlAmlH5l1KPghfCseAq1sQ1"
#'     ),
#'     ragnarok = TRUE,
#'     force = TRUE
#'   )
#' }
#' @seealso
#'  \code{\link[httr]{add_headers}}, \code{\link[httr]{RETRY}}
#' @rdname ZoteroDelete
#' @export
ZoteroDelete <- \(zotero,
                  delete.collections = FALSE,
                  delete.items = FALSE,
                  delete.limit = 50,
                  force = FALSE,
                  ragnarok = FALSE,
                  silent = FALSE) {

  # Function to delete collection and items
  MassDelete <- \(zotero,
                  append.collections = FALSE,
                  delete.limit,
                  silent = FALSE) {

    # Find latest Zotero version
    version <- ZoteroGet(
      zotero,
      limit = 1,
      all.results = FALSE,
      silent = TRUE
    )$version

    # Define data as collection if append.collection is set to TRUE
    if (append.collections) {
      data <- zotero[["collections"]]
      # Else define data as items
    } else {
      data <- zotero[["items"]]
    }

    #Number of items/collections
    total.data <- nrow(data)

    # Split metadata into acceptable chunks if k > post.limit
    metadata <- SplitData(data, delete.limit)

    # JUST SOME MORE POINTLESS LINGUISTICS
    if (append.collections) {
      items <- Pluralis(total.data, "collection")
    } else {
      items <- Pluralis(total.data, "item")
    }
    delete <- Pluralis(length(metadata), "DELETE request")

    # Create message
    delete.message <- sprintf("Deleting %s using %s", items, delete)

    # Add message to log
    zotero$log <- LogCat(
      delete.message,
      silent = silent,
      log = zotero$log
    )

    # Start time for query
    query.start <- Sys.time()
    # Cycle through metadata
    for (i in seq_along(metadata)) {

      # Define either collections or items
      if (append.collections) {
        items <- Pluralis(
          length(metadata[[i]]$key),
          "collection"
        )
        query.type <- "collectionKey"
      } else {
        items <- Pluralis(
          length(metadata[[i]]$key),
          "item"
        )
        query.type <- "itemKey"
      }
      query <- stats::setNames(
        list(paste(metadata[[i]]$key, collapse = ",")), query.type
      )

      # Add version to header
      json.header <- httr::add_headers(
        "If-Unmodified-Since-Version" = version
      )

      # Delete collections/items from library
      httr.delete <- Online(
        httr::RETRY(
          "DELETE",
          ZoteroUrl(
            zotero$url,
            append.collections = append.collections,
            api = zotero$api
          ),
          json.header,
          query = query,
          quiet = TRUE),
        silent = TRUE,
        message = "Zotero delete"
      )
      zotero$log <- append(zotero$log, httr.delete$log)

      # Update version
      if (!httr.delete$error) {
        version <- httr.delete$data$headers$`last-modified-version`
      }

      # Estimate time of arrival
      log.eta <-
        LogCat(
          Eta(query.start,
              i,
              length(metadata)),
          silent = silent,
          flush = TRUE,
          append.log = FALSE
        )
    }

    # Append eta log to log
    zotero$log <- append(zotero$log, log.eta)

    return (zotero)

  }

  # Do if ragnarok is TRUE
  if (ragnarok) {

    # Send error message if force is FALSE
    if (!force) {
      zotero$log <- LogCat(
        "To delete the entire library set force to TRUE",
        fatal = TRUE,
        silent = silent,
        log = zotero$log
      )
    }

    # Select all collections and items
    zotero$collections <- ZoteroGet(
      zotero,
      silent = TRUE,
      format = "keys",
      append.collections = TRUE
    )$results

    zotero$items <- ZoteroGet(
      zotero,
      silent = TRUE,
      format = "keys"
    )$results

    # Set delete.collections and delete.items to TRUE
    delete.collections <- TRUE
    delete.items <- TRUE
  }

  # Delete collections if delete.collections = TRUE
  if (delete.collections ) {
    if (is.null(zotero$collections)) {
      zotero$log <- LogCat(
        "There are no collections to DELETE",
        silent = silent,
        log = zotero$log
      )
    } else {
      zotero <- MassDelete(zotero, TRUE, delete.limit, silent)
    }
  }
  # Delete items if delete.items = TRUE
  if (delete.items) {
    if (is.null(zotero$items)) {
      zotero$log <- LogCat(
        "There are no items to DELETE",
        silent = silent,
        log = zotero$log
      )
    } else {
      zotero <- MassDelete(zotero, FALSE, delete.limit, silent)
    }
  }

  return (zotero)

}
