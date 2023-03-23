#' @title Access the Zotero library
#' @description The function uses information stored in the `zotero` list to
#'   access specified collections and items in the Zotero library
#' @param zotero A list with information on the specified Zotero library (e.g.,
#'   id, API key, collections, and items)
#' @param case.insensitive Disregard letter casing when searching for
#'   collections, Default: TRUE
#' @param ancestor Trace the lineage of a collection (i.e., find the top-level
#'   collection), Default: FALSE
#' @param recursive Find all nested collections, Default: FALSE
#' @param create Create missing collections, Default: FALSE
#' @param limit Number of results per query (max 100), Default: 100
#' @param start Starting position of query (0 = first result), Default: 0
#' @param get.items Fetch items along with collections, Default: TRUE
#' @param item.type Items to search for (NULL = everything), Default: NULL
#' @param all.results Find all results in query, Default: TRUE
#' @param max.results Do you need a limit?, Default: NULL
#' @param result.type Pointless linguistics to display result type (default =
#'   `result`), Default: NULL
#' @param include.bib Include HTML-formatted bibliography from Zotero, Default:
#'   FALSE
#' @param style Citation style to use for appended bibliography and/or
#'   citations, Default: apa
#' @param force Force is seldom wise, but sometimes..., Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return A list with information on the specified Zotero library (e.g.,
#'   collections and items)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Access the default group library
#'   example <- ZoteroLibrary(
#'     Zotero(
#'       user = FALSE,
#'       id = "4827927",
#'       api = "RqlAmlH5l1KPghfCseAq1sQ1"
#'     )
#'   )
#'
#'   # Print index using `ZoteroIndex`
#'   ZoteroIndex(example$items) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[tibble]{tibble}}
#' @rdname ZoteroLibrary
#' @export
ZoteroLibrary <- \(zotero,
                   case.insensitive = TRUE,
                   ancestor = FALSE,
                   recursive = FALSE,
                   create = FALSE,
                   limit = 100,
                   start = 0,
                   get.items = TRUE,
                   item.type = NULL,
                   all.results = TRUE,
                   max.results = NULL,
                   result.type = NULL,
                   include.bib = FALSE,
                   style = "apa",
                   force = FALSE,
                   silent = FALSE) {

  # Visible bindings
  path <- key <- itemType <- meta <- item <- NULL

  # Run if collection.key is specified
  if (!is.null(zotero$collection.key)) {

    # Query all collections in Zotero library
    if (is.null(result.type)) {
      result.collection <- "subcollection"
    } else {
      result.collection <- result.type
    }
    zotero <- ZoteroGet(zotero,
                        silent = silent,
                        result.type = result.collection)

    # Convert json data to data list
    if (zotero$data.cache$status_code == 200) {
      meta <- jsonlite::fromJSON(
        ParseUrl(zotero$data.cache, "text")
      )

      # Find number of subcollections and items
      n.collections <- max(0,meta$meta$numCollections)
      n.items <- max(0,meta$meta$numItems)

      # Add or append collections to zotero list
      zotero$collections <- AddAppend(
        zotero$results, zotero$collections
      )

      # Find items if n.items > 0 and get.items = TRUE
      if (n.items > 0 & get.items) {

        # Query items in collection
        zotero <- ZoteroGet(zotero,
                            append.items = TRUE,
                            item.type = item.type,
                            limit = limit,
                            start = start,
                            all.results = all.results,
                            max.results = max.results,
                            result.type = "item",
                            include.bib = include.bib,
                            style = style,
                            force = force,
                            silent = silent)

        # Add or append items to zotero list
        zotero$items <- AddAppend(zotero$results, zotero$items) |>
          dplyr::distinct()

      }

      # Just some pointless linguistics
      collections <- Pluralis(n.collections, "subcollection", "subcollections")

      # More pointless linguistics
      items <- Pluralis(n.items, "item", "items")

      # Show number of subcollections and item
      zotero$log <- LogCat(
        sprintf("Collection %s (%s) contains: %s and %s",
                meta$data$name,
                meta$key,
                collections,
                items),
        silent = silent,
        log = zotero$log
      )

      # Run if more than zero subcollections and recursive is set to TRUE
      if (n.collections > 0 & recursive) {

        # Query all collections in Zotero library
        zotero <- ZoteroGet(zotero,
                            append.collections = TRUE,
                            all.results = TRUE,
                            force = force,
                            silent = silent,
                            result.type = "subcollection")

        # Find keys
        keys <- zotero$results$key

        # Check if keys are in the collection path
        if (!is.null(zotero$collection.path)) {
          keys <- keys[keys %in% zotero$collection.path]
        }

        # Run if keys have length
        if (length(keys)) {

          # Loop through keys extracting data
          for (i in seq_along(keys)) {

            zotero$collection.key <- keys[i]
            zotero <- ZoteroLibrary(zotero, case.insensitive, ancestor,
                                    recursive, create, limit, start, get.items,
                                    item.type, all.results, max.results,
                                    "nested subcollection", include.bib, style,
                                    force, silent)

          }

          # Restore initial collection key
          zotero$collection.key <- zotero$collections$key[1]

        }

      }

      # Set collection names
      zotero$collection.names <- zotero$collections$name[1]

    }

  } else {

    # Query all collections in Zotero library
    zotero <- ZoteroGet(zotero,
                        append.collections = TRUE,
                        force = force,
                        silent = silent,
                        result.type = "collection")

    # If searching for a specific collection name
    if (!is.null(zotero$collection.names)) {

      # if force is not set
      # send error message if collection contains > 10 subfolders
      if (length(zotero$collection.names) > 10 & !force) {
        zotero$log <- LogCat(
          "You have provided ten or more collection subfolders,
          is this correct? If so set force = TRUE",
          fatal = TRUE,
          log = zotero$log
        )
      }

      # Run for each collection name/level
      for (i in seq_along(zotero$collection.names)) {

        # Find parent, if k=1 there are no parents, strangely enough
        parent <- if (i == 1) "FALSE" else parent

        # Ignore case of collection name if case insensitive is set to TRUE
        ## Thus collection will not be created if Collection exists
        if (case.insensitive) {
          zotero$results$name <- tolower(zotero$results$name)
          new.collection <- tolower(zotero$collection.names[i])
        } else {
          new.collection <- zotero$collection.names[i]
        }

        # Find collection key by name and parent
        zotero$collection.key <- zotero$results$key[
          zotero$results$name %in% new.collection &
            zotero$results$parentCollection %in% parent
        ]

        # Choose first if more than one collection named collections[i],
        if (length(zotero$collection.key) > 1) {

          zotero$collection.key <- zotero$collection.key[[1]]
          zotero$log <- LogCat(
            sprintf("Found multiple collections named %s, selecting: %s",
                    zotero$collection.names[i],
                    zotero$collection.key),
            silent = silent,
            log = zotero$log
          )
          # Else select the one found
        } else if (length(zotero$collection.key) == 1) {

          zotero$log <- LogCat(
            sprintf("Found collection %s (%s)",
                    zotero$collection.names[i],
                    zotero$collection.key),
            silent = silent,
            log = zotero$log
          )

          # Send error if no keys is found and create is not set to TRUE
        } else if (length(zotero$collection.key) == 0 & !create) {

          zotero$log <- LogCat(
            sprintf("The collection `%s` was not found. Please set `create` to
            TRUE if you want to create a new collection.",
                    zotero$collection.names[i]),
            fatal = TRUE,
            silent = silent,
            log = zotero$log
          )
          # Else create new collection
        } else if (length(zotero$collection.key) == 0 & create) {

          zotero$log <- LogCat(
            sprintf("Creating a new collection named %s",
                    zotero$collection.names[i]),
            silent = silent,
            log = zotero$log
          )

          # Define collection data
          zotero$collections <- tibble::tibble(
            key = ZoteroKey(),
            version = 0,
            name = zotero$collection.names[i],
            parentCollection = as.character(parent)
          )

          # Create new collection
          zotero <- ZoteroPost(zotero,
                               post.collections = TRUE,
                               silent = silent)

        }

        # Define parent
        parent <- zotero$collection.key
        # Set ancestor key
        if (i == 1) ancestor.key <- parent
        # Add to path
        path <- c(path, parent)

      }

      # Use ancestor and recursive if ancestor is set to TRUE
      if (ancestor) {
        zotero$collection.key <- ancestor.key
        zotero$collection.path <- path
        recursive <- TRUE
      }

      # Fetch specified data
      zotero <- ZoteroLibrary(zotero, case.insensitive, ancestor, recursive,
                              create, limit, start, get.items, item.type,
                              all.results, max.results, result.type,
                              include.bib, style, force, silent)

      # Else find all collections and items
    } else {

      # Fetch total number of collections in Zotero library
      n.collections <- max(0, as.numeric(
        zotero$data.cache$headers$`total-results`)
      )

      # Add collections to zotero list
      zotero$collections <- zotero$results

      # Only get items if get.items = TRUE
      if (get.items) {

        # Query all items in Zotero library
        zotero <- ZoteroGet(zotero,
                            append.items = TRUE,
                            item.type = item.type,
                            limit = limit,
                            start = start,
                            all.results = all.results,
                            max.results = max.results,
                            result.type = "item",
                            include.bib = include.bib,
                            style = style,
                            force = force,
                            silent = silent)

        # Add item data to zotero list
        zotero$items <- zotero$results

      } else {

        # Query all items in Zotero library
        zotero <- ZoteroGet(zotero,
                            append.items = TRUE,
                            item.type = item.type,
                            limit = 1,
                            start = start,
                            all.results = FALSE,
                            max.results = max.results,
                            force = force,
                            silent = TRUE,
                            result.type = "item")

      }

      # Fetch total number of items in Zotero library
      n.collections <- max(0, as.numeric(
        zotero$data.cache$headers$`total-results`)
      )

      # Just some pointless linguistics
      collections <- Pluralis(n.collections,
                              "collection", "collections")

      # More pointless linguistics
      items <- Pluralis(max(0, n.collections), "item", "items")

      # Print number of collections and items in library
      zotero$log <- LogCat(
        sprintf("The zotero library contains: %s and %s",
                collections, items),
        silent = silent,
        log = zotero$log
      )

    }

  }

  # Make certain that items are distinct
  if (!is.null(zotero$items)) {
    zotero$items <- zotero$items |>
      dplyr::distinct() |>
      dplyr::arrange(
        itemType == "attachment" | itemType == "note"
      )

    # Set number of items
    zotero$n.items <- zotero$items |>
      dplyr::filter(!grepl("attachment|note", itemType)) |>
      nrow()

    # Set number of extras (i.e. notes and attachments)
    zotero$n.attachments <- zotero$items |>
      dplyr::filter(grepl("attachment|note", itemType)) |>
      nrow()
  }
  # Order collections by when they were created
  if (!is.null(zotero$collections)) {
    zotero$collections <- zotero$collections |>
      dplyr::arrange(version) |>
      dplyr::distinct()

    # Set number of collections
    zotero$n.collections <- nrow(zotero$collections)
  }

  return (zotero)
}
