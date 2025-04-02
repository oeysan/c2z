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
#' @param search.collections Search all collections if collection.key fails,
#' Default: TRUE
#' @param get.collections Fetch collections, Default: TRUE
#' @param get.items Fetch items, Default: TRUE
#' @param item.type Items to search for (NULL = everything), Default: NULL
#' @param all.results Find all results in query, Default: TRUE
#' @param max.results Do you need a limit?, Default: NULL
#' @param library.type Commma-separated data from Zotero (i.e., data, bib,
#' citation), Default: NULL
#' @param linkwrap Set URL (e.g., DOI) as HTML link (1 = yes), Default: 1
#' @param style Citation style to use for appended bibliography and/or
#'   citations, Default: apa
#' @param locale Desired language format of bibliography, Default: 'en-US'
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
#'       api = "Io1bwAiOjB1jPgUNegjJhJxF"
#'     )
#'   )
#'   # Print index using `ZoteroIndex`
#'   if (any(nrow(example$items))) {
#'     ZoteroIndex(example$items) |>
#'       dplyr::select(name) |>
#'       print(width = 80)
#'   }
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
                   search.collections = TRUE,
                   get.collections = TRUE,
                   get.items = TRUE,
                   item.type = NULL,
                   all.results = TRUE,
                   max.results = NULL,
                   library.type = NULL,
                   linkwrap = 1,
                   style = "apa",
                   locale = "en-US",
                   force = FALSE,
                   silent = FALSE) {

  # Visible bindings
  path <- key <- itemType <- meta <- item <- items <- NULL

  # Fetch items if get.collections is TRUE
  if (get.collections) {

    # Log number of units and periods
    zotero$log <-  LogCat(
      "Searching for collections",
      silent = silent,
      log = zotero$log
    )

    # Find specified collection if collection.key is defined
    if (!is.null(zotero$collection.key) & !ancestor & !recursive) {

      # Query collection key
      zotero <- ZoteroGet(
        zotero,
        silent = silent,
        result.type = "collection key"
      )

      # Define  collections
      zotero$collections <- zotero$results
      # Remove collection key if not found
      if (is.null(zotero$collections)) {

        zotero$collection.key <- NULL
        # Return zotero list if not searching all collections
        if (!search.collections) return(zotero)

      }

    } # End find collection based on key

    if (is.null(zotero$collection.key) | any(ancestor, recursive)) {

      # Query all collections in Zotero library
      zotero <- ZoteroGet(
        zotero,
        append.collections = TRUE,
        use.collection = FALSE,
        force = force,
        silent = silent,
        result.type = "collection"
      )

      # Define all collections
      zotero$collections <- zotero$results

      # Search for specific collection key(s) if collection.names is defined
      if (!is.null(zotero$collection.names)) {

        # Look for collections names in Zotero libary
        # Ignore case of collection name if case insensitive is set to TRUE
        ## Thus collection will not be created if Collection exists
        find.path <- FindPath(
          zotero$collections,
          zotero$collection.names,
          case.insensitive
        )

        # Filter out collections with collection.names
        new.collections <- find.path$data |>
          dplyr::filter(key %in% find.path$path & version == 0)

        if (nrow(new.collections)) {

          # Return error if create is FALSE
          if (!create) {

            zotero$log <- LogCat(
              sprintf(
                "Found %s new %s. Please set `create` to TRUE if you want
                to create new collections",
                nrow(new.collections),
                Numerus(nrow(new.collections), "collection", prefix = FALSE)
              ),
              fatal = TRUE,
              silent = silent,
              log = zotero$log
            )

          }

          # Post new collections
          zotero$collections <- new.collections
          zotero <- ZoteroPost(
            zotero,
            post.items = FALSE,
            post.attachments = FALSE,
            silent = silent
          )

          # Update data with version
          zotero$collections <- find.path$data |>
            dplyr::rows_update(new.collections, by = "key")

        } # End new.data

        # Add collections key
        zotero$collection.key <- tail(find.path$path, 1)

      } # End collection.names

    } # End all Zotero collections

    # Find collection path if collection key is defined
    if (!is.null(zotero$collection.key)) {
      # Find all ancestors of current key if ancestor is TRUE
      if (ancestor) {
        # Set collection path
        zotero$collection.path <- AncestorPath(
          zotero$collections,
          zotero$collection.key
        )
        # Else find all descendants of current key if recursive is TRUE
      } else if (recursive) {
        zotero$collection.path <- DescendingPath(
          zotero$collections,
          zotero$collection.key
        )
        # Else set path as collection.key
      } else {
        zotero$collection.path <- zotero$collection.key
      }

      # Define collections according to collection.path
      zotero$collections <- zotero$collections |>
        dplyr::filter(key %in% zotero$collection.path)

    } # Edn collection path

  } # End get collections

  # Fetch items if get.items is TRUE
  if (get.items) {

    if (!is.null(zotero$collection.path)) {
      message <- sprintf(
        "Searching for items using %s",
        Numerus(length(zotero$collection.path), "collection")
      )
    } else {
      message <- "Searching for all items in library"
    }

    # Log message
    zotero$log <-  LogCat(
      message,
      silent = silent,
      log = zotero$log
    )

    for (i in seq_len(max(1, length(zotero$collection.path)))) {

      # Set current collection key in loop
      if (!is.null(zotero$collection.path)) {
        zotero$collection.key <- zotero$collection.path[[i]]
      }

      # Query all items in Zotero library
      zotero <- ZoteroGet(
        zotero,
        append.items = TRUE,
        item.type = item.type,
        limit = limit,
        start = start,
        all.results = all.results,
        max.results = max.results,
        result.type = "item",
        library.type = library.type,
        linkwrap = linkwrap,
        style = style,
        locale = locale,
        force = force,
        silent = silent
      )

      # Add item data to zotero list
      items <- dplyr::bind_rows(items, zotero$results)

    }

    # Restore collection.key
    if (!is.null(zotero$collection.path)) {
      zotero$collection.key <- tail(zotero$collection.path, 1)
    }

    # Add to Zotero list
    if (nrow(items)) zotero$items <- items

  } # End get items

  # Order collections by when they were created
  if (!is.null(zotero$collections)) {
    zotero$collections <- zotero$collections |>
      dplyr::arrange(version) |>
      dplyr::distinct()

    # Set number of collections
    zotero$n.collections <- nrow(zotero$collections)
  }

  # Make certain that items are distinct with attachments at bottom
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

  # Print number of collections and items in library
  if (sum(zotero$n.collections, zotero$n.items, zotero$n.attachments)) {
    zotero$log <- LogCat(
      sprintf(
        "The Zotero list contains: %s, %s, and %s",
        Numerus(zotero$n.collections, "collection"),
        Numerus(zotero$n.items, "item"),
        Numerus(zotero$n.attachments, "attachment")
      ),
      silent = silent,
      log = zotero$log
    )
  }

  return (zotero)
}
