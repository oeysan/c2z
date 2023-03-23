#' @title Connect to Zotero API
#' @description Wrapper to connect with the Zotero API and the main functions of
#'   *c2z*
#' @param collection.names Vector of collection names to create or search for,
#'   Default: NULL
#' @param collection.key A specified collection key, Default: NULL
#' @param collection.path Vector of nested collection keys, Default: NULL
#' @param item.key A specified item key, Default: NULL
#' @param library Use `ZoteroLibrary` to fetch collections and items, Default:
#'   FALSE
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
#' @param all.results Find all results in query, Default: TRUE
#' @param items Predefined metadata (as tibble), Default: NULL
#' @param doi Use \code{\link{ZoteroDoi}} to fetch DOI metadata, Default: NULL
#' @param isbn Use \code{\link{ZoteroIsbn}} to fetch ISBN metadata, Default:
#'   NULL
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @param export Use `ZoteroExport` to export items, Default: FALSE
#' @param csl.type Specify a CSL type to Official repository for Citation Style
#'   Language (CSL), Default: NULL
#' @param csl.name Name of saved CSL file, Default: 'style'
#' @param format Export format of Zotero items, Default: 'biblatex'
#' @param save.data Save data (e.g., bibliography) to disk, Default: FALSE
#' @param save.path Location to store data on disk, Default: NULL
#' @param bib.name Name of exported bibliography, Default: 'references'
#' @param include.bib Include HTML-formatted bibliography from Zotero, Default:
#'   FALSE
#' @param style Citation style to use for appended bibliography and/or
#'   citations, Default: apa
#' @param locale Desired language format of bibliography, Default: 'en-US'
#' @param copy Use `ZoteroCopy` to delete collections and/or items, Default:
#'   FALSE
#' @param copy.collections Try to copy specified collections, Default: TRUE
#' @param copy.items Try to copy specified items?, Default: TRUE
#' @param copy.extras Try to copy specified extras (i.e., attachments and
#'   notes)?, Default: TRUE
#' @param remove.missing Deleted missing extras, Default: TRUE
#' @param change.library Stage changing of library (e.g., from a group to a
#'   personal library), Default: FALSE
#' @param copy.user New user type (The functions will use `group` as prefix if
#'   FALSE), Default: TRUE
#' @param copy.id New id, Default: NULL
#' @param copy.api New API key. Set API to `NA` if key is not needed, Default:
#'   NULL
#' @param post Use `ZoteroPost` to post collections and/or items, Default: FALSE
#' @param post.collections Try to copy specified collections, Default: TRUE
#' @param post.items Try to copy specified items?, Default: TRUE
#' @param post.attachments Try to copy specified extras (i.e., attachments and
#'   notes)?, Default: TRUE
#' @param post.limit Number of collections/items to post per request (max 50),
#'   Default: 50
#' @param delete Use `ZoteroDelete` to delete collections and/or items, Default:
#'   FALSE
#' @param delete.collections Try to delete specified collections, Default: TRUE
#' @param delete.items Try to delete specified items?, Default: TRUE
#' @param delete.limit Number of collections/items to delete per request (max
#'   50), Default: 50
#' @param ragnarok Delete EVERYTHING in the specified library, Default: FALSE
#' @param user User type (The functions will use `group` as prefix if FALSE),
#'   Default: TRUE
#' @param index Create an index of items, Default: FALSE
#' @param id User or group ID, Default: NULL
#' @param api API key to connect with the Zotero library. Set API to `NA` if key
#'   is not needed. See
#'   \href{https://oeysan.github.io/c2z/articles/zotero_api.html}{Zotero API},
#'   Default: NULL
#' @param force Force is seldom wise, but sometimes..., Default: FALSE
#' @param base.url Base url of the Zotero API, Default: 'https://api.zotero.org'
#' @param debug Let you test the Zotero API for errors, Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @param zotero A list with information on the specified Zotero library (e.g.,
#'   id, API key, collections, and items), Default: NULL
#' @param log A list for storing log elements, Default: list()
#' @return A list with information on the specified Zotero library (e.g., id,
#'   API key, collections, and items)
#' @details Please see
#' \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' # Create the default Zotero list
#' example <- Zotero(id = "9913421", api = "RqlAmlH5l1KPghfCseAq1sQ1")
#' # Print the interesting pars of an otherwise empty list
#' print(tail(example,5))
#' @seealso
#'  \code{\link[httr]{http_error}}, \code{\link[httr]{GET}}
#' @rdname Zotero
#' @export
Zotero <- \(collection.names = NULL,
            collection.key = NULL,
            collection.path = NULL,
            item.key = NULL,
            library = FALSE,
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
            items = NULL,
            doi = NULL,
            isbn = NULL,
            export = FALSE,
            csl.type = NULL,
            csl.name = "style",
            format = "biblatex",
            save.data = FALSE,
            save.path = NULL,
            bib.name = "references",
            include.bib = FALSE,
            style = "apa",
            locale = "en-US",
            copy = FALSE,
            copy.collections = TRUE,
            copy.items = TRUE,
            copy.extras = TRUE,
            remove.missing = TRUE,
            change.library = FALSE,
            copy.user = TRUE,
            copy.id = NULL,
            copy.api = NULL,
            post = FALSE,
            post.collections = TRUE,
            post.items = TRUE,
            post.attachments = TRUE,
            post.limit = 50,
            delete = FALSE,
            delete.collections = FALSE,
            delete.items = FALSE,
            delete.limit = 50,
            ragnarok = FALSE,
            user = TRUE,
            index = FALSE,
            id = NULL,
            api = NULL,
            force = FALSE,
            base.url = "https://api.zotero.org",
            debug = FALSE,
            silent = FALSE,
            zotero = NULL,
            log = list()) {

  # Visible bindings
  key <- NULL

  # Create work list if zotero is not defined
  if (is.null(zotero)) {
    zotero <- list(
      collection.names = collection.names,
      collection.key = collection.key,
      collection.path = collection.path,
      item.key = item.key,
      n.collections = 0,
      n.items = 0,
      n.attachments = 0,
      data.cache = NULL,
      results = NULL,
      collections = NULL,
      items = NULL,
      attachments = NULL,
      export = NULL,
      bibliography = NULL,
      locale = locale,
      index = NULL,
      log = log
    )
  } else {
    zotero$collection.names <- collection.names
    zotero$collection.key <- collection.key
    zotero$collection.path <- collection.path
    zotero$item.key <- item.key
  }

  # Determine whether to use user or group id
  if (is.null(zotero$user)) zotero$user <- user
  if (zotero$user) {
    zotero$id <- if (is.null(id)) Sys.getenv("ZOTERO_USER") else id
    if (is.null(zotero$id)) {
      zotero$log <- LogCat(
        "User id is empty. Have du set Sys.getenv(\"ZOTERO_USER\")?
        Please specify id",
        fatal = TRUE,
        log = zotero$log
      )
    }
    user.type <- "users"
  } else {
    zotero$id <- if (is.null(id)) Sys.getenv("ZOTERO_GROUP") else id
    if (is.null(zotero$id)) {
      zotero$log <- LogCat(
        "Group id is empty. Have du set  Sys.getenv(\"ZOTERO_GROUP\")?
        Please specify id",
        fatal = TRUE,
        log = zotero$log
      )
    }
    user.type <- "groups"
  }

  # Set prefix
  zotero$prefix <- paste0(user.type, "/", zotero$id)

  # Define API key and check if API is missing or needed
  if (all(!is.na(api))) {
    zotero$api <- if (is.null(api)) Sys.getenv("ZOTERO_API") else api
    if (is.null(zotero$api)) {
      zotero$log <- LogCat(
        "API is missing. Have du set sys.getenv(\"ZOTERO_API\")?
        Please specify API",
        fatal = TRUE,
        log = zotero$log
      )
    }
  } else {
    zotero$api <- NULL
  }

  # Define primary zotero url
  zotero$url <- sprintf("%s/%s/", base.url, zotero$prefix)

  if (debug) {

    # Check Zotero API
    zotero$log <- LogCat(
      message = "Zotero API",
      error = httr::http_error(httr::GET(base.url)),
      debug = TRUE,
      silent = silent,
      log = zotero$log
    )


    # Check primary url
    zotero$log <- LogCat(
      message = sprintf("%s id '%s'", user.type, zotero$id),
      error = httr::GET(ZoteroUrl(zotero$url))$status_code == 500,
      debug = TRUE,
      silent = silent,
      log = zotero$log
    )

    # Check API key
    zotero$log <- LogCat(
      message = "The provided API key",
      error = httr::GET(
        ZoteroUrl(zotero$url, api = zotero$api)
      )$status_code == 403,
      silent = silent,
      debug = TRUE,
      log = zotero$log
    )

    # Check collection key
    if (!is.null(collection.key)) {
      zotero$log <- LogCat(
        message = sprintf("Collection key '%s'", collection.key),
        error = httr::GET(
          ZoteroUrl(zotero$url,
                    collection.key = collection.key,
                    api = zotero$api)
        )$status_code == 404,
        silent = silent,
        debug = TRUE,
        log = zotero$log
      )
    }

    # Check item key
    if (!is.null(item.key)) {
      zotero$log <- LogCat(
        message = sprintf("Item key '%s'", item.key),
        error = httr::GET(
          ZoteroUrl(zotero$url,
                    item.key = item.key,
                    api = zotero$api)
        )$status_code == 404,
        silent = silent,
        debug = TRUE,
        log = zotero$log
      )
    }

  }

  # Fetch collections if library is set to TRUE
  if (library) {
    zotero <- ZoteroLibrary(
      zotero,
      case.insensitive,
      ancestor,
      recursive,
      create,
      get.items = FALSE,
      force = force,
      silent = any(include.bib, get.items, silent)
    )
  }
  # Copy data if library is set to TRUE
  if (copy) zotero <- ZoteroCopy(
    zotero,
    copy.collections,
    copy.items,
    copy.extras,
    remove.missing,
    change.library,
    copy.user,
    copy.id,
    copy.api,
    silent
  )
  # Add data if add is set to TRUE
  if (!is.null(items) | !is.null(doi) | !is.null(isbn)) {
    zotero <- ZoteroAdd(
      zotero,
      items,
      doi,
      isbn,
      silent
    )
  }
  # Post data if post is set to TRUE
  if (post) {
    zotero <- ZoteroPost(
      zotero,
      post.collections,
      post.items,
      post.attachments,
      post.limit,
      force,
      silent
    )
    # Remove items and collections
    if (library & any(include.bib, get.items)) {
      zotero$collections <- zotero$items <- NULL
    }
  }
  # Fetch items / bibliography if get.items / include.bib is set to TRUE
  if (library & any(include.bib, get.items)) {
    zotero <- ZoteroLibrary(
      zotero,
      case.insensitive,
      ancestor,
      recursive,
      create,
      limit,
      start,
      get.items,
      item.type,
      all.results,
      max.results,
      result.type,
      include.bib,
      style,
      force,
      silent
    )
  }
  # Export data if export is set to TRUE
  if (export) zotero <- ZoteroExport(
    zotero,
    csl.type,
    csl.name,
    locale,
    format,
    bib.name,
    save.data,
    save.path,
    silent
  )
  # Delete data if delete is set to TRUE
  if (delete) zotero <- ZoteroDelete(
    zotero,
    delete.collections,
    delete.items,
    delete.limit,
    force,
    ragnarok,
    silent
  )

  # Add an index if index is set to TRUE
  if (index & !is.null(zotero$items)) {

    zotero$log <- LogCat(
      "Creating index for items",
      silent = silent,
      log = zotero$log
    )
    zotero$index <- ZoteroIndex(zotero$items)

    # Add bibliography to index if defined
    if (!is.null(zotero$bibliography)) {
      zotero$index <- zotero$index |>
        dplyr::full_join(zotero$bibliography,
                         by = dplyr::join_by(key))
    }

  }

  # Clean up log somewhat...
  log = log[!duplicated(log)]

  # Return Zotero list
  return (zotero)

}
