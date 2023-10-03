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
#' @param search.collections Search all collections if collection.key fails,
#' Default: TRUE
#' @param get.collections Fetch collections, Default: TRUE
#' @param get.items Fetch items, Default: TRUE
#' @param item.type Items to search for (NULL = everything), Default: NULL
#' @param all.results Find all results in query, Default: TRUE
#' @param max.results Do you need a limit?, Default: NULL
#' @param all.results Find all results in query, Default: TRUE
#' @param collections Predefined collections (as tibble), Default: NULL
#' @param items Predefined metadata (as tibble), Default: NULL
#' @param attachments Predefined attachments (as tibble), Default: NULL
#' @param metadata Predefined metadata in Zoter-format, Default: NULL
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
#' @param library.type Commma-separated data from Zotero (i.e., data, bib,
#' citation), Default: NULL
#' @param linkwrap Set URL (e.g., DOI) as HTML link (1 = yes), Default: 1
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
#' @param token Name of user or group token as defined in `.Renviron`, Default:
#' NULL
#' @param token.api Name of API token as defined in `.Renviron`, Default:
#' NULL
#' @param api API key to connect with the Zotero library. Set API to `NA` if key
#'   is not needed. See
#'   \href{https://oeysan.github.io/c2z/articles/zotero_api.html}{Zotero API},
#'   Default: NULL
#' @param force Force is seldom wise, but sometimes..., Default: FALSE
#' @param base.url Base url of the Zotero API, Default: 'https://api.zotero.org'
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
            search.collections = TRUE,
            get.collections = TRUE,
            get.items = TRUE,
            item.type = NULL,
            all.results = TRUE,
            max.results = NULL,
            collections = NULL,
            items = NULL,
            attachments = NULL,
            metadata = NULL,
            doi = NULL,
            isbn = NULL,
            export = FALSE,
            csl.type = NULL,
            csl.name = "style",
            format = "biblatex",
            save.data = FALSE,
            save.path = NULL,
            bib.name = "references",
            library.type = NULL,
            linkwrap = 1,
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
            token = NULL,
            token.api = NULL,
            api = NULL,
            force = FALSE,
            base.url = "https://api.zotero.org",
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
      collections = collections,
      items = items,
      attachments = attachments,
      n.collections = 0,
      n.items = 0,
      n.attachments = 0,
      data.cache = NULL,
      results = NULL,
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

  # Define tokens
  if (zotero$user) {
    if (is.null(token)) token <- "ZOTERO_USER"
    user.type <- "users"
  } else {
    if (is.null(token)) token <- "ZOTERO_GROUP"
    user.type <- "groups"
  }
  if (is.null(token.api)) token.api <- "ZOTERO_API"

  # Fetch tokens from .Renviron
  token <- Sys.getenv(token)
  token.api <- Sys.getenv(token.api)

  # Check if id is empty
  if (is.null(zotero$id)) {
    zotero$id <- if (is.null(id)) token else id
    if (grepl("^\\s*$", zotero$id)) {
      zotero$log <- LogCat(
        "Id is empty. Have du defined a token in .Renviron? Please specify
        `id`",
        fatal = TRUE,
        log = zotero$log
      )
    }
  }

  if (is.null(zotero$api)) {
    # Set API as NULL if NA
    if (any(is.na(api))) {
      zotero$api <- NULL
      # Define API key
    } else {
      zotero$api <- if (is.null(api)) token.api else api
      if (grepl("^\\s*$", zotero$api)) {
        zotero$log <- LogCat(
          "API key is empty. Have du defined a token in .Renviron? Please
          specify `api`",
          fatal = TRUE,
          log = zotero$log
        )
      }
    }
  }

  # Set prefix
  zotero$prefix <- paste0(user.type, "/", zotero$id)

  # Define primary zotero url
  zotero$url <- sprintf("%s/%s/", base.url, zotero$prefix)

  # Fetch collections if library is set to TRUE
  if (library & get.collections) {
    zotero <- ZoteroLibrary(
      zotero,
      case.insensitive,
      ancestor,
      recursive,
      create,
      search.collections = search.collections,
      get.items = FALSE,
      force = force,
      silent = silent
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
  if (!is.null(metadata) | !is.null(doi) | !is.null(isbn)) {
    zotero <- ZoteroAdd(
      zotero,
      metadata,
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
  }
  # Fetch items / bibliography
  if (library & any(!is.null(library.type), get.items)) {
    zotero <- ZoteroLibrary(
      zotero,
      case.insensitive,
      ancestor,
      recursive,
      create,
      limit,
      start,
      search.collections,
      get.collections = FALSE,
      get.items,
      item.type,
      all.results,
      max.results,
      library.type,
      linkwrap,
      style,
      locale,
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
