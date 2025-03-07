#' @title Get collections and items from a Zotero library
#' @description Get
#' @param zotero A list with information on the specified Zotero library (e.g.,
#'   id, API key, collections, and items)
#' @param use.collection Use collection key if present in Zotero list, Default:
#'   TRUE
#' @param use.item Use item key if present in Zotero list, Default: FALSE
#' @param append.collections Append `/collections/` to Zotero API url, Default:
#'   FALSE
#' @param append.items Append `/items/` to Zotero API url, Default: FALSE
#' @param append.top Append `/top/` to Zotero API url, Default: FALSE
#' @param append.file Append `/file/` to Zotero API url, Default: FALSE
#' @param custom.url Use a custom Zotero API url, Default: NULL
#' @param open.query Use your own query, Default: NULL
#' @param limit Number of results per query (max 100), Default: 100
#' @param start Starting position of query (0 = first result), Default: 0
#' @param format Format of response from the Zotero API, Default: 'json'
#' @param item.keys Specified vector of items keys, Default: NULL
#' @param collection.keys Specified vector of collection keys, Default: NULL
#' @param item.type Items to search for (NULL = everything), Default: NULL
#' @param library.type Commma-separated data from Zotero (i.e., data, bib,
#' citation), Default: NULL
#' @param linkwrap Set URL (e.g., DOI) as HTML link (1 = yes), Default: 1
#' @param style Citation style to use for appended bibliography and/or
#'   citations, Default: apa
#' @param locale Desired language format of bibliography, Default: 'en-US'
#' @param all.results Find all results in query, Default: TRUE
#' @param max.results Do you need a limit?, Default: NULL
#' @param result.type Pointless linguistics to display result type (default =
#'   `result`), Default: NULL
#' @param force Force is seldom wise, but sometimes..., Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return A list with information on the specified Zotero library (e.g.,
#'   collections and items)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Define Zotero list according to default setings
#'   zotero = Zotero(
#'     user = FALSE,
#'     id = "5250382",
#'     api = "RqlAmlH5l1KPghfCseAq1sQ1"
#'   )
#'
#'   # Query default group Zotero library for 1 item
#'   example <- ZoteroGet(
#'     zotero,
#'     max.results = 1
#'   )
#'
#'   # Print index using `ZoteroIndex`
#'   if (any(nrow(example$results))) {
#'   ZoteroIndex(example$results) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
#'   }
#' }
#' @seealso
#'  \code{\link[httr]{RETRY}}
#' @rdname ZoteroGet
#' @export
ZoteroGet <- \(zotero,
               use.collection = TRUE,
               use.item = FALSE,
               append.collections = FALSE,
               append.items = FALSE,
               append.top = FALSE,
               append.file = FALSE,
               custom.url = NULL,
               open.query = NULL,
               limit = 100,
               start = 0,
               format = "json",
               item.keys = NULL,
               collection.keys = NULL,
               item.type = NULL,
               library.type = NULL,
               linkwrap = 1,
               style = "apa",
               locale = "en-US",
               all.results = TRUE,
               max.results = NULL,
               result.type = NULL,
               force = FALSE,
               silent = FALSE) {

  # Visible bindings
  bibliography <- json.data <- include <- zotero.items <- keys <- key <-
    key.type <- valid.keys <- bib <- citation <- log.eta <- any_of <-
    bib.item <- NULL

  # Set definitions if include is defined
  if (format == "json") {
    include <- library.type
  } else {
    style <- locale <- linkwrap <-  NULL
  }

  # Set no limit if format is versions or keys
  if (format %in% c("versions", "keys")) {
    limit <- NULL
  # Else if include contains bib or citation set to 25
  } else if (!is.null(include) &&
             grepl("bib|citation", include, ignore.case = TRUE)) {
    limit <- 25
    # else if limit > zotero max limit
  } else if (limit > 100) {
    limit <- 100
  }

  # Run if max.results is defined
  if (!is.null(max.results)) {
    # Set limit as max.results if limit is greater than max.results
    if (limit > max.results) {
      limit <- max.results
      all.results <- FALSE
      # Else set all.results to TRUE if max.results > limit
    } else if (max.results > limit & !all.results) {
      max.results <- limit
    }
  } else if (!all.results) {
    max.results <- min(limit, max.results)

  }

  # Create query list
  query.list <- if (!is.null(open.query)) {
    open.query
  } else {
    list(
      limit = limit,
      start = start,
      format = format,
      itemType = item.type,
      include = include,
      style = style,
      locale = locale,
      linkwrap = linkwrap
    )
  }

  # Remove empty elements from query
  query.list <- (query.list[lengths(query.list) != 0])

  # Fetch collection keys if defined
  if (!is.null(collection.keys)) {
    key.type <- "collectionKey"
    keys <- collection.keys
    append.collections <- TRUE
    # Else fetch item keys if defined
  } else if (!is.null(item.keys)) {
    key.type <- "itemKey"
    keys <- item.keys
    append.items <- TRUE
  }

  # Add collection/item keys if defined to query.list
  if (!is.null(key.type)) {

    # Query all keys
    httr.get <- Online(
      httr::RETRY("GET",
                  ZoteroUrl(
                    zotero$url,
                    api = zotero$api,
                    append.collections = append.collections),
                  query = list(format = "keys"), quiet = TRUE),
      silent = TRUE
    )

    # Log and return error if status code != 200
    if (httr.get$error) {
      return (zotero)
    }

    # check valid keys
    valid.keys <- httr.get$data |>
      ParseUrl() |>
      (\(x) tibble::tibble(key = unlist(strsplit(x, "\n"))))() |>
      dplyr::filter(key %in% keys)

    # Add keys if defined to query list
    if (any(nrow(valid.keys))) {

      # Create key list according to limit
      key.list <- SplitData(valid.keys, limit)

      query.list[[key.type]] <- ToString(key.list[[1]], ",")

    }

  }

  # Define url
  url <- ZoteroUrl(zotero$url,
                   zotero$collection.key,
                   use.collection,
                   zotero$item.key,
                   use.item,
                   zotero$api,
                   append.collections,
                   append.items,
                   append.file,
                   append.top)

  # API query
  if (is.null(custom.url)) {
    httr.get <- Online(
      httr::RETRY("GET", url, query = query.list, quiet = TRUE),
      silent = TRUE
    )
  } else {
    httr.get <- Online(
      httr::RETRY("GET", custom.url, quiet = TRUE),
      silent = TRUE
    )
  }
  zotero$log <- append(zotero$log, httr.get$log)

  # Return zotero list if status code != 200
  if (httr.get$error) {
    # Log number of results
    zotero$log <-  LogCat(
      "Resource not found",
      silent = silent,
      log = zotero$log
    )
    zotero$results <- NULL
    return (zotero)
  }

  # Number of results
  total.results <- as.numeric(httr.get$data$headers[["total-results"]])

  if (!is.null(key.type)) {
    total.results <- max(0, nrow(valid.keys))
  } else if (!any(length(total.results))) {
    total.results <- 1
  } else {
    total.results <- max(0, total.results)
  }

  # Pointless linguistics
  if (is.null(result.type)) result.type <- "result"
  result.type[2] <- if (length(result.type == 1)) paste0(result.type,"s")

  # Log number of results
  zotero$log <-  LogCat(
    sprintf(
      "Found %s",
      Numerus(total.results, result.type[1], result.type[2])
    ),
    silent = silent,
    log = zotero$log
  )

  # Parse data if defined
  if (!is.null(format)) {
    # Parse zotero items according to format if not json
    if (format != "json") {
      zotero.items <- ParseUrl(httr.get$data, format)
      # else parse data as JSON
    } else {
      # Full JSON data including bibliography
      json.data <- JsonToTibble(httr.get$data)
      # Zotero metadata
      if ("data" %in% names(json.data)) zotero.items <- json.data$data
    }
  }

  # Format
  results <- ZoteroFormat(zotero.items, format, zotero$prefix)

  # Set bibliography if include is defined
  if (!is.null(include) & !is.null(json.data)) {
    bibliography <- json.data |>
      dplyr::select(any_of(c("key", "version", "bib", "citation")))
  }

  # Set found.results to total.results if total.results > max.results
  max.results <- min(max.results, total.results)

  if (start > max.results) {
    zotero$log <-  LogCat(
      sprintf(
        "The current query contains %s" ,
        Numerus(max(0,(max.results-1)), "page")
      ),
      silent = silent,
      fatal = TRUE,
      log = zotero$log
    )
  }

  # Log number of remaining results if max.results < total.results
  if (max.results < total.results | total.results > max(0, limit) & !all.results) {
    zotero$log <-  LogCat(
      sprintf(
        "The provided query is limited to %s",
        Numerus(max.results, "result")
      ),
      silent = silent,
      log = zotero$log
    )
  }

  # Fetch remaining results if max.results > limit and all.results set to TRUE
  if (max.results > max(0, limit) & all.results & !is.null(limit)) {

    # Find number of pages given query parameters
    pages <- seq(0, (ceiling(max.results / limit) - 1) * limit, limit)

    # Remaining pages
    remaining.pages <- pages[-1]

    # Number of remaining pages
    k.remaining.pages <- length(remaining.pages)

    # Stop function if 10 or more queries are needed and force is FALSE
    if (k.remaining.pages >= 10 & !force) {

      zotero$log <- LogCat(
        "10 or more queries to the API is needed to complete the request.
        Are you sure you want do this? Set force to TRUE",
        fatal = TRUE,
        silent = silent,
        log = zotero$log
      )
    }

    zotero$log <-  LogCat(
      sprintf(
        "Need %s addtional %s to gather %s" ,
        k.remaining.pages,
        Numerus(k.remaining.pages, "query", "queries", FALSE),
        result.type[2]
      ),
      silent = silent,
      log = zotero$log
    )

    # Start time for query
    query.start <- Sys.time()

    # Cycle through remaining queries
    ## Should perhaps vectorize, but for loop seems more informative tbh
    for (i in seq_len(k.remaining.pages)) {

      # Add collection/item keys if defined to query.list
      if (!is.null(key.type)) {
        query.list[[key.type]] <- ToString(key.list[[i+1]], ",")
      } else {
        # Change start
        query.list$start <- remaining.pages[i]
      }

      # Fetch remaining data
      httr.get <- Online(
        httr::RETRY("GET", url, query = query.list, quiet = TRUE),
        silent = TRUE
      )
      zotero$log <- append(zotero$log, httr.get$log)

      # Log and skip skip iteration upon error
      if (httr.get$error) {
        next
      }

      # Parse data if defined
      # Parse zotero items according to format if not json
      if (format != "json") {
        zotero.items <- ParseUrl(httr.get$data, format)
        # else parse data as JSON
      } else {
        # Full JSON data including bibliography
        json.data <- JsonToTibble(httr.get$data)
        # Zotero metadata
        if ("data" %in% names(json.data)) zotero.items <- json.data$data
      }

      # Append data to list
      results <- AddAppend(
        ZoteroFormat(zotero.items, format, zotero$prefix),
        results,
        sep = "\n"
      )

      # Append bibliography if include is defined
      if (!is.null(include) & !is.null(json.data)) {
        bibliography <- AddAppend(
          bibliography,
          json.data |>
            dplyr::select(any_of(c("key", "version", "bib", "citation")))
        )
      }

      # Estimate time of arrival
      log.eta <-
        LogCat(
          Eta(query.start, i, k.remaining.pages),
          silent = silent,
          flush = TRUE,
          log = log,
          append.log = FALSE
        )
    }
    # Add to log
    zotero$log <- append(zotero$log, log.eta)
  }

  # Add temp data to zotero list
  zotero$data.cache <- httr.get$data

  # Add results
  zotero$results <- results

  # Set bibliography if include is defined
  if (any(nrow(bibliography))) {
    zotero$bibliography <- bibliography |>
      dplyr::mutate(
        # Find csl-bib-body class
        bib.body = rvest::read_html(toString(bib)) |>
          rvest::html_nodes(".csl-bib-body") |>
          toString() |>
          (\(x) gsub("\\\n.*", "", x))() |>
          (\(x) paste0(x, "%s", "</div>"))(),
        # Find csl-entry class
        bib.item = rvest::read_html(toString(bib)) |>
          rvest::html_nodes(".csl-entry") |>
          (\(x) lapply(x, toString))() |>
          unlist()
      ) |>
      # arrange by author
      dplyr::arrange(bib.item) |>
      GoFish()
  }

  # Add zotero version to zotero list
  zotero$version <- httr.get$data$headers$`last-modified-version`

  return (zotero)

}
