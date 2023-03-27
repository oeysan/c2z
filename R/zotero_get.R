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
#' @param include.bib Include HTML-formatted bibliography from Zotero, Default:
#'   FALSE
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
#'     id = "4827927",
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
#'   ZoteroIndex(example$results) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
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
               include.bib = FALSE,
               style = "apa",
               locale = "en-US",
               all.results = TRUE,
               max.results = NULL,
               result.type = NULL,
               force = FALSE,
               silent = FALSE) {

  # Visible bindings
  bibliography <- json.data <- include <- zotero.items <- keys <- key <-
    bib <- citation <- NULL

  # Set definitions if include is defined
  if (include.bib) {
    include <- paste("data", "bib", "citation", sep = ",")
    # Set include as NULL if format is anything other than JSON
    if (format != "json") include <- NULL
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
                   append.top,
                   append.file)

  # if limit > zotero max limit
  if (limit > 100) limit <- 100

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
      locale = locale
    )
  }

  # Remove empty elements from query
  query.list <- (query.list[lengths(query.list) != 0])

  # Fetch collection keys if defined
  if (!is.null(collection.keys)) {
    keys <- "collectionKey"
    key.list <- SplitData(collection.keys, limit)
    # Else fetch item keys if defined
  } else if (!is.null(item.keys)) {
    keys <- "itemKey"
    key.list <- SplitData(item.keys, limit)
  }
  # Add keys if defined to query list
  if (!is.null(keys)) query.list[[keys]] <- ToString(key.list[[1]], ",")

  # API query
  if (is.null(custom.url)) {
    json.get <- GoFish(
      httr::RETRY("GET", url, query = query.list, quiet = TRUE),
      stats::setNames(list(404), "status_code")
    )
  } else {
    json.get <- GoFish(
      httr::RETRY("GET", custom.url, quiet = TRUE),
      stats::setNames(list(404), "status_code")
    )
  }

  # Return Zotero list upon error
  if (json.get$status_code != 200) {
    zotero$log <-  LogCat(
      ErrorCode(json.get$status_code),
      silent = silent,
      log = zotero$log
    )
    return (zotero)
  }

  # Number of results
  if (!is.null(keys)) {
    total.results <- do.call(sum,lapply(key.list, nrow))
  } else if (!all(append.collections, append.items, append.top, append.file)) {
    total.results <- 1
  } else {
    total.results <- max(0, as.numeric(json.get$headers[["total-results"]]))
  }

  # Pointless linguistics
  if (is.null(result.type)) result.type <- "result"
  result.types <- paste0(result.type,"s")

  # Log number of results
  zotero$log <-  LogCat(
    sprintf(
      "Found %s",
      Pluralis(
        total.results,
        result.type,
        result.types
      )
    ),
    silent = silent,
    log = zotero$log
  )

  # Set found.results to total.results if total.results > max.results
  max.results <- min(max.results, total.results)

  if (start > max.results) {
    zotero$log <-  LogCat(sprintf("The current query contains %s" ,
                           Pluralis(max(0,(max.results-1)), "page", "pages")),
                   silent = silent,
                   fatal = TRUE,
                   log = zotero$log)
  }

  # Log number of remaining results if max.results < total.results
  if (max.results < total.results | total.results > limit & !all.results) {
    zotero$log <-  LogCat(
      sprintf(
        "The provided query is limited to %s",
        Pluralis(max.results,
                 "result",
                 "results")
      ),
      silent = silent,
      log = zotero$log
    )
  }

  # Parse data if defined
  if (!is.null(format)) {
    # Parse zotero items according to format if not json
    if (format != "json") {
      zotero.items <- ParseUrl(json.get, format)
      # else parse data as JSON
    } else {
      # Full JSON data including bibliography
      json.data <- JsonToTibble(json.get)
      # Zotero metadata
      zotero.items <- json.data$data
    }
  }

  # Format
  results <- ZoteroFormat(zotero.items, format, zotero$prefix)

  # Set bibliography if include is defined
  if (!is.null(include)) {
    bibliography <- json.data |>
      dplyr::select(key, bib, citation)
  }

  # Fetch remaining results if max.results > limit and all.results set to TRUE
  if (max.results > limit & all.results) {

    # Find number of pages given query parameters
    pages <- seq(0, (ceiling(max.results / limit) - 1) * limit, limit)

    # Remaining pages
    remaining.pages <- pages[-1]

    # Number of remaining pages
    k.remaining.pages <- length(remaining.pages)

    # Stop function if 10 or more queries are needed and force is FALSE
    if (k.remaining.pages >= 10 & !force) {

      zotero$log <- LogCat("10 or more queries to the API is needed to complete
                           the request. Are you sure you want do this?
                           Set force to TRUE",
                    fatal = TRUE, silent = silent, log = zotero$log
      )
    }

    zotero$log <-  LogCat(sprintf("Conducting remaining %s" ,
                           Pluralis(k.remaining.pages, "query", "queries")),
                   silent = silent,
                   log = zotero$log)

    # Start time for query
    query.start <- Sys.time()

    # Cycle through remaining queries
    ## Should perhaps vectorize, but for loop seems more informative tbh
    for (i in seq_along(k.remaining.pages)) {

      # Add collection/item keys if defined to query.list
      if (!is.null(keys)) {
        query.list[[keys]] <- ToString(key.list[[i+1]], ",")
      } else {
        # Change start
        query.list$start <- remaining.pages[i]
      }

      # Fetch remaining data
      json.get <- httr::RETRY("GET", url, query = query.list, quiet = TRUE)

      # Parse data if defined
      # Parse zotero items according to format if not json
      if (format != "json") {
        zotero.items <- ParseUrl(json.get, format)
        # else parse data as JSON
      } else {
        # Full JSON data including bibliography
        json.data <- JsonToTibble(json.get)
        # Zotero metadata
        zotero.items <- json.data$data
      }

      # Append data to list
      results <- AddAppend(
        ZoteroFormat(zotero.items, format, zotero$prefix),
        results,
        sep = "\n"
      )

      # Append bibliography if include is defined
      if (!is.null(include)) {
        bibliography <- AddAppend(
          bibliography,
          json.data |>
            dplyr::select(key, bib, citation)
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
    zotero$log <- append(zotero$log,log.eta)
  }

  # Add temp data to zotero list
  zotero$data.cache <- json.get

  # Add results
  zotero$results <- results

  # Set bibliography if include is defined
  if (!is.null(include)) {
    zotero$bibliography <- bibliography |>
      dplyr::mutate(
        # Find csl-bib-body class
        bib.body = rvest::read_html(toString(bib)) |>
          rvest::html_nodes(".csl-bib-body") |>
          toString() |>
          (\(.) gsub("\\\n.*", "", .))() |>
          (\(.) paste0(., "%s", "</div>"))(),
        # Find csl-entry class
        bib.item = rvest::read_html(toString(bib)) |>
          rvest::html_nodes(".csl-entry") |>
          (\(.) lapply(., toString))() |>
          unlist()
      ) |>
      # arrange by author
      dplyr::arrange(citation)
  }

  # Add zotero version to zotero list
  zotero$version <- json.get$headers$`last-modified-version`

  return (zotero)

}
