#' @title Get collections and items from a Zotero library
#' @description Get
#' @param zotero A list with information on the specified Zotero library (e.g.,
#' id, API key, collections, and items)
#' @param use.collection Use collection key if present in Zotero list, Default: TRUE
#' @param use.item Use item key if present in Zotero list, Default: FALSE
#' @param append.collections Append `/collections/` to Zotero API url, Default: FALSE
#' @param append.items Append `/items/` to Zotero API url, Default: FALSE
#' @param append.top Append `/top/` to Zotero API url, Default: FALSE
#' @param append.file Append `/file/` to Zotero API url, Default: FALSE
#' @param custom.url Use a custrom Zotero API url, Default: NULL
#' @param open.query Use your own query, Default: NULL
#' @param limit Number of results per query (max 100), Default: 100
#' @param start Starting position of query (0 = first result), Default: 0
#' @param format Format of reponse from the Zotero API, Default: 'json'
#' @param item.keys Specified vector of items keys, Default: NULL
#' @param collection.keys Specified vector of collection keys, Default: NULL
#' @param item.type Items to search for (NULL = everything), Default: NULL
#' @param include Include bibliography (i.e., `bib`) and/or citation (i.e., `citation`), Default: NULL
#' @param style Citation style to use for appended bibliography and/or citations, Default: apa
#' @param locale Desired language format of bibliography, Default: 'en-US'
#' @param all.results Find all results in query, Default: TRUE
#' @param max.results Do you need a limit?, Default: NULL
#' @param force Force is seldom wise, but sometimes..., Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return A list with information on the specified Zotero library (e.g., collections and items)
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
               include = NULL,
               style = "apa",
               locale = "en-US",
               all.results = TRUE,
               max.results = NULL,
               force = FALSE,
               silent = FALSE) {

  # Visible bindings
  citation <- bibliography <- meta <- NULL

  # Set definitions if include is defined
  if (!is.null(include)) {
    include <- paste0("data,", include)
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
      itemKey = item.keys,
      collectionKey = collection.keys,
      include = include,
      style = style,
      locale = locale
    )
  }

  # Remove empty elements from query
  query.list <- (query.list[lengths(query.list) != 0])

  # initial query
  zotero$log <-  LogCat("Conducting initial query",
                 silent = silent,
                 log = zotero$log)

  # API query
  if (is.null(custom.url)) {
    json.get <- httr::RETRY("GET", url, query = query.list, quiet = TRUE)
  } else {
    json.get <- httr::RETRY("GET", custom.url, quiet = TRUE)
  }

  # Number of results
  total.results <- max(0,as.numeric(json.get$headers[["total-results"]]))

  # Log number of results
  zotero$log <-  LogCat(sprintf("Found %s" ,
                         Pluralis(total.results, "result", "results")),
                 silent = silent,
                 log = zotero$log)

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
    zotero$log <-  LogCat(sprintf("The provided query is limited to %s" ,
                           Pluralis(max.results,
                                    "result", "results")),
                   silent = silent,
                   log = zotero$log)
  }

  # Parse data if defined
  if (!is.null(format)) {
    json.data <- ParseUrl(json.get, format)
    # Convert to zotero friendly tibble if JSON data
    if (format == "json" & !is.null(json.data)) {
      json.data <- jsonlite::fromJSON(json.data)$data
    }
  }
  # Format
  results <- ZoteroFormat(json.data, format, zotero$prefix)

  # Set bibliography if include is defined
  if (!is.null(include)) {
    # Format bibliography data
    bib.data <- JsonToTibble(json.get) |> dplyr::arrange(meta$creatorSummary,
                                                     meta$parsedData)
    if (grepl("bib", include)) bibliography <- bib.data$bib
    if (grepl("citation", include)) citation <- bib.data$citation
  }

  # Fetch remaining results if max.results > limit and all.results set to TRUE
  if (max.results > limit & all.results) {

    # Find number of pages given query parameters
    pages <- seq(0, (ceiling(max.results / limit)-1) * limit, limit)

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
    for (i in 1:k.remaining.pages) {

      # Change start
      query.list$start = remaining.pages[i]

      # Fetch remaining data
      json.get <- httr::RETRY("GET", url, query = query.list, quiet = TRUE)

      # Parse data if defined
      if (!is.null(format)) {
        json.data <- ParseUrl(json.get, format)
        # Convert to zotero friendly tibble if JSON data
        if (format == "json" & !is.null(json.data)) {
          json.data <- jsonlite::fromJSON(json.data)$data
        }
      }

      # Append data to list
      results <- AddAppend(
        ZoteroFormat(json.data, format, zotero$prefix), results
      )

      # Set bibliography if include is defined
      if (!is.null(include)) {
        # Fetch and format bibliography data
        bib.data <- JsonToTibble(json.get) |>
          dplyr::arrange(meta$creatorSummary,
                         meta$parsedData)

        # Create bibliography if bib is defined
        if (grepl("bib", include)) {
          bibliography <- AddAppend(bib.data$bib, bibliography)
        }
        # Create citation if citation is defined
        if (grepl("citation", include)) {
          citation <- AddAppend(bib.data$citation, citation)
        }
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

  # Add bibliography
  zotero$bibliography <- bibliography

  # Add citation
  zotero$citation <- citation

  # Add zotero version to zotero list
  zotero$version <- json.get$headers$`last-modified-version`

  return (zotero)

}
