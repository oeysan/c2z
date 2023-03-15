#' @title Search Cristin API
#' @description Use search parameters for the Cristin API and return the results
#'  as a tibble. See \code{\link{CristinWrangler}}
#' @param id The Cristin id of the result, Default: NULL
#' @param doi DOI name (e.g. doi=10.1000/123456), Default: NULL
#' @param title The title of the result, Default: NULL
#' @param contributor Author's name or Cristin person id, Default: NULL
#' @param issn The issn of the result, Default: NULL
#' @param unit Id (e.g. unit=185.53.18.10), Default: NULL
#' @param institution Id (one number, e.g. institution=185), name or acronym of the institution the contributors belong to, Default: NULL
#' @param user A person's username in Cristin together with the institution id separated by colon (e.g., `askeladd:185`), Default: NULL
#' @param category Result \href{https://api.cristin.no/v2/doc/index.html#GETResultCategories}{category code}, Default: NULL
#' @param published_since Results published since and inclusive the given year, (yyyy), Default: NULL
#' @param published_before Results published before and inclusive the given year, (yyyy), Default: NULL
#' @param created_since Results created since and inclusive the given date, (yyyy-mm-dd), Default: NULL
#' @param created_before Results created before and inclusive the given date, (yyyy-mm-dd), Default: NULL
#' @param modified_since Results modified since and inclusive the given date, (yyyy-mm-dd), Default: NULL
#' @param modified_before Results modified before and inclusive the given date, (yyyy-mm-dd), Default: NULL
#' @param year_reported The year a result was reported, Default: NULL
#' @param project_code Project code is the internal reference number used by funding source, Default: NULL
#' @param funding_source Funding source code e.g: NFR, Default: NULL
#' @param funding Funding source code e.g: NFR, and project_code together separated by colon (e.g., NFR:1234), Default: NULL
#' @param lang Two letter \href{https://api.cristin.no/v2/doc/index.html#lang}{language code}, Default: NULL
#' @param page Page number. See \href{https://api.cristin.no/v2/doc/index.html#pagination}{pagination}, Default: 1
#' @param per_page Number of items per page (1000 is max). See \href{https://api.cristin.no/v2/doc/index.html#pagination}{pagination}, Default: 1000
#' @param max.results Do you need a limit?, Default: NULL
#' @param no.results Do you need only the number of results?, Default: FALSE
#' @param sort Sorts on `category` and/or `year_published`. See \href{https://api.cristin.no/v2/doc/index.html#search}{search and sort}. Default sort order is on `cristin_result_id` in ascending order, Default: NULL
#' @param filter Vector of cateogries to include in results. See \href{https://api.cristin.no/v2/doc/index.html#GETResultCategories}{category code}, Default: NULL
#' @param fields `fields = all` gives a list of result objects with all available fields. If this parameter is omitted, a list of result summaries with fewer fields will be returned, Default: all
#' @param open.query Define your own query terms, Default: NULL
#' @param all.results Find all results in query, Default: TRUE
#' @param force Force is seldom wise, but sometimes..., Default: FALSE
#' @param debug Let you test the Cristin API for errors, Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @param base.url The base url for the Cristin API, Default: https://api.cristin.no/v2/results
#' @param custom.url Define your own Cristin API url, Default: NULL
#' @param zotero A list with information on the specified Zotero library (e.g.,
#' id, API key, collections, and items), Default: NULL
#' @param zotero.import Use \code{\link{CristinWrangler}} to wrangle metadata
#' into an acceptable format for Zotero, Default: TRUE
#' @param zotero.check Check for Cristin references already stored in Zotero, Default: TRUE
#' @param use.identifiers Use if ISBN/DOI identifiers if enabled, Default: TRUE
#' @param crossref.search Query Crossref database based on title, authors,
#'  and date if enabled, Default: FALSE
#' @param autosearch Results could be automatically evaluated (based on some
#' logic) or you could inspect them manually if set to FALSE, Default: TRUE
#' @param remove.na Cristin contains many, more or less, obscure categories,
#' and not all are (yet) supported. By default these are removed, however, if
#' this option is set to FALSE unsupported categories are treated according to
#' replace.na, Default: TRUE
#' @param replace.na May the odds be in your favor and replace unsupported
#' categories with a predefined itemType if remove.na is set to
#' false, Default: 'book'
#' @param force.type Force all items to a predefined itemType, Default: NULL
#' @param override Put your faith in the algorithms and the identifiers
#' (i.e., DOI/ISNB) and override what is reported in Cristin, Default: FALSE
#' @param polite Please store you email in `.Renviron` to query Crossref, Default: TRUE
#' @param log A list for storing log elements, Default: list()
#' @return A list with (exported) items from Cristin
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Simple `Cristin` search by id
#'     example <- Cristin(id = "840998")
#'     # Use `ZoteroIndex` to print
#'     ZoteroIndex(example$export)$name
#'   }
#' }
#' @seealso
#'  \code{\link[httr]{http_error}}, \code{\link[httr]{GET}},
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[utils]{tail}}, \code{\link[utils]{head}}
#'  \code{\link[dplyr]{slice}}
#' @rdname Cristin
#' @export
Cristin <- function (id  = NULL,
                     doi  = NULL,
                     title  = NULL,
                     contributor  = NULL,
                     issn  = NULL,
                     unit  = NULL,
                     institution  = NULL,
                     user  = NULL,
                     category  = NULL,
                     published_since  = NULL,
                     published_before  = NULL,
                     created_since  = NULL,
                     created_before  = NULL,
                     modified_since  = NULL,
                     modified_before  = NULL,
                     year_reported  = NULL,
                     project_code  = NULL,
                     funding_source  = NULL,
                     funding  = NULL,
                     lang  = NULL,
                     page  = 1,
                     per_page  = 1000,
                     max.results = NULL,
                     no.results = FALSE,
                     sort  = NULL,
                     filter = NULL,
                     fields  = "all",
                     open.query = NULL,
                     all.results = TRUE,
                     force = FALSE,
                     debug = FALSE,
                     silent = FALSE,
                     base.url = "https://api.cristin.no/v2/results",
                     custom.url = NULL,
                     zotero = NULL,
                     zotero.import = TRUE,
                     zotero.check = TRUE,
                     use.identifiers = TRUE,
                     crossref.search = FALSE,
                     autosearch = TRUE,
                     remove.na = TRUE,
                     replace.na = "book",
                     force.type = NULL,
                     override = FALSE,
                     polite = TRUE,
                     log = list()) {


  # Trim arguments
  args <- as.list(environment())
  for (i in 1:length(args)) {
    arg <- if (is.character(args[[i]])) Trim(args[[i]]) else args[[i]]
    assign(names(args[i]), arg)
  }

  # if debug test status of Cristin API
  if (debug) {
    if(httr::http_error(httr::GET(base.url))) {
      stop("Cristin API appears offline", call. = FALSE)
    }
  }

  # Set per_page to Cristin limit (1000) if per_page > 1000
  if (per_page > 1000) per_page <- 1000
  # Set per_page to 1 if no.results is set to TRUE
  if (no.results) per_page <- 1
  # Run if max.results is defined
  if (!is.null(max.results)) {
    # Set limit as max.results if limit is greater than max.results
    if (per_page > max.results) {
      per_page <- max.results
      all.results <- FALSE
      # Else set all.results to TRUE if max.results > limit
    } else if (max.results > per_page & !all.results) {
      max.results <- per_page
    }
  }
  # Set export as NULL
  export <- NULL

  # Create query list
  query.list <- if (!is.null(open.query)) {
    open.query
  } else {
    list(id = id,
         doi = doi,
         title = title,
         contributor = contributor,
         issn = issn,
         unit = unit,
         institution = institution,
         user = user,
         category = category,
         published_since = published_since,
         published_before = published_before,
         created_since = created_since,
         created_before = created_before,
         modified_since = modified_since,
         modified_before = modified_before,
         year_reported = year_reported,
         project_code = project_code,
         funding_source = funding_source,
         funding = funding,
         lang = lang,
         page = page,
         per_page = per_page,
         sort = sort,
         fields = fields)
  }

  # Remove empty elements from query
  query.list <- (query.list[lengths(query.list) != 0])

  # API query
  if (is.null(custom.url)) {
    data <- httr::RETRY("GET", base.url, query = query.list, quiet = TRUE)
  } else {
    data <- httr::RETRY("GET", custom.url, quiet = TRUE)
  }

  # Number of results
  if (!is.null(custom.url) & data$status_code == "200") {
    total.results <- 1
  } else if (data$status_code == "200") {
    total.results <- max(0,as.numeric(data$headers[["x-total-count"]]))
  } else if (data$status_code != 200) {
    log <-  LogCat(
      ErrorCode(data$status_code),
      silent = silent,
      log = log
    )
  }

  # Log number of results
  log <-  LogCat(sprintf("Found %s" ,
                         Pluralis(total.results, "result", "results")),
                 silent = silent,
                 log = log)

  # Return only number of results if no.results is set to TRUE
  if (no.results) {

    results <- total.results

  } else {

    # Find number of pages given query parameters
    pages <- 1:ceiling(total.results / per_page)

    # Stop function if maximum number of pages exceeds total pages
    if (page > max(pages)) {
      log <-  LogCat(sprintf("The current query contains only %s" ,
                             Pluralis(max(pages), "page", "pages")),
                     silent = silent,
                     fatal = TRUE,
                     log = log)
    }

    # Number of results per page
    limits <-  c(
      rep(per_page, length(pages)-1),
      per_page - (per_page * length(pages) - total.results)
    )

    # Set found.results to total.results if total.results > max.results
    max.results <- max(0,min(max.results, total.results))
    # Set max.results to maximum page size if start page == last page
    if (page == max(pages)) {
      max.results <- max(0,min(
        max.results, utils::tail(limits,1)
      ))
    }

    # Run if max.results > limit and all.results set to TRUE
    if (max.results > per_page & all.results) {

      # Find remaining pages given the query parameters
      remaining.pages <- seq(ceiling(max.results / per_page)-1)+page
      # Set remaining pages to the maximum allowed by constraints
      remaining.pages <- remaining.pages[remaining.pages <= max(pages)]

      # Number of remaining pages
      k.remaining.pages <- length(remaining.pages)

      # Remaining results per pages
      limits <- limits[match(remaining.pages, pages)]

      # Calculate maximum results
      max.results <- max(0,min(max.results, per_page + sum(limits)))

      # Results per remaining pages
      limits <- c(
        utils::head(limits, -1), # Remove last page value
        min( # min value of page value and remaining value
          max.results - (per_page * k.remaining.pages), # remaining value
          utils::tail(limits, 1) # Last page value
        )
      )

    }

    # Log number of remaining results if max.results < total.results
    if (max.results < total.results |
        total.results > per_page & !all.results) {
      log <-  LogCat(sprintf("The provided query is limited to %s" ,
                             Pluralis(max.results,
                                      "result", "results")),
                     silent = silent,
                     log = log)
    }

    # Format data
    results <- JsonToTibble(data)

    # Fetch remaining results if max.results > limit
    # and all.results set to TRUE
    if (max.results > per_page & all.results) {

      # Stop function if 10 or more queries are needed and force is FALSE
      if (k.remaining.pages >= 10 & !force) {

        log <- LogCat("10 or more queries to the API is needed to complete the
                    request. Are you sure you want do this? Set force to TRUE",
                      fatal = TRUE, silent = silent, log = log
        )
      }

      log <-  LogCat(sprintf("Conducting remaining %s" ,
                             Pluralis(k.remaining.pages, "query", "queries")),
                     silent = silent,
                     log = log)

      # Start time for query
      query.start <- Sys.time()

      # Cycle through remaining queries
      ## Should perhaps vectorize, but for loop seems more informative tbh
      for (i in 1:k.remaining.pages) {

        # Change start
        query.list$page = remaining.pages[i]

        # Fetch remaining data
        data <- httr::RETRY("GET", base.url, query = query.list, quiet = TRUE)

        # Append data to list
        results <- AddAppend(JsonToTibble(data), results)

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
      log <- append(log,log.eta)
    }

    # Remove rows not wanted from results
    results <- results |>
      dplyr::slice_head(n = max.results)

    # Filter ot results if filter is defined
    if (!is.null(filter)) {
      filter <- results$category$code %in% filter
      results <- results[filter, ]
      # Message
      message <- sprintf(
        "Filtered out %s",
        Pluralis(sum(!filter), "reference", "references")
      )
      # Log message
      log <-  LogCat(message, silent = silent, log = log)
    }

    # Wrangle results to Zotero format if zotero.import is set to TRUE
    if (zotero.import & max(0,nrow(results))>0) {
      export <- CristinWrangler(
        data = results,
        zotero.check = zotero.check,
        zotero = zotero,
        use.identifiers = use.identifiers,
        crossref.search = crossref.search,
        autosearch = autosearch,
        remove.na = remove.na,
        replace.na = replace.na,
        force.type = force.type,
        override = override,
        silent = silent,
        polite = polite,
        log = log
      )
      # Add to log
      log <- export$log
    }

  }

  # Return list
  cristin <- list(
    data.cache = data,
    results = results,
    export = export$results,
    unsupported.id = export$unsupported.id,
    removed.id = export$removed.id,
    log = log
  )

  return (cristin)

}
