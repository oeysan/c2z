#' @title Use DOI to acquire metadata
#' @description Connects with doi.org to create metadata
#' @param doi A digital object identifier
#' @param meta A list collecting all metadata used to create , Default: list()
#' @param prefer.semantic Prefer metadata from Semantic Scholar, Default: FALSE
#' @param check.retraction Check if marked as retracted, Default: TRUE
#' @param use.json Use either json (TRUE) or XML (FALSE), Default: TRUE
#' @param silent c2z is noisy, tell it to be quiet, Default: TRUE
#' @param log A list for storing log elements, Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see
#' \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Simple use of `ZoteroDoi`
#'   example <- ZoteroDoi("10.1126/sciadv.abd1705")
#'
#'   # Print index using `ZoteroIndex`
#'   if (any(nrow(example$data))) {
#'   ZoteroIndex(example$data) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
#'   }
#' }
#' @seealso
#'  \code{\link[httr]{http_error}}, \code{\link[httr]{GET}},
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[rvest]{rename}}, \code{\link[rvest]{html_attr}},
#'  \code{\link[rvest]{html_text}}, \code{\link[rvest]{reexports}},
#'  \code{\link[rvest]{html_children}}, \code{\link[rvest]{html_name}}
#'  \code{\link[dplyr]{bind}}, \code{\link[dplyr]{arrange}}
#' @rdname ZoteroDoi
#' @export
ZoteroDoi <- \(doi,
               meta = list(),
               prefer.semantic = TRUE,
               check.retraction = TRUE,
               use.json = TRUE,
               silent = TRUE,
               log = list()) {

  # Visible bindings
  key <- data <- log.eta <- NULL

  # Function to check DOI
  TestDoi <- \(doi, meta = list(), use.json) {

    # Check DOI
    doi <- CheckDoi(doi, TRUE)

    # Try DOI key
    httr.get <- Online(
      httr::RETRY(
        "GET",
        sprintf("https://doi.org/api/handles/%s", doi),
        quiet = TRUE
      ),
      silent = TRUE,
      message = "DOI",
      reference = doi,
    )

    # Return data if error
    if (httr.get$error) {
      return (c(httr.get, meta = list(meta)))
    }

    # Format JSON date
    doi.json <- jsonlite::fromJSON(
      ParseUrl(httr.get$data, "text")
    )

    # Set new DOI if alias exist
    doi.alias <- doi.json$values$data[
      doi.json$values$type == "HS_ALIAS","value"
    ][[1]] |>
      CheckDoi(doi.only = TRUE) |>
      GoFish(NULL)
    if (!is.null(doi.alias)) doi <- doi.alias

    # Use JSON if use.json is TRUE
    if (use.json) {
      doi.url <- paste0("https://api.crossref.org/works/", doi)
      # Else use XML
    } else {
      doi.url <- sprintf("https://api.crossref.org/works/%s.xml", doi)
    }

    # Try DOI key
    httr.get <- Online(
      httr::RETRY(
        "GET",
        doi.url,
        quiet = TRUE
      ),
      silent = TRUE,
      message = "CrossRef",
      log = httr.get$log
    )

    # Return data if no error
    if (!httr.get$error) {
      # Set libraryCatalog
      meta$libraryCatalog <- "DOI.org (Crossref)"
      return (c(httr.get, meta = list(meta)))
    }

    # Else query datacite for metadata
    httr.get <- Online(
      httr::RETRY(
        "GET",
        sprintf("https://api.datacite.org/dois/%s", doi),
        quiet = TRUE
      ),
      silent = TRUE,
      message = "DataCite",
      log = httr.get$log
    )

    # Return data if no error
    if (!httr.get$error) {
      # Set libraryCatalog
      meta$libraryCatalog <- "DOI.org (Datacite)"
      return (c(httr.get, meta = list(meta)))
    }

    # Else return error
    return(list(error = TRUE, append(httr.get, "DOI not identified")))

  }

  # Start time for query
  query.start <- Sys.time()

  # Cycle through queries
  ## Should perhaps vectorize, but for loop seems more informative tbh
  for (i in seq_along(doi)) {

    # Check DOI
    check <- TestDoi(doi[[i]], use.json = use.json)

    # Skip if error
    if (check$error) {
      log <- append(log, check$log)
      next
    }

    # Run as crossref
    if (check$meta$libraryCatalog == "DOI.org (Crossref)") {
      metadata <- DoiCrossref(check$data, check$meta, use.json, silent, check$log)
      # Else run as datacite
    } else if (check$meta$libraryCatalog == "DOI.org (Datacite)") {
      metadata <- DoiDatacite(check$data, check$meta, silent, check$log)
    }

    # Set meta
    meta <- metadata$data

    # Set abstractNote to string
    meta$abstractNote <- ToString(GoFish(meta$abstractNote,""),"\n")

    # Set abstractNote from Semantic Scholar if prefer.semantic
    if (prefer.semantic & any(is.na(meta$abstractNote))) {
      semantic <- SemanticScholar(meta$DOI)
      if (any(!is.na(GoFish(semantic$abstract)))) {
        meta$abstractNote <- semantic$abstract
      }
    }

    # Clean Abstract
    meta$abstractNote <- CleanText(meta$abstractNote)

    # Check if retracted if check.retraction
    if (check.retraction) {
      if (GoFish(CrossrefRetracted(doi[[i]]), FALSE)) {
        meta$title <- paste("[RETRACTED]", meta$title)
      }
    }

    # Set accessDate
    meta$accessDate <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%S%z")

    # Create zotero-type matrix
    meta <- GoFish(ZoteroFormat(meta), NULL)

    # Remove if no Creator is found
    if (all(is.na(GoFish(meta$creators[[1]])))) meta <- NULL

    # Append data
    data <- AddAppend(meta, data)

    # Add to log
    log <- append(log, metadata$log)

    # Estimate time of arrival
    log.eta <- LogCat(
      Eta(query.start, i, length(doi)),
      silent = silent,
      flush = TRUE,
      append.log = FALSE
    )
  }
  # Add to log
  log <- append(log, log.eta)

  return (list(data = data, log = log))

}

################################################################################
################################################################################
################################Helper Functions################################
################################################################################
################################################################################

#' Extract DOI from a URL or String
#'
#' This function checks if the input \code{url} contains a valid Digital Object
#' Identifier (DOI) using a regular expression.
#' If a valid DOI is found, it extracts the DOI and optionally returns either
#' the DOI string or the full DOI URL.
#'
#' @param url A character string representing a URL or a DOI-containing text.
#' @param doi.only Logical; if \code{TRUE}, the function returns only the DOI.
#' If \code{FALSE} (default), the function
#'   returns the full DOI URL (i.e., \code{"https://doi.org/"} concatenated with
#'    the DOI).
#'
#' @return Returns a character string with the DOI (or full DOI URL) if found;
#' otherwise, returns \code{NULL}.
#'
#' @details The function uses a regular expression to search for a DOI in the
#'  input. It leverages an external function
#'   \code{GoFish} (with \code{type = FALSE}) to perform additional checks.
#'   The DOI is then trimmed using a function called
#'   \code{Trim}. The regular expression pattern is designed to match the
#'   standard DOI format.
#'
#' @examples
#' \dontrun{
#'   # Example 1: Extract full DOI URL from a string containing a DOI
#'   doi_url <- CheckDoi("https://doi.org/10.1000/xyz123")
#'   # doi_url will be "https://doi.org/10.1000/xyz123"
#'
#'   # Example 2: Extract only the DOI without the URL prefix
#'   doi_only <- CheckDoi("10.1000/xyz123", doi.only = TRUE)
#'   # doi_only will be "10.1000/xyz123"
#' }
#'
#' @export


################################################################################
################################################################################
################################Helper Functions################################
################################################################################
################################################################################


CheckDoi <- function(url, doi.only = FALSE) {

  check <- grepl("10\\.\\d{4,9}/[-._;()/:A-Z0-9]+", url, ignore.case = TRUE) |>
    GoFish(type = FALSE)

  if (!any(check)) return(NULL)

  doi <- sub(
    ".*?(10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+).*", "\\1",
    url[check][1],
    perl = TRUE
  )
  if (!doi.only) doi <- paste0("https://doi.org/", Trim(doi))
  return(doi)
}

