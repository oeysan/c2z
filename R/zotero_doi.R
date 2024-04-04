#' @title Use DOI to acquire metadata
#' @description Connects with doi.org to create metadata
#' @param doi A digital object identifier
#' @param meta A list collecting all metadata used to create , Default: list()
#' @param prefer.semantic Prefer metadata from Semantic Scholar, Default: FALSE
#' @param check.retraction Check if marked as retracted, Default: TRUE
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
               prefer.semantic = FALSE,
               check.retraction = TRUE,
               silent = TRUE,
               log = list()) {

  # Visible bindings
  key <- data <- log.eta <- NULL

  # Function to check DOI
  CheckDoi <- \(doi, meta = list()) {

    # Visible bindings
    type <- NULL

    if (!grepl("^.*(10\\..*)", doi)) {
      return (list(error = TRUE, log = sprintf("DOI: `%s` is not valid", doi)))
    }

    # Remove any https part
    doi <- Trim(gsub("^.*(10\\..*)", "\\1", doi, perl = TRUE))
    # Remove any excess white space
    doi <- gsub("\\s", "", doi)

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

    # Check if DOI is an alias
    doi.alias <- GoFish(
      doi.json$values$data[
        doi.json$values$type == "HS_ALIAS","value"
      ][[1]]
    )

    # Set new DOI if alias exist
    if (any(!is.na(doi.alias))) {
      doi <- Trim(
        gsub("^.*(10\\..*)", "\\1", doi.alias[[1]], perl = TRUE)
      )
    }

    # Try DOI key
    httr.get <- Online(
      httr::RETRY(
        "GET",
        sprintf("https://api.crossref.org/works/%s.xml", doi),
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
    check <- CheckDoi(doi[[i]])

    # Skip if error
    if (check$error) {
      log <- append(log, check$log)
      next
    }

    # Run as crossref
    if (check$meta$libraryCatalog == "DOI.org (Crossref)") {
      metadata <- DoiCrossref(check$data, check$meta, silent, check$log)
      # Else run as datacite
    } else if (check$meta$libraryCatalog == "DOI.org (Datacite)") {
      metadata <- DoiDatacite(check$data, check$meta, silent, check$log)
    }

    # Set meta
    meta <- metadata$data

    # Set abstractNote to string
    meta$abstractNote <- ToString(GoFish(meta$abstractNote,""),"\n")

    # Set abstractNote from Semantic Scholar if prefer.semantic
    if (prefer.semantic) {
      semantic <- SemanticScholar(doi[[i]])
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
