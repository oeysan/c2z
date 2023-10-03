#' @title Wrangle CRAN metadata into Zotero-type format
#' @description Query CRAN by name and fetch metadata
#' @param id name of R package
#' @param meta A list collecting all metadata used to create , Default: list()
#' @param silent c2z is noisy, tell it to be quiet, Default: TRUE
#' @param log A list for storing log elements, Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see
#' \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Search cran for package `dplyr`
#'   example <- ZoteroCran(c("dplyr", "jsonlite", "httr"))
#'
#'   # Print index using `ZoteroIndex`
#'   if (any(nrow(example$data))) {
#'   ZoteroIndex(example$data) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
#'   }
#' }
#' @seealso
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[rvest]{reexports}}
#' @rdname ZoteroCran
#' @export
ZoteroCran <- \(id,
                meta = list(),
                silent = TRUE,
                log = list()) {

  # Visible bindings
  key <- data <- log.eta <- NULL

  Cran <- \(id, meta, silent, log) {

    # Canon url
    url <- sprintf("https://cran.r-project.org/package=%s", id)

    # Query for metadata
    httr.get <- Online(
      httr::RETRY(
        "GET",
        url,
        quiet = TRUE
      ),
      silent = silent,
      message = sprintf("CRAN id: %s", id)
    )
    log <- append(log, httr.get$log)

    # Log and return data if no error
    if (httr.get$error) {
      return (list(log = log))
    }

    # Define data
    data <- httr.get$data |>
      rvest::read_html() |>
      rvest::html_nodes("meta")

    # Set itemType
    meta$itemType <- "computerProgram"
    # Set language
    meta$programmingLanguage <- "R"
    # Fetch title
    meta$title <- ReadAttr(
      data, "//meta[@name='og:title']", "content"
    )
    # id as short title
    meta$shortTitle <- id
    # Fetch url
    meta$url <- url
    # Fetch date
    meta$date <- ReadAttr(
      data, "//meta[@name='citation_publication_date']", "content"
    )
    # Fetch version
    version <- ReadAttr(
      data, "//meta[@name='citation_title']", "content"
    )
    # Set version
    meta$versionNumber <- gsub(".*version |\\][^.]*$", "", version)
    # Set archive
    meta$archive <- "Comprehensive R Archive Network (CRAN)"
    meta$archiveLocation <- id
    # Fetch abstract
    meta$abstractNote <- ReadAttr(
      data, "//meta[@name='og:description']", "content"
    )
    # Fetch creator(s)
    n.creators <- sum(grepl("citation_author", data))
    meta$creators <- ZoteroCreator(lapply(seq_along(n.creators), \(i) {
      if (n.creators == 1) i <- ""
      path <- sprintf("//meta[@name='citation_author%s']", i)
      name <- unlist(strsplit(ReadAttr(
        data, path, "content"
      ), " "))
      list(
        type = "author",
        name = c(tail(name,1),
                 paste(name[seq_along(length(name)-1)], collapse = " "))
      )
    }))
    # Fetch rights
    meta$rights <- ReadXpath(
      data, "//td[.=\"License:\"]/following-sibling::td", FALSE
    )
    # Set abstractNote to string
    meta$abstractNote <- CleanText(
      ToString(GoFish(meta$abstractNote, ""), "\n")
    )
    # Set accessDate
    meta$accessDate <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%S%z")
    # Create zotero-type matrix
    meta <- GoFish(ZoteroFormat(meta), NULL)
    # Remove if no Creator is found
    if (all(is.na(GoFish(meta$creators[[1]])))) meta <- NULL

    return (list(data = meta, log = log))

  }

  # Start time for query
  query.start <- Sys.time()

  # Cycle through queries
  ## Should perhaps vectorize, but for loop seems more informative tbh
  for (i in seq_along(id)) {

    # Check DOI
    check <- Cran(id[[i]], meta, silent, log)

    # Add log
    log <- append(log, check$log)

    # Skip if error
    if (is.null(check$data)) {
      next
    }

    # Append data
    data <- AddAppend(check$data, data)

    # Estimate time of arrival
    log.eta <- LogCat(
      Eta(query.start, i, length(id)),
      silent = silent,
      flush = TRUE,
      append.log = FALSE
    )
  }
  # Add to log
  log <- append(log, log.eta)

  return (list(data = data, log = log))

}
