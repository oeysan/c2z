#' @title Wrangle CRAN metadata into Zotero-type format
#' @description Query CRAN by name and fetch metadata
#' @param id name of R package
#' @param meta A list collecting all metadata used to create , Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Search cran for package `bfw`
#'     example <- ZoteroCran("bfw")
#'     # Use `ZoteroIndex` to print
#'     ZoteroIndex(example)$name
#'   }
#' }
#' @seealso
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[rvest]{reexports}}
#' @rdname ZoteroCran
#' @export
ZoteroCran <- \(id, meta = list()) {

  # Visible bindings
  key <- NULL

  Cran <- \(id, meta) {

    cran.url <- sprintf(
      "https://cran.r-project.org/package=%s",
      id)
    # Query for metadata
    data <- httr::RETRY("GET", cran.url) |>
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
    meta$url <- cran.url
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
    meta$creators <- ZoteroCreator(lapply(1:n.creators, \(i) {
      if (n.creators == 1) i <- ""
      path <- sprintf("//meta[@name='citation_author%s']", i)
      name <- unlist(strsplit(ReadAttr(
        data, path, "content"
      ), " "))
      list(
        type = "author",
        name = c(tail(name,1),
                 paste(name[1:(length(name)-1)], collapse = " "))
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
    meta$accessDate <- as.character(Sys.time())
    # Create zotero-type matrix
    meta <- GoFish(ZoteroFormat(meta), NULL)
    # Remove if no Creator is found
    if (all(is.na(GoFish(meta$creators[[1]])))) meta <- NULL

    return (meta)

  }

  # loop through defined MeldSt
  metadata <- lapply(id, \(x) Cran(x, meta))

  # Check if metadata has tibbles
  if (any(lengths(metadata))) {
    metadata <- dplyr::bind_rows(metadata)
    # Set metadata as null if empty
  } else {
    metadata <- NULL
  }

  return (metadata)

}
