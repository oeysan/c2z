#' @title Wrangle regjeringen.no metadata into Zotero-type format
#' @description Query regjeringen by search word and type and fetch metadata
#' @param search Search term (e.g., 2018: 2)
#' @param type type of query (e.g., white paper, official norwegian reports), Default: "NOU"
#' @param meta A list collecting all metadata used to create , Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Search the default entity, Norwegian official reports in regjeringen.no
#'     example <- ZoteroGov("2001:4")
#'     # Use `ZoteroIndex` to print
#'     ZoteroIndex(example)$name
#'   }
#' }
#' @seealso
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[rvest]{reexports}}
#'  \code{\link[dplyr]{bind}}, \code{\link[dplyr]{arrange}}
#' @rdname ZoteroGov
#' @export
ZoteroGov <- \(search, type = "NOU", meta = list()) {

  # Visible bindings
  key <- NULL

  # List of search ids by name
  ids <- list(
    nou = "id1767",
    hoyring = "id1763",
    meldst = "id1754",
    prop = "id1753",
    rettleiar = "id438822",
    rapport = "id438817",
    anbud = "id438824",
    lov = "id438754",
    brev = "id2000006"
  )

  # Define search type according to match in ids
  id <- ids[names(ids) %in% tolower(type)]
  # Or if type is actual id
  if (!length(id)) id <- ids[ids %in% tolower(type)]
  # Set as NOU if not found
  if (!length(id)) id <- "id1767"

  # Function to create zotero-type matrix from MeldSt
  GovSearch <- \(search, id, meta) {

    # Search data
    gov.search <- httr::RETRY(
      "GET", sprintf("https://www.regjeringen.no/%s/", id),
      query = list(term = search, sortby = 0), quiet = TRUE
    ) |> rvest::read_html()

    search.id <- gov.search |>
      rvest::html_nodes(".results ul li:first-child .title a") |>
      rvest::html_attr('href') |>
      basename()

    # Run if serach id is defined
    if (!is.na(search.id)) {

      # Find results data
      search.data <- httr::RETRY(
        "GET", paste0("https://www.regjeringen.no/", search.id), quiet = TRUE
      )

      # Define html
      search.html <- search.data$url |> rvest::read_html()

      # Set itemType
      meta$itemType <- "document"

      # Find title
      title <- ReadAttr(
        search.html, "//meta[@name='DC.Title']", "content"
      )
      # Use author if empty
      if (!length(title)) title <- ReadAttr(
        search.html, "//meta[@name='title']", "content"
      )
      # Use current owner if empty
      if (!length(title)) title <- ReadCss(
        search.html, ".article-header h1"
      )
      # Find subtitle
      subtitle <- ReadCss(
        search.html, ".article-header h2.subheader"
      )
      # Find publication info (publisher, date)
      meta$publisher <- ReadAttr(
        search.html, "//meta[@name='DC.Creator']", "content"
      )
      # Use author if empty
      if (!length(meta$publisher)) meta$publisher <- ReadAttr(
        search.html, "//meta[@name='author']", "content"
      )
      # Use current owner if empty
      if (!length(meta$publisher)) meta$publisher <- ReadCss(
        search.html, ".content-owner-dep div:first-child"
      )

      # PROP, NOU, MELDST as special cases
      if (id == "id1767" | id == "id1754" | id == "id1753") {
        # Set title as creator
        creator <- title
        # Set subtitle as author if PROP, NOU, MELDST
        title <- ReadCss(
          search.html, ".article-header h2.subheader"
        )
        # Else set creator as publisher
      } else {

        if(length(subtitle)) title <- sprintf("%s: %s", title, subtitle)

        # Set creator as publisher
        creator <- meta$publisher
      }

      # Set title
      meta$title <- title

      # Create zotero-type creator matrix
      meta$creators <- data.frame(
        creatorType = "author",
        name = creator
      )

      # Find date
      meta$date <- ReadAttr(
        search.html, "//meta[@name='DC.Date']", "content"
      )
      # Find publication info (publisher, date)
      abstract <- ReadCss(search.html, ".article-ingress p:first-child")
      # Find publication info (publisher, date)
      if (!length(abstract)) abstract <- ReadAttr(
        search.html, "//meta[@name='DC.Description']", "content"
      )
      # Use author if empty
      if (!length(abstract)) abstract <- ReadAttr(
        search.html, "//meta[@name='description']", "content"
      )
      # Define abstract
      if (length(abstract)) meta$abstractNote <- ToString(Trim(abstract), "\n")
      # Set language
      meta$language <- ReadAttr(
        search.html, "//meta[@name='DC.Language']", "content"
      )
      # Url to document on regjeringen.no
      meta$url <- paste0("https://www.regjeringen.no/",
                         basename(search.data$url)
      )
      # Find keywords
      tags <- ReadAttr(
        search.html, "//meta[@name='keywords']", "content"
      )
      if (!length(tags)) tags <- ReadAttr(
        search.html, "//meta[@name='DC.Subject']", "content"
      )
      if (length(tags)) {
        meta$tags <- data.frame(
          tag = unique(Trim(unlist(strsplit(tags, ", "))))
        )
      }

      # Set regjeringen.no id in extra
      meta$extra <- sprintf("Regjeringen: %s.",
                            basename(search.data$url)
      )

      # Set abstractNote to string
      meta$abstractNote <- ToString(GoFish(meta$abstractNote,""),"\n")

      # Set accessDate
      meta$accessDate <- as.character(Sys.time())

      # Create zotero-type matrix
      meta <- GoFish(ZoteroFormat(meta), NULL)

      # Remove if no Creator is found
      if (all(is.na(GoFish(meta$creators[[1]])))) meta <- NULL

    }

    return (meta)

  }

  # loop through defined ids
  metadata <- lapply(search, \(x) GovSearch(x, id, meta))

  # Check if metadata has tibbles
  if (any(lengths(metadata))) {
    metadata <- dplyr::bind_rows(metadata)
    # Set metadata as null if empty
  } else {
    metadata <- NULL
  }

  return (metadata)
}
