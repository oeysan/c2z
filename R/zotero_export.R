#' @title Export Zotero items to bibliography
#' @description Export Zotero items to a specified format (e.g., BibLaTeX) using a CSL format (e.g., APA7)
#' @param zotero A list with information on the specified Zotero library (e.g., id, API key, collections, and items)
#' @param csl.type Specify a CSL type to Official repository for Citation Style Language (CSL), Default: NULL
#' @param csl.name Name of saved CSL file, Default: 'style'
#' @param locale Desired language format of bibliography, Default: 'en-US'
#' @param format Export format of Zotero items, Default: 'biblatex'
#' @param bib.name Name of exported bibliography, Default: 'references'
#' @param append Append extra metadata to Zotero query, Default: FALSE
#' @param include Include bibliography (i.e., `bib`) and/or citation (i.e., `citation`), Default: NULL
#' @param style Citation style to use for appended bibliography and/or citations, Default: apa
#' @param save.data Save data (e.g., bibliography) to disk, Default: FALSE
#' @param save.path Location to store data on disk, Default: NULL
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return A list with information on the specified Zotero library (e.g., exported items, bibliography and citations)
#' @seealso
#'  \code{\link[dplyr]{select}}
#'  \code{\link[tidyselect]{where}}
#' @rdname ZoteroExport
#' @export
ZoteroExport <- \(zotero,
                  csl.type = NULL,
                  csl.name = "style",
                  locale = "en-US",
                  format = "biblatex",
                  bib.name = "references",
                  append = FALSE,
                  include = NULL,
                  style = "apa",
                  save.data = FALSE,
                  save.path = NULL,
                  silent = FALSE) {

  # Run if items exists and
  if (!is.null(zotero$items)) {

    # Get CSL if csl.type is defined
    if (!is.null(csl.type)) {

      # Define CSL
      # Select base url
      base.url <- "https://raw.githubusercontent.com"
      # Add csl
      csl.url <- sprintf("%s/citation-style-language/styles/master/%s.csl",
                         base.url,
                         csl.type)
      # Query for csl
      get.csl <- httr::RETRY("GET", csl.url)
      # Format csl
      csl.style <- rawToChar(get.csl$content)

      # Save data if save.data is TRUE
      if (save.data) {
        saved.file <- SaveData(csl.style, csl.name, "csl", save.path)

        # Add to log
        zotero$log <- LogCat(sprintf("CSL saved as %s format in %s",
                              csl.type,
                              saved.file),
                      silent = silent,
                      log = zotero$log)

      }

    }

    # Get keys from items that is not attachments or notes
    export.keys <- zotero$items$key[
      !grepl('attachment|note', zotero$items$itemType)
    ]

    # JUST SOME MORE.....
    n.exported <- Pluralis(length(export.keys),
                           "item", "items")

    # Saving data
    zotero$log <- LogCat(paste("Exporting", n.exported),
                         silent = silent,
                         log = zotero$log)

    # Fetch data in requested format
    export.data <- ZoteroGet(Zotero(user = zotero$user,
                                    id = zotero$id,
                                    api = zotero$api),
                             append.items = TRUE,
                             item.keys = paste(export.keys,
                                               collapse=","),
                             format = format,
                             force = TRUE,
                             silent = TRUE)

    # Add export data
    zotero$export <- export.data$results

    # Remove excess columns if export format is a tibble
    if (is.data.frame(zotero$export)) {
      zotero$export <- zotero$export |>
        dplyr::select(tidyselect::where(~all(!is.na(.))))
    }

    # Fetch additional data if include is defined
    if (!is.null(include)) {
      bib.data <- ZoteroGet(Zotero(user = zotero$user,
                                   id = zotero$id,
                                   api = zotero$api),
                            append.items = TRUE,
                            item.keys = paste(export.keys,
                                              collapse=","),
                            include = include,
                            style = style,
                            force = TRUE,
                            silent = TRUE)

      zotero$bibliography <- bib.data$bibliography
      zotero$citation <- bib.data$citation
    }


    # Saving data
    zotero$log <- LogCat(paste("Items exported as", format),
                         silent = silent,
                         log = zotero$log)

    # Do if save data is TRUE
    if (save.data) {

      # Define export
      ## if JSON use items in JSON format
      if (format == "json") {
        zotero$export <- ZoteroToJson(zotero$items)
        extension <- "json"
        # Else use format in export
      } else {
        extension <- Mime(
          export.data$data.cache$headers$`content-type`
        )
      }

      # Save as single file is save is set to TRUE
      saved.file <- SaveData(zotero$export,
                             bib.name,
                             extension,
                             save.path,
                             append)

      # Add to log
      zotero$log <- LogCat(paste("Items saved in", saved.file),
                           silent = silent,
                           log = zotero$log)

    }

  }

  return (zotero)

}
