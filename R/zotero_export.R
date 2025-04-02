#' @title Export Zotero items to bibliography
#' @description Export Zotero items to a specified format (e.g., BibLaTeX) using
#'   a CSL format (e.g., APA7)
#' @param zotero A list with information on the specified Zotero library (e.g.,
#'   id, API key, collections, and items)
#' @param csl.type Specify a CSL type to Official repository for Citation Style
#'   Language (CSL), Default: NULL
#' @param csl.name Name of saved CSL file, Default: 'style'
#' @param locale Desired language format of bibliography, Default: 'en-US'
#' @param format Export format of Zotero items, Default: 'biblatex'
#' @param bib.name Name of exported bibliography, Default: 'references'

#' @param save.data Save data (e.g., bibliography) to disk, Default: FALSE
#' @param save.path Location to store data on disk, Default: NULL
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return A list with information on the specified Zotero library (e.g.,
#'   exported items, bibliography and citations)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Define Zotero list according to default group
#'   zotero = Zotero(
#'     user = FALSE,
#'     id = "4827927",
#'     api = "Io1bwAiOjB1jPgUNegjJhJxF",
#'     item.type = "-attachment || note",
#'     max.result = 1,
#'     library = TRUE
#'   )
#'
#'   # Export 1 items from the default group
#'   example <- ZoteroExport(
#'     zotero
#'   )
#'   # Display exported
#'   cat(example$export, fill = 80)
#' }
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
                  save.data = FALSE,
                  save.path = NULL,
                  silent = FALSE) {

  # Return Zotero list if items are not defined
  if (is.null(zotero$items)) {
    # Add to log
    zotero$log <- LogCat(
      "The items list is empty",
      silent = silent,
      log = zotero$log
    )
    return (zotero)
  }

  # Visible bindings
  itemType <- key <- NULL

  # Get CSL if csl.type is defined
  if (!is.null(csl.type)) {

    csl.type <- Csl(csl.type, csl.name, save.data, save.path, silent)
    zotero$log <- append(zotero$log, csl.type$log)
    zotero$bib.style <- csl.type
  }

  # Get keys from items that is not attachments or notes
  export.keys <- zotero$items |>
    dplyr::filter(!grepl("attachment|note", itemType)) |>
    dplyr::pull(key)

  # Fetch data in requested format
  zotero <- ZoteroGet(
    zotero,
    use.collection = FALSE,
    item.keys = export.keys,
    format = format,
    force = TRUE,
    silent = silent,
    result.type = sprintf("`%s` reference", format)
  )

  # Set export
  zotero$export <- zotero$results
  # Set export extension
  export.extension <- zotero$data.cache$headers$`content-type`

  # Remove excess columns if export format is a tibble
  if (is.data.frame(zotero$export)) {
    zotero$export <- zotero$export |>
      dplyr::select(tidyselect::where(~all(!is.na(.))))
  }

  # Do if save data is TRUE
  if (save.data) {

    # Define export
    ## if JSON use items in JSON format
    if (format == "json") {
      zotero$export <- ZoteroToJson(zotero$export)
      extension <- "json"
      # Else use format in export
    } else {
      extension <- Mime(export.extension)
    }

    # Save as single file is save is set to TRUE
    zotero$bib.file <- SaveData(
      zotero$export,
      bib.name,
      extension,
      save.path
    )

    # Add to log
    zotero$log <- LogCat(
      paste("Bibliography saved in", zotero$bib.file),
      silent = silent,
      log = zotero$log
    )

  }

  return (zotero)

}
