#' @title Use DOI to acquire metadata
#' @description Connects with doi.org to create metadata
#' @param doi A digital object identifier
#' @param meta A list collecting all metadata used to create , Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Simple use of `ZoteroDoi`
#'     example <- ZoteroDoi("10.1126/sciadv.abd1705")
#'     # Use `ZoteroIndex` to print
#'     ZoteroIndex(example)$name
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
ZoteroDoi <- \(doi, meta = list()) {

  # Visible bindings
  key <- NULL

  # Function to check DOI
  CheckDOI <- \(doi) {

    # Visible bindings
    error <- TRUE
    data <- type <- NULL

    # Check if DOI
    if (grepl("^.*(10\\..*)", doi)) {

      # Remove any https part
      doi <- Trim(gsub("^.*(10\\..*)", "\\1", doi, perl = TRUE))

      # Try DOI key
      doi.works <- httr::RETRY(
        "GET", sprintf("https://doi.org/api/handles/%s", doi),
        quiet = TRUE
      )

      if (doi.works$status_code == "200") {

        # Format JSON date
        doi.json <- jsonlite::fromJSON(
          ParseUrl(doi.works, "text")
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

        # Query Crossref for metadata
        crossref.data <- httr::RETRY(
          "GET", sprintf("https://api.crossref.org/works/%s.xml", doi),
          quiet = TRUE
        )

        # Set type and data as crossref data if found
        if (crossref.data$status_code == "200") {

          # Set libraryCatalog
          meta$libraryCatalog <- "DOI.org (Crossref)"

          # Set error to FALSE
          error <- FALSE

          # Set data as crossref data
          data <- crossref.data

          # Else check for datacite data
        } else {
          # Query datacite for metadata
          datacite.data <- httr::RETRY(
            "GET", sprintf("https://api.datacite.org/dois/%s", doi),
            quiet = TRUE
          )
          # Set type and data as datacite data if found
          if (datacite.data$status_code == "200") {

            # Set libraryCatalog
            meta$libraryCatalog <- "DOI.org (Datacite)"
            # Set error to FALSE
            error <- FALSE
            # Set data as datacite data

            # Format datacite as JSON
            datacite.json <- jsonlite::fromJSON(
              ParseUrl(datacite.data, "text")
            )$data$attributes

            # There is currently a bug in OSF/Datacite XML where
            # Authors are not correctly identified.
            # Thus, c2z currently use JSON data to create creators metadata

            # Add url
            meta$url <- GoFish(datacite.json$url)

            # Run if osf.io in url
            if (grepl("osf.io", meta$url)) {

              # Find osf data
              osf.data <- httr::RETRY(
                "GET", sprintf("https://api.osf.io/v2/nodes/%s/contributors/",
                               basename(meta$url)),
                query = stats::setNames(list("true", "json"),
                                        c("filter[bibliographic]","format")),
                quiet = TRUE
              )

              # Format osf data as JSON
              osf.json <- jsonlite::fromJSON(
                ParseUrl(osf.data, "text")
              )$data$embeds$users$data$attributes

              # Create Zotero-type matrix
              meta$creators <- ZoteroCreator(lapply(1:nrow(osf.json), \(i) {
                list(
                  type = "author",
                  name = c(osf.json[i,]$family_name, osf.json[i,]$given_name)
                )
              }))

              # Else find creators from datacite data
            } else {

              # Find creators
              creators <- GoFish(datacite.json$creators$name)
              if (any(!is.na(creators))) {
                meta$creators <- ZoteroCreator(
                  lapply(creators, \(x) {
                    c(type = "author",
                      name = strsplit(x, ", "))
                  })
                )
              }

            }

            # Check for contributors
            contributors <- GoFish(datacite.json$contributors$name)
            # ... and the role of he contribuors
            contributor.type <- GoFish(
              datacite.json$contributors$contributorType
            )
            # Append any contributors to creators
            if (any(!is.na(contributors))) {
              meta$creators <- AddAppend(ZoteroCreator(
                lapply(1:length(contributors), \(i) {
                  c(type = contributor.type[i],
                    name = strsplit(contributors[i], ", "))
                })
              ), meta$creators)
            }

            # Set data as datacite data
            data <- rawToChar(jsonlite::base64_dec(
              datacite.json$xml
            ))

          } # End check of datecite
        } # End check of crossref
      } # End of DOI lookup
    } # End check of DOI

    # Create return list
    return.list <- list(
      error = error,
      data = data,
      meta = meta
    )

    return (return.list)

  }

  # Function to convert Crossref XML to zotero-type reference
  DOI <- \(data, meta) {

    # Read  metadata
    data <- rvest::read_html(data)

    # Run as crossref
    if (meta$libraryCatalog == "DOI.org (Crossref)") {
      meta <- DoiCrossref(data, meta)
      # Else run as datacite
    } else if (meta$libraryCatalog == "DOI.org (Datacite)") {
      meta <- DoiDatacite(data, meta)
    }

    # Set abstractNote to string
    meta$abstractNote <- ToString(GoFish(meta$abstractNote,""),"\n")

    # Set accessDate
    meta$accessDate <- as.character(Sys.time())

    # Create zotero-type matrix
    meta <- GoFish(ZoteroFormat(meta), NULL)

    # Remove if no Creator is found
    if (all(is.na(GoFish(meta$creators[[1]])))) meta <- NULL

    return (meta)

  }

  # Fetch metadata from Crossref
  metadata <- lapply(doi, \(x) {
    x <- CheckDOI(x)
    if (!x$error) DOI(x$data, x$meta)
  })

  # Check if metadata has tibbles
  if (any(lengths(metadata))) {
    metadata <- dplyr::bind_rows(metadata)

    # Set metadata as null if empty
  } else {
    metadata <- NULL
  }

  return (metadata)

}
