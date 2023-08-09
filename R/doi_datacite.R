#' @title Wrangle DataCite metadata into Zotero-type format
#' @description Query DataCite by DOI and fetch metadata
#' @param data XML data from DataCite containing metadata
#' @param meta A list collecting all metadata used to create , Default: list()
#' @param silent c2z is noisy, tell it to be quiet, Default: TRUE
#' @param log A list for storing log elements, Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # `DoiDatacite` is called from `ZoteroDoi` (if item found)
#'   example <- ZoteroDoi("10.17605/OSF.IO/7HZ4P")
#'
#'   # Use `ZoteroIndex` to print
#'   # Print index using `ZoteroIndex`
#'   if (any(nrow(example$data))) {
#'     ZoteroIndex(example$data) |>
#'       dplyr::select(name) |>
#'       print(width = 80)
#'
#'     # Display catalog
#'     example$data$libraryCatalog
#'   }
#' }
#' @rdname DoiDatacite
#' @export
DoiDatacite <- \(data, meta = list(), silent = TRUE, log = list()) {

  # Format datacite as JSON
  datacite.json <- GoFish(jsonlite::fromJSON(
    ParseUrl(data, "text")
  )$data$attributes)

  # Return NULL if empty
  if (all(is.na(datacite.json))) {
    return (list(data = NULL, log = log))
  }

  # There is currently a bug in OSF/Datacite XML where
  # Authors are not correctly identified.
  # Thus, c2z currently use JSON data to create creators metadata

  # Add url
  meta$url <- GoFish(data$url)

  # Run if osf.io in url
  if (grepl("osf.io", datacite.json$url)) {

    # Find osf data
    httr.get <- Online(
      httr::RETRY(
        "GET",
        sprintf("https://api.osf.io/v2/nodes/%s/contributors/",
                basename(meta$url)
        ),
        query = stats::setNames(
          list("true", "json"),
          c("filter[bibliographic]","format")
        ),
        quiet = TRUE,
      ),
      silent = silent,
      message = "osf.io"
    )

    # Add to log
    log <- append(log, httr.get$log)

    # Log and return data if error
    if (!httr.get$error) {

      # Format osf data as JSON
      osf.json <- jsonlite::fromJSON(
        ParseUrl(httr.get$data, "text")
      )$data$embeds$users$data$attributes

      # Create Zotero-type matrix
      meta$creators <- ZoteroCreator(
        lapply(seq_len(nrow(osf.json)), \(i) {
          list(
            type = "author",
            name = c(osf.json[i,]$family_name,
                     osf.json[i,]$given_name)
          )
        })
      )
    }
  }

  # Find creators fo datacite.json if creators is NULL
  if (is.null(meta$creators)) {

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
      lapply(seq_along(contributors), \(i) {
        c(type = contributor.type[i],
          name = strsplit(contributors[i], ", "))
      })
    ), meta$creators)
  }

  # Find xml datacite data
  xml.data <- GoFish(rvest::read_html(
    rawToChar(jsonlite::base64_dec(datacite.json$xml))
  ))

  # Return NULL if empty
  if (all(is.na(xml.data))) {
    return (list(data = NULL, log = log))
  }

  # Find metadata nodes
  ref <- xml.data |>
    rvest::html_nodes(xpath = "//resource") |>
    rvest::html_children()

  # Set itemType
  meta$itemType <- "preprint"

  # Fetch titles
  titles <- ReadXpath(ref, "//titles/title")
  # Find main title (any without titletype)
  meta$title <- titles[
    is.na(ReadAttr(ref, "//titles/title", "titletype"))
  ]
  # Fetch translated title
  translation <- titles[
    grepl("TranslatedTitle", ReadAttr(ref, "//titles/title", "titletype"))
  ]
  # Combine title and translated title if translation exist
  if (length(translation)) {
    # Set short-title
    meta$title <- sprintf("%s (%s)", meta$title, translation)
  }
  # Fetch subtitle
  subtitle <- titles[
    grepl("Subtitle", ReadAttr(ref, "//titles/title", "titletype"))
  ]
  # Combine title and subtitle if subtitle exist
  if (length(subtitle)) {
    # Set short-title
    meta$shortTitle <- meta$title
    meta$title <- paste0(meta$title,": ", subtitle)
  }

  # Set abstract
  meta$abstractNote <- ReadXpath(ref, "//descriptions")

  # Find language
  meta$language <- GoFish(ReadXpath(ref, "//language"))
  if (any(is.na(meta$language))) {
    meta$language <- ReadAttr(ref, "//titles/title", "xml:lang")
    if (length(meta$language )>1) meta$language <- "mul"
  }

  # Fetch copyrights
  meta$rights <- GoFish(ReadAttr(ref, "//rights", "rightsidentifier"))

  # Fetch resource type
  type <- GoFish(ReadXpath(ref, "//resourcetype"))
  if (any(grepl("^$", type)) | any(is.na(type))) type <- ReadAttr(
    ref, "//resourcetype", "resourcetypegeneral"
  )
  # List over datacites types

  datacite.types <- c(audiovisual = "audiovisual",
                      book = "Book",
                      bookchapter = "Book chapter",
                      collection = "Collection",
                      computationalnotebook = "Computational notebook",
                      conferencepaper = "Conference paper",
                      conferenceproceeding = "Conference proceeding",
                      datapaper = "Data paper",
                      dataset = "Dataset",
                      dissertation = "Dissertation",
                      event = "Event",
                      image = "Image",
                      interactiveresource = "Interactive resource",
                      journal = "Journal",
                      journalarticle = "Journal article",
                      model = "Model",
                      monograph = "Book",
                      outputmanagementplan = "Output management plan",
                      peerreview = "Peer review",
                      physicalobject = "Physical object",
                      preprint = "Preprint",
                      report = "Report",
                      service = "Service",
                      software = "Software",
                      sound = "Sound",
                      standard = "Standard",
                      text = "Text",
                      thesis = "Thesis",
                      workflow = "Workflow",
                      other = "Other")

  # Define genre according to match in datacite genres
  meta$genre <- datacite.types[names(datacite.types) %in% tolower(type)]
  # Set as supplemental materials if not found
  if (!length(meta$genre)) meta$genre <- "Supplemental materials"

  # set DOI
  meta$DOI <- ReadXpath(ref, "//identifier", FALSE)
  if (is.na(meta$url) & length(meta$DOI)) meta$url <- paste0(
    "https://doi.org/", meta$DOI
  )


  # Set date
  meta$date <- ReadXpath(ref, "//publicationyear")
  if (!length(meta$date)) meta$date <- GoFish(
    tail(ReadXpath(ref, "//dates/date"),1)
  )

  # Fetch tags
  tags <- ReadXpath(ref, "//subjects/subject")
  if (length(tags)) {
    meta$tags <- data.frame(tag = unique(unlist(strsplit(tags, ", "))))
  }

  # Set published as repository
  meta$repository <- GoFish(ReadXpath(ref, "//publisher")[[1]])

  # Find size and eventually append to extra
  size <- ReadXpath(ref, "//sizes", FALSE)
  if (length(size)) meta$extra <- AddAppend(
    paste("Size:", ToString(size)), meta$extra, "\n"
  )
  # Find format and eventually append to extra
  format <- ReadXpath(ref, "//format", FALSE)
  if (length(format)) meta$extra <- AddAppend(
    paste("Format:", ToString(format)), meta$extra, "\n"
  )
  # Find version and eventually append to extra
  version <- ReadXpath(ref, "//version", FALSE)
  if (length(version)) meta$extra <- AddAppend(
    paste("Version:", ToString(version)), meta$extra, "\n"
  )

  return (list(data = meta, log = log))

}
