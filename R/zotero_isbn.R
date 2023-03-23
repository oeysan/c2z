#' @title Search libraries using ISBN or MMS ID
#' @description Query libraries using ISBN (or MMS ID) and fetch metadata
#' @param key Key to search with (e.g., ISBN or MMS ID)
#' @param meta A list collecting all metadata used to create , Default: list()
#' @return A Zotero-type matrix (tibble)
#' @seealso
#'  \code{\link[rvest]{rename}}, \code{\link[rvest]{html_children}},
#'  \code{\link[rvest]{html_attr}}, \code{\link[rvest]{html_text}},
#'  \code{\link[rvest]{reexports}}
#'  \code{\link[dplyr]{bind}}, \code{\link[dplyr]{distinct}},
#'  \code{\link[dplyr]{arrange}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[stats]{setNames}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname ZoteroIsbn
#' @details Please see
#' \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Search libraries for ISBN metadata
#'   example <- ZoteroIsbn("978-1529797138")
#'
#'   # Print index using `ZoteroIndex`
#'   ZoteroIndex(example) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
#' }
#' @export
ZoteroIsbn <- \(key, meta = list()) {

  # Visible bindings
  metadata <- NULL

  # Function to check key format
  CheckId <- \(key) {

    # Trim and remove foreign characters from key
    key <- GoFish(as.character(
      Trim(gsub('[^[:alnum:] ]', "", key))
    ), NULL)

    if (!is.null((key))) {
      # Convert to ISBN13 if ISBN10
      if (nchar(key) == 10) {
        isbn10 <- c(9,7,8, as.numeric(strsplit(key,"")[[1]][1:9]))
        check <- sum(isbn10 * c(1,3)) %% 10
        check <- if (check == 0) check else 10 - check
        key <- paste0(ToString(isbn10, ""),check)
      }
      if (!(substr(key, 1, 2) == "97" & nchar(key) == 13 |
            substr(key, 1, 2) == "99" & nchar(key) > 7)) {
        key <- NULL
      }
    }

    return (key)
  }

  # Function to fetch metadata from ALMA/LoC using ISBN or MMS id
  ISBN <- \(key, meta = list()) {

    # Visible bindings
    metadata <- NULL

    # Set key as MMS if firs two key digits are 99 else use ISBN
    query.type <- if (substr(key, 1, 2) == "99") "mms_id" else "isbn"

    # Query alma for metadata
    marc.get <- httr::RETRY("GET",
                            "https://api.bibs.aws.unit.no/alma",
                            query = stats::setNames(list(key), query.type),
                            quiet = FALSE)

    if (marc.get$status_code == 200) {

      # Parse content as text
      metadata <- ParseUrl(marc.get, "text")

      # Run if data is not empty
      if (!is.null(metadata)) {

        # Format data from JSON
        marc.json <- jsonlite::fromJSON(metadata)
        # Find MARC id
        id <- unique(marc.json$id)
        # Determine number of ids (should be one, but you know...)
        n.id <- length(marc.json$id)

        if (n.id > 1) marc.json <- marc.json[
          which(as.numeric(id) == min(as.numeric(id))),
        ]

        # MARCXML metadata
        metadata <- list(rvest::read_html(marc.json$xmlPresentation))
        # Set libraryCatalog
        meta$libraryCatalog <- "Alma (47BIBSYS)"

      }
    }

    # Try LOC if data is still empty
    if (is.null(metadata)) {

      # Query list
      query <- list(
        query = paste0("bath.ISBN=", key),
        operation = "searchRetrieve",
        version = "1.1",
        maximumRecords = 1
      )

      # Query loc for metadata
      marc.get <- httr::RETRY("GET",
                              "http://lx2.loc.gov:210/LCDB",
                              query = query, quiet = FALSE)

      if (marc.get$status_code == 200) {

        # MARCXML metadata
        metadata <- list(rvest::read_html(marc.get))

        # Number of results
        n.data <- suppressWarnings(as.numeric(
          ReadXpath(metadata[[1]], "//numberofrecords", FALSE)
        ))

        # Set libraryCatalog
        meta$libraryCatalog <- "Library of Congress"

        # Set metadata as NULL if no results or empty
        if (length(n.data)) {
          if (n.data != 1) metadata <- NULL
        }

      }

    }

    # Convert MARC21 to zotery-type matrix
    if (!is.null(metadata)) {
      metadata <- ReadMARC(metadata[[1]], meta)
    }

    return (metadata)
  }

  # Function to read MARC XML
  ReadMARC <- \(marc, meta) {

    # Function to convert XML fields
    MARC <- \(marc, tag, code = NULL, clean.text = TRUE) {

      # Run if extracting specific code tag within tags
      if (!is.null(code)) {

        # Define path
        x.path <- sprintf(
          "//datafield[@tag='%s']//subfield[@code='%s']",
          tag, code
        )

        # Extract text from specified tag
        field <- ReadXpath(marc, x.path, clean.text)

        # Else find all elements within specified tag
      } else {

        # Find selected tag
        x.path <- sprintf("//datafield[@tag='%s']", tag)
        # Extract data
        datafields <- marc |> rvest::html_nodes(xpath = x.path)

        # Format each datafield
        field <- dplyr::bind_rows(lapply(datafields, function(x) {
          subfield <- rvest::html_children(x)
          code <- subfield |> rvest::html_attr("code")
          text <- subfield |> rvest::html_text()
          names(text) <- make.unique(code)
          as.data.frame(t(text))
        }))

      }

      # Set empty fields as NULL
      if (!length(field)) field <- NULL

      return (field)

    }

    # Function to create Zotero-type creators matrix
    FormatCreator <- \(creator) {
      if (!is.null(creator)) {
        # Check if data.frame
        if (!is.data.frame(creator)) {
          creator <- data.frame(a = creator)
        }
        # Split name by namme
        name <- Trim(strsplit(creator$a, "," )[[1]])
        # If column marked 4 exist use contents else authors
        type <- if ("4" %in% names(creator)) creator[, "4"] else "aut"
        # Create zotero matrix
        creator <- ZoteroCreator(list(type = type, name = name))
      }
      return (creator)
    }

    # Find leader to subtract type of document
    leader <- ReadXpath(marc, "//leader", FALSE)
    leader.type <- strsplit(leader[1], "")[[1]][1:8]

    meta$itemType <- if (leader.type[7] == "a" &
                         (leader.type[8] == "b" |
                          leader.type[8] == "i" |
                          leader.type[8] == "s")) {
      "journalArticle"
    } else if (leader.type[7] == "t") {
      "manuscript"
    } else if (leader.type[7] == "c" |
               leader.type[7] == "d" |
               leader.type[7] == "i" |
               leader.type[7] == "j" ) {
      "audioRecording"
    } else if (leader.type[7] == "e" |
               leader.type[7] == "f") {
      "map"
    } else if (leader.type[7] == "g" |
               leader.type[7] == "o" |
               leader.type[7] == "r" ) {
      "fim"
    } else if (leader.type[7] == "m" ) {
      "artwork"
    } else if (leader.type[7] == "k" ) {
      "software"
    } else {
      "book"
    }

    # Fetch title
    meta$title <- ToString(
      MARC(marc, "245", "a"), " \u2013 ")
    # Fetch subtitle
    subtitle <- MARC(marc, "245", "b")
    # Combine title and subtitle if subtitle exist
    if (length(subtitle)) meta$title <- paste0(meta$title,": ", subtitle)

    # Fetch part number if part of a volume
    part.number <- GoFish(MARC(marc, "245", "n")[[1]])
    # Fetch volume name
    part.name <- GoFish(ToString(MARC(marc, "245", "p"), " \u2013 "))

    # Define volume title if part number and part name exists
    if (!is.na(part.number)) {
      if (!is.na(part.name)) part.number <- paste0(part.number, ": ", part.name)
      meta$title <- sprintf("%s %s", meta$title, part.number)
    }

    # Fetch creator
    creator <- FormatCreator(MARC(marc, "100"))
    # Fetch contributors
    creators <- MARC(marc, "700")

    # Set contributors as editors if no first author exists
    if (is.null(creator) & (!is.null(creators))) {
      if (!"4" %in% names(creators)) creators[,"4"] <- "edt"
    }

    # Create zotero-type creator matrix if creators exists
    if (!is.null(creators)) {
      creators <- lapply(seq_len(nrow(creators)), \(i) {
        FormatCreator(creators[i, ])
      })
    }

    # Combine creator and contributors matrices
    meta$creators <- dplyr::bind_rows(creator,creators) |>
      dplyr::distinct()

    # Check statement of responsibility field
    statement <- MARC(marc, "245", "c")
    if (!is.null(statement)) {
      statement <- strsplit(statement, ",") |> # split by comma
        unlist() |>
        (\(.) strsplit(., "og|and|&"))() |> # split by and/or
        unlist() |>
        Trim() |>
        (\(.) strsplit(., " "))() |> # split by space
        (\(.) lapply(., \(x) tail(x, 1)))() |> # fetch last elements
        unlist()
      # Check if all author names are in the creators matrix
      if (all(statement %in% GoFish(meta$creators$lastName))) {
        if (nrow(meta$creators) == length(statement)) {
          # order creators matrix by statement order
          meta$creators <- meta$creators |>
            dplyr::arrange(match(meta$creators$lastName, statement))
        }
      }
    }

    # Fetch abstract
    meta$abstractNote <- meta$abstractNote <- ToString(
      GoFish(MARC(marc, 520, "a"), ""),
      "\n"
    )

    # Fetch ISBN
    meta$ISBN <- GoFish(
      ToString(gsub("[^0-9-]", "", MARC(marc, "020", "a")))
    )
    # Fetch ISSN
    issn <- MARC(marc, "022", "a")
    # Fetch language field
    language <- ReadXpath(marc, "//controlfield[@tag='008']", FALSE)
    # Subtract language
    meta$language <- substr(language, 36, 38)
    # Fetch meeting name
    meta$meetingName <- MARC(marc, "111", "a")
    if (is.null(meta$meetingName)) meta$meetingName  <- MARC(marc, "711", "a")

    # Fetch edition
    meta$edition <- EditionFix(MARC(marc, "250", "a", FALSE))

    # Fetch place of publication
    meta$place <- MARC(marc, "260", "a")
    if (is.null(meta$place)) meta$place <- MARC(
      marc, "264", "a")
    # Remove everything after first semicolon if place !is.na
    if (!is.null(meta$place)) meta$place <- Trim(
      gsub("(.*);.*", "\\1", meta$place)
    )
    # Fetch distributor
    meta$distributor <- MARC(marc, "260", "b")
    # Fetch publisher
    meta$publisher <- MARC(marc, "260", "b")
    if (is.null(meta$publisher)) meta$publisher <- MARC(marc, "264", "b")
    # Fetch publication date
    meta$date <- MARC(marc, "260", "c")
    if (is.null(meta$date)) meta$date <- MARC(marc, "264", "c")
    # Set as year
    meta$date <- GoFish(
      as.character(gsub('.*?(\\d{4}).*', '\\1', meta$date)[[1]])
    )

    # Fetch number of pages and force numeric
    meta$numPages <- as.character(MARC(marc, "300", "a"))

    # Fetch series title
    meta$series <- MARC(marc, "490", "a")
    # Fetch series number
    meta$seriesNumber <- MARC(marc, "490", "v")
    if (is.null(meta$series)) meta$series <- MARC(marc, "440", "a")
    if (is.null(meta$seriesNumber)) meta$seriesNumber <- MARC(marc, "440", "v")

    # Fetch tags
    tags <- GoFish(unique(as.character(unlist(
      c(MARC(marc, 650, "a"), MARC(marc, 653, "a"))
    ))))
    if (any(!is.na(tags))) meta$tags <- tibble::tibble(tag = tags)

    # Set accessDate
    meta$accessDate <- as.character(Sys.time())

    # Create zotero-type matrix
    meta <- GoFish(ZoteroFormat(meta), NULL)

    # Remove if no Creator is found
    if (all(is.na(GoFish(meta$creators[[1]])))) meta <- NULL

    return (meta)
  }

  # Fetch MARC21 data from Alma or LOC
  metadata <- lapply(key, \(x) {
    if (!is.null(CheckId(x))) ISBN(CheckId(x), meta)
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
