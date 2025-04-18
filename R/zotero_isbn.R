#' @title Search libraries using ISBN or MMS ID
#' @description Query libraries using ISBN (or MMS ID) and fetch metadata
#' @param keys Keys to search with (e.g., ISBN or MMS ID)
#' @param meta A list collecting all metadata used to create , Default: list()
#' @param silent c2z is noisy, tell it to be quiet, Default: TRUE
#' @param log A list for storing log elements, Default: list()
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
#'   if (any(nrow(example$data))) {
#'   ZoteroIndex(example$data) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
#'   }
#' }
#' @export
ZoteroIsbn <- \(keys,
                meta = list(),
                silent = TRUE,
                log = list()) {

  # Visible bindings
  data <- log.eta <- NULL

  # Function to fetch metadata from ALMA/LoC using ISBN or MMS id
  Isbn <- \(key, meta = list(), log = list()) {

    # Visible bindings
    metadata <- NULL

    # Set key as MMS if firs two key digits are 99 else use ISBN
    query.type <- if (substr(key, 1, 2) == "99") "mms_id" else "isbn"

    # Query alma for metadata
    httr.get <- Online(
      httr::RETRY(
        "GET",
        "https://api.bibs.aws.unit.no/alma",
        query = stats::setNames(list(key), query.type),
        quiet = TRUE
      ),
      silent = TRUE,
      message = "Alma (47BIBSYS)",
      log = log
    )

    # Get data if no error
    if (!httr.get$error) {

      # Parse content as text
      metadata <- ParseUrl(httr.get$data, "text")

      # Run if data is not empty
      if (!is.null(metadata)) {

        # Format data from JSON
        marc.json <- jsonlite::fromJSON(metadata)
        # Find Marc id
        id <- unique(marc.json$id)
        # Determine number of ids (should be one, but you know...)
        n.id <- length(marc.json$id)

        if (n.id > 1) marc.json <- marc.json[
          which(as.numeric(id) == min(as.numeric(id))),
        ]

        # MARCXML metadata
        metadata <- list(rvest::read_html(marc.json$xmlPresentation))

        if (is.null(metadata)) return (list(log = httr.get$log))

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
      httr.get <- Online(
        httr::RETRY(
          "GET",
          "http://lx2.loc.gov:210/LCDB",
          query = query,
          quiet = TRUE
        ),
        silent = TRUE,
        message = "Library of Congress",
        log = httr.get$log
      )

      # Get data if no error
      if (!httr.get$error) {

        # MARCXML metadata
        metadata <- list(rvest::read_html(httr.get$data)) |>
          GoFish()

        if (is.null(metadata)) return (list(log = httr.get$log))

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
      metadata <- ReadMarc(metadata[[1]], meta)
    }

    return (list(data = metadata, log = httr.get$log))
  }

  # Function to read Marc XML
  ReadMarc <- \(marc, meta) {

    # Function to convert XML fields
    Marc <- \(marc, tag, code = NULL, clean.text = TRUE, collapse = FALSE) {

      # Run if extracting specific code tag within tags
      if (!is.null(code)) {

        # Define path
        x.path <- sprintf(
          "//datafield[@tag='%s']//subfield[@code='%s']",
          tag, code
        )

        # Extract text from specified tag
        field <- ReadXpath(marc, x.path, clean.text, collapse = collapse)

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

    # Find leader
    leader <- ReadXpath(marc, "//leader", FALSE)

    # Return NULL if empty
    if (all(is.na(leader))) {
      return (list(data = NULL, log = log))
    }

    # Subtract type of document from leader
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
      Marc(marc, "245", "a"), " \u2013 ")
    # Fetch subtitle
    subtitle <- Marc(marc, "245", "b")
    # Combine title and subtitle if subtitle exist
    if (length(subtitle)) meta$title <- paste0(meta$title,": ", subtitle)

    # Fetch part number if part of a volume
    part.number <- GoFish(Marc(marc, "245", "n")[[1]])
    # Fetch volume name
    part.name <- GoFish(ToString(Marc(marc, "245", "p"), " \u2013 "))

    # Define volume title if part number and part name exists
    if (!is.na(part.number)) {
      if (!is.na(part.name)) part.number <- paste0(part.number, ": ", part.name)
      meta$title <- sprintf("%s %s", meta$title, part.number)
    }

    # Fetch creator
    creator <- FormatCreator(Marc(marc, "100"))
    # Fetch contributors
    creators <- Marc(marc, "700")

    # Set contributors as editors if no first author exists
    if (is.null(creator) & (!is.null(creators))) {
      if (!"4" %in% names(creators)) creators[,"4"] <- "edt"
    }

    # Create zotero-type creator matrix if creators exists
    if (!is.null(creators)) {
      creators <- lapply(seq_len(nrow(creators)), \(i) {
        FormatCreator(creators[i, ])
      }) |>
        dplyr::bind_rows()
      # Remove initial creator if all creators are editors
      if (identical(GoFish(unique(creators$creatorType)), "editor")) {
        creator <- NULL
      }
    }

    # Combine creator and contributors matrices
    meta$creators <- dplyr::bind_rows(creator, creators) |>
      dplyr::distinct()

    # Check statement of responsibility field
    statement <- Marc(marc, "245", "c")
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
    meta$abstractNote <- ToString(
      GoFish(Marc(marc, 520, "a", collapse = TRUE), ""),
      "\n"
    )

    # Fetch ISBN
    meta$ISBN <- GoFish(CheckIsbn(Marc(marc, "020", "a")))
    # Fetch ISSN
    issn <- Marc(marc, "022", "a")
    # Fetch language field
    language <- ReadXpath(marc, "//controlfield[@tag='008']", FALSE)
    # Subtract language
    meta$language <- substr(language, 36, 38)
    # Fetch meeting name
    meta$meetingName <- Marc(marc, "111", "a")
    if (is.null(meta$meetingName)) meta$meetingName  <- Marc(marc, "711", "a")

    # Fetch edition
    meta$edition <- FixEdition(Marc(marc, "250", "a", FALSE))

    # Fetch place of publication
    meta$place <- Marc(marc, "260", "a")
    if (is.null(meta$place)) meta$place <- Marc(
      marc, "264", "a")
    # Remove everything after first semicolon if place !is.na
    if (!is.null(meta$place)) meta$place <- Trim(
      gsub("(.*);.*", "\\1", meta$place)
    )
    # Fetch distributor
    meta$distributor <- Marc(marc, "260", "b")
    # Fetch publisher
    meta$publisher <- Marc(marc, "260", "b")
    if (is.null(meta$publisher)) meta$publisher <- Marc(marc, "264", "b")
    # Fetch publication date
    meta$date <- Marc(marc, "260", "c")
    if (is.null(meta$date)) meta$date <- Marc(marc, "264", "c")
    # Set as year
    meta$date <- GoFish(
      as.character(gsub('.*?(\\d{4}).*', '\\1', meta$date)[[1]])
    )

    # Fetch number of pages and force numeric
    meta$numPages <- as.character(Marc(marc, "300", "a"))

    # Fetch series title
    meta$series <- Marc(marc, "490", "a")
    # Fetch series number
    meta$seriesNumber <- Marc(marc, "490", "v")
    if (is.null(meta$series)) meta$series <- Marc(marc, "440", "a")
    if (is.null(meta$seriesNumber)) meta$seriesNumber <- Marc(marc, "440", "v")

    # Fetch tags
    tags <- GoFish(unique(as.character(unlist(
      c(Marc(marc, 650, "a"), Marc(marc, 653, "a"))
    ))))
    if (any(!is.na(tags))) meta$tags <- tibble::tibble(tag = tags)

    # Set accessDate
    meta$accessDate <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%S%z")

    # Clean Abstract
    meta$abstractNote <- CleanText(meta$abstractNote)

    # Create zotero-type matrix
    meta <- GoFish(ZoteroFormat(meta), NULL)

    # Remove if no Creator is found
    if (all(is.na(GoFish(meta$creators[[1]])))) meta <- NULL

    return (meta)
  }



  # Start time for query
  query.start <- Sys.time()

  # Cycle through queries
  ## Should perhaps vectorize, but for loop seems more informative tbh
  for (i in seq_along(keys)) {

    # Check key
    key <- CheckIsbn(keys[[i]])

    # Skip if not valid key
    if (is.null(key)) {
      log <- append(log, sprintf("Key: `%s` is not valid", keys[[i]]))
      next
    }

    # Check libraries for metadata
    metadata <- Isbn(key)

    if (any(is.na(GoFish(metadata$title))) ||
        any(is.na(GoFish(metadata$creators)))) {
      # Check libraries for metadata
      metadata <- Isbn(key)
    }

    # Add data
    data <- AddAppend(metadata$data, data)

    # Append log
    log <- append(log, metadata$log)

    # Estimate time of arrival
    log.eta <- LogCat(
      Eta(query.start, i, length(keys)),
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


#' Check and Convert ISBN
#'
#' This function processes an input string that contains an ISBN (or a compound string
#' where the first element is the ISBN). It first removes unwanted characters (keeping only digits, commas, and spaces),
#' then uses the \code{GoFish} function to further process the string. The function splits the cleaned string by spaces or commas
#' to extract a candidate ISBN. If the candidate ISBN is in ISBN10 format, it is converted to ISBN13 by prepending "978",
#' recalculating the check digit, and concatenating the result. Finally, the function validates that the resulting ISBN is a valid ISBN13,
#' i.e., it is 13 digits long and starts with either "978" or "979".
#'
#' @param key A character string that contains an ISBN or a compound string where the first element is the ISBN.
#'
#' @return A cleaned and validated ISBN string. If the resulting ISBN does not meet the expected criteria,
#'   the function returns \code{NULL}.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Cleans the input \code{key} by removing all characters except digits, commas, and spaces.
#'   \item Processes the cleaned key using the \code{GoFish} function (which must be defined in your environment).
#'   \item Splits the resulting string by spaces and commas to extract the first candidate ISBN.
#'   \item If the candidate is in ISBN10 format (10 characters), it converts it to ISBN13 by:
#'         \enumerate{
#'           \item Converting the first 9 digits to numeric.
#'           \item Prepending the ISBN13 prefix "978".
#'           \item Calculating the ISBN13 check digit using alternating weights of 1 and 3.
#'           \item Concatenating the digits to form the full ISBN13.
#'         }
#'   \item Validates that the final key is a valid ISBN13 (i.e., 13 digits long and starting with "978" or "979").
#' }
#'
#' @examples
#' \dontrun{
#' # Example with a compound string where the first part is the ISBN:
#' isbn_input <- "316148410X, some extra text"
#' valid_isbn <- CheckIsbn(isbn_input)
#'
#' # Example converting an ISBN10 to ISBN13:
#' isbn10 <- "316148410X"
#' valid_isbn <- CheckIsbn(isbn10)
#' }
#'
#' @export
CheckIsbn <- function(key) {

  # Helper function to validate and, if needed, convert an ISBN candidate.
  ValidIsbn <- function(key) {
    valid <- FALSE
    # If the candidate is ISBN10 (10 characters), convert to ISBN13.
    if (nchar(key) == 10) {
      # Convert first 9 characters to numeric.
      digits <- as.numeric(strsplit(key, "")[[1]][1:9])
      if (any(is.na(digits))) return(FALSE)
      # Prepend the ISBN13 prefix "978".
      isbn13_digits <- c(9, 7, 8, digits)
      # Calculate the ISBN13 check digit using alternating weights of 1 and 3.
      weights <- rep(c(1, 3), length.out = 12)
      sum_val <- sum(isbn13_digits * weights)
      check <- (10 - (sum_val %% 10)) %% 10
      # Reconstruct the ISBN13 key.
      key <- paste0(paste(isbn13_digits, collapse = ""), check)
    }
    # Validate that the key is a valid ISBN13: 13 digits long and starting with "978" or "979".
    if (nchar(key) == 13 && grepl("^(978|979)", key)) {
      valid <- TRUE
    }
    return(valid)
  }

  # Remove all characters except digits, commas, and spaces.
  key <- gsub("[^0-9, ]", "", key) |>
    GoFish(type = NULL)

  if (is.null(key)) return(NULL)

  # Attempt extraction using space separation.
  space_key <- GoFish(TrimSplit(key, " ")[1], FALSE)
  if (ValidIsbn(space_key)) return(space_key)

  # Attempt extraction using comma separation.
  comma_key <- GoFish(TrimSplit(key, ",")[1], FALSE)
  if (ValidIsbn(comma_key)) return(comma_key)

  return(NULL)
}
