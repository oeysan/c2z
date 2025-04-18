#' @title Wrangle CrossRef metadata into Zotero-type format
#' @description Query CrossRef by DOI and fetch metadata
#' @param data XML data from CrossRef containing metadata
#' @param meta A list collecting all metadata used to create , Default: list()
#' @param use.json Use either json (TRUE) or XML (FALSE), Default: TRUE
#' @param silent c2z is noisy, tell it to be quiet, Default: TRUE
#' @param log A list for storing log elements, Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # `DoiCrossref` is called from `ZoteroDoi` (if item found)
#'   example <- ZoteroDoi("10.1126/sciadv.abd1705")
#'
#'   # Print index using `ZoteroIndex`
#'   if (!is.null(example$data)) {
#'     ZoteroIndex(example$data) |>
#'       dplyr::select(name) |>
#'       print(width = 80)
#'
#'     # Display catalog
#'     example$data$libraryCatalog
#'   }
#' }
#' @rdname DoiCrossref
#' @export
DoiCrossref <- \(data,
                 meta = list(),
                 use.json = TRUE,
                 silent = TRUE,
                 log = list()) {

################################################################################
################################################################################
#################################JSON Version###################################
################################################################################
################################################################################

  if (use.json) {

    # Parse the input JSON into a list
    crossref <- jsonlite::fromJSON(ParseUrl(data, "text")) |>
      GoFish(NULL)
    if (is.null(crossref)) return (list(data = NULL, log = log))
    msg <- crossref$message

    # Map the Crossref "type" to a Zotero itemType.
    if (msg$type == "journal-article") {
      itemType <- "journalArticle"
    } else if (msg$type == "proceedings-article") {
      itemType <- "conferencePaper"
    } else if (msg$type == "posted-content" &&
               !is.null(msg$subtype) && msg$subtype == "preprint") {
      itemType <- "preprint"
    } else if (msg$type == "book-chapter" ||
               msg$type == "book-section" ||
               msg$type == "book-part"
               ) {
      itemType <- "bookSection"
    } else if (msg$type == "edited-book" ||
               msg$type == "book" ||
               msg$type == "monograph") {
      itemType <- "book"
    } else {
      itemType <- "document"
    }

    # Initialize meta list to store Zotero fields.
    meta$itemType <- itemType

    # Title (first element if multiple exist)
    meta$title  <- GoFish(msg$title[[1]])
    # Combine title and subtitle if subtitle exist
    subtitle <- GoFish(msg$subtitle[[1]])
    if (!any(is.na(subtitle))) {
      # Set short-title
      meta$shortTitle <- meta$title
      meta$title <- paste0(meta$title,": ", subtitle)
    }

    # Initialize creators as an empty list.
    creators <- list()
    # Define the contributor roles to process.
    roles <- c("author", "editor", "translator")
    for (role in roles) {
      current.role <- msg[[role]]
      if (any(nrow(current.role))) {
        for (row in seq_len(nrow(current.role))) {
          names <- current.role[row, ]
          # Combine names
          name <- c(names$family, names$given)
          new.names <- list(type = role, name = name)
          creators <- c(creators, list(new.names))
        }
      } else if (is.list(current.role)) {
        for (el in seq_along(current.role)) {
          names <- current.role[[el]]
          new.names <- list(type = role, name = name)
          creators <- c(creators, list(new.names))
        }
      }
    }

    # If item is not a book chapter remove authors if editors are present.
    if (itemType != "bookSection") {
      has.editor <- any(sapply(creators, \(x) x$type == "editor"))
      if (has.editor) {
        # Remove creators with type "author"
        creators <- Filter(\(x) x$type != "author", creators)
      }
    } else {
      # Book title
      meta$bookTitle <- GoFish(msg$`container-title`[[1]])
    }

    # Create a Zotero-type creator tibble
    meta$creators <- ZoteroCreator(creators)

    # Publication title and journal abbreviation.
    meta$publicationTitle    <- GoFish(msg$`container-title`[1])
    meta$journalAbbreviation <- GoFish(msg$`short-container-title`[1])

    # Volume, issue, and pages.
    meta$volume <- GoFish(msg$volume)
    meta$issue  <- GoFish(msg$issue)
    meta$pages  <- GoFish(msg$page)

    # Construct a publication date from available date parts.
    dateParts <- GoFish(msg$`published-print`$`date-parts`)
    if (any(is.na(dateParts))) dateParts <- GoFish(msg$issued$`date-parts`)
    if (any(is.na(dateParts))) dateParts <- GoFish(msg$posted$`date-parts`)
    if (any(!is.na(dateParts))) {
      year <- GoFish(dateParts[1], NULL)
      month <- GoFish(dateParts[2], NULL)
      day <- GoFish(dateParts[3], NULL)
      meta$date <- paste(c(year, month, day), collapse="-")
      if (meta$date == "") date <- NA
    }

    # Other common fields.
    meta$DOI       <- GoFish(msg$DOI)
    meta$ISSN      <- GoFish(paste(msg$ISSN, collapse = ", "))
    meta$ISBN      <- GoFish(CheckIsbn(msg$ISBN))
    meta$url       <- GoFish(msg$URL)
    meta$language  <- GoFish(msg$language)
    meta$publisher <- GoFish(msg$publisher)

    # Abstrac
    meta$abstractNote <- GoFish(msg$abstract)

    # Additional fields.
    meta$series       <- GoFish(msg$series)
    meta$shortTitle   <- GoFish(msg$`short-title`[1])
    meta$edition      <- GoFish(FixEdition(msg$edition))
    if (any(is.na(meta$edition))) {
      meta$edition    <- GoFish(FixEdition(msg$`edition-number`))
    }
    meta$place          <- GoFish(msg$`publisher-location`)
    meta$conferenceName <- GoFish(msg$event$name)
    meta$notes          <- GoFish(msg$notes, list())
    meta$tags           <- GoFish(msg$tags, list())
    meta$repository <- GoFish(msg$`group-title`)

    # Return the meta list
    return (list(data = meta, log = log))

  }
################################################################################
################################################################################
##################################XML Version###################################
################################################################################
################################################################################

  # Function to extract date from Crossref
  CrossrefDate <- \(ref, type, date.type = "/publication_date") {

    year <- paste0(type, date.type, "/year")
    month <- paste0(type, date.type, "/month")
    day <- paste0(type, date.type, "/day")

    if (length(ReadXpath(ref, year))==1) {

      year <- ReadXpath(ref, year)
      month <- ReadXpath(ref, month)
      day <- ReadXpath(ref, day)

    } else {

      year <- ReadXpath(
        ref, paste0(type, date.type, "[@media_type='print']/year")
      )
      month <- ReadXpath(
        ref, paste0(type, date.type, "[@media_type='print']/month")
      )
      day <- ReadXpath(
        ref, paste0(type, date.type, "[@media_type='print']/day")
      )

      if (!length(year)) {

        year <- ReadXpath(
          ref, paste0(type, date.type, "[@media_type='online']/year")
        )
        month <- ReadXpath(
          ref, paste0(type, date.type, "[@media_type='online']/month")
        )
        day <- ReadXpath(
          ref, paste0(type, date.type, "[@media_type='online']/day")
        )

      }

    }

    date <- paste(c(year, month, day), collapse="-")
    if (date == "") date <- NA

    return (date)

  }

  # Function to extract creators from Crossref
  CrossRefCreator <- \(data, xpath, type = NULL) {

    # Find creator nodes
    creator <- data |>
      rvest::html_nodes(xpath = xpath)

    # Loop through creators and combine into data.frame
    creators <- lapply(creator, \(x) {

      # Use contrtibutor_role from reference if type is not defined
      if (is.null(type)) {
        type <- x |>
          rvest::html_attr("contributor_role")
      }

      # Fetch last name
      last.name <- x |>
        rvest::html_nodes(xpath = "surname[not(parent::alt-name)]") |>
        rvest::html_text()
      # Fetch first name
      first.name <- x |>
        rvest::html_nodes(xpath = "given_name[not(parent::alt-name)]") |>
        rvest::html_text()

      # Combine names
      name <- c(last.name, first.name)

      # Some authors are only named by surname/name
      if (length(name) == 1) {
        # Sometimes surnames incorrectly contains both family and given name
        ## Try to correct by splitting any double names in surname
        if (grepl("\\s+", name))  name <- strsplit(name, "\\s+")[[1]]
        # Else fetch organization name if any
      } else if (!length(name)) {
        name <- x |> rvest::html_text()
      }

      # Return creators
      list(type = type, name = name)

    })

    # Return zotero matrix
    ZoteroCreator(creators)

  }

  # Find crossref metadata
  ref <- rvest::read_html(data) |>
    rvest::html_nodes(xpath = "//doi_record//crossref") |>
    rvest::html_children() |>
    GoFish(NULL)

  # Return NULL is data is empty
  if (is.null(ref)) return (list(data = NULL, log = log))

  # Type of metadata (e.g., journal, book)
  ref.type <- ref |>
    rvest::html_name()
  # Fetch sub type
  sub.type <- ref |>
    rvest::html_attrs() |>
    unlist() |>
    (\(x) {
      if (!length(x)) FALSE else x[grep("type", names(x))]
    } )()

  # Fetch pages
  meta$pages <- GoFish(ToString(
    c(ReadXpath(ref, "//pages//first_page"),
      ReadXpath(ref, "//pages//last_page")),
    "-"
  ))

  # Return NULL if empty
  if (all(is.na(ref.type))) {
    return (list(data = NULL, log = log))
  }

  # Do if ref.type is a journal article
  if ((ref.type == "journal")) {

    # Set itemType
    meta$itemType <- "journalArticle"
    # Fetch abstract
    meta$abstractNote <- ReadXpath(
      ref, "//journal_article//abstract", collapse = TRUE
    )
    # Fetch title
    meta$title <- ReadXpath(ref, "//journal_article/titles/title")
    # Fetch subtitle
    subtitle <- ReadXpath(ref, "//journal_article/titles/subtitle")
    # Combine title and subtitle if subtitle exist
    if (length(subtitle)) {
      # Set short-title
      meta$shortTitle <- meta$title
      meta$title <- paste0(meta$title,": ", subtitle)
    }
    # Fetch date
    meta$date <- CrossrefDate(ref, "//journal_issue")
    if (is.na(meta$date)) meta$date <- CrossrefDate(
      ref, "//journal_article"
    )
    # Fetch creators
    meta$creators <- CrossRefCreator(ref, "//contributors/person_name")
    # Check if article has an organization field
    if (length(ReadXpath(ref, "//contributors/organization"))) {
      meta$creators <- AddAppend(
        CrossRefCreator(ref, "//contributors/organization"),
        meta$creators)
    }
    # Fetch issue
    meta$issue <- ReadXpath(ref, "//journal_issue//issue")
    # Fetch volume
    meta$volume <- ReadXpath(ref, "//journal_issue//volume")
    # Fetch publication title
    meta$publicationTitle <- ReadXpath(
      ref, "//journal_metadata/full_title",

      first = TRUE
    )
    # Fecth short title for journal
    meta$journalAbbreviation <- ReadXpath(
      ref, "//journal_metadata/abbrev_title",
      first = TRUE
    )

    # Fetch language
    meta$language <- ReadAttr(ref, "journal_metadata", "language")
    # Fetch ISSN
    meta$ISSN <- paste(
      ReadXpath(ref, "journal_metadata/issn", FALSE),
      collapse = ", "
    )
    # Fetch DOI
    meta$DOI <- ReadXpath(
      ref, "//journal_article/doi_data/doi", FALSE
    )
    # Fetch URL
    meta$url <- ReadXpath(
      ref, "//journal_article/doi_data/resource", FALSE
    )

    # Else if ref.type is a book
  } else if (ref.type == "book") {

    if (grepl("book_metadata", ref)) {
      book.metadata <- "//book_metadata"
    } else {
      book.metadata <- "//book_series_metadata"
    }
    # Fetch abstract
    meta$abstractNote <- ReadXpath(
      ref, paste0(book.metadata,"//abstract"), collapse = TRUE
    )
    # Set itemType
    meta$itemType <- ref.type

    # Fetch title
    meta$title <- ReadXpath(
      ref, paste0(book.metadata,"/titles[1]/title[1]")
    )
    # Fetch subtitle
    subtitle <-  ReadXpath(
      ref,paste0(book.metadata,"/titles[1]/subtitle[1]")
    )
    # Combine title and subtitle if subtitle exist
    if (length(subtitle)) {
      meta$shortTitle <- meta$title
      meta$title <- paste0(meta$title,": ", subtitle)
    }
    # Fetch publisher
    meta$publisher <- ReadXpath(
      ref, paste0(book.metadata,"//publisher_name"
      ))
    # Fetch place
    meta$place <- ReadXpath(
      ref, paste0(book.metadata,"//publisher_place")
    )
    # Remove everything after first semicolon if place !is.na
    if (!is.null(meta$place)) meta$place <- Trim(
      gsub("(.*);.*", "\\1", meta$place)
    )
    # Fetch date
    meta$date <- CrossrefDate(ref, book.metadata)
    # Fetch DOI
    doi <- ReadXpath(
      ref, paste0(book.metadata,"/doi_data/doi"), FALSE
    )
    if (length(doi)) {
      meta$extra <- AddAppend(
        paste0("DOI: ",doi), meta$extra, "\n"
      )
    }
    # Fetch ISBN
    meta$ISBN <- CheckIsbn(ReadXpath(ref, "//isbn"))
    # Fetch URL
    meta$url <- ReadXpath(
      ref, paste0(book.metadata,"/doi_data/resource[1]"), FALSE)
    # Fetch language
    meta$language <- ReadAttr(ref, book.metadata, "language")
    # Fetch edition
    meta$edition <- FixEdition(
      ReadXpath(ref, paste0(book.metadata,"//edition_number"), FALSE
      ))
    # Check if there are series editors
    series.editors <- "//series_metadata//person_name"
    if (length(ReadXpath(ref, series.editors))) {
      meta$creators <- CrossRefCreator(ref, series.editors, "seriesEditor")
    }
    # Set volume
    meta$volume <- ReadXpath(ref, "//book_series_metadata/volume")
    # Fetch series title
    meta$series <- ReadXpath(
      ref, "//series_metadata/titles[1]/title[1]"
    )
    # Fetch subtitle
    subtitle <- ReadXpath(
      ref, "//series_metadata/titles[1]/subtitle[1]"
    )
    # Combine title and subtitle if subtitle exist
    if (length(subtitle)) meta$series <- paste0(meta$series,": ", subtitle)

    # Use only editors if other creator types exists in metadata
    book.creators <- paste0(
      book.metadata,"/contributors/person_name[@contributor_role='editor']"
    )
    # Set creators as editors
    creator.types <- "editor"

    # Use listed creators if no editors are found
    if (!length(ReadXpath(ref, book.creators))) {
      book.creators <- paste0(book.metadata,"/contributors/person_name")
      creator.types <- ReadAttr(
        ref,
        paste0(book.metadata,"/contributors/person_name"),
        "contributor_role"
      )
    }

    # Check if reference contains content_item (i.e. is a chapter)
    book.chapter <- ref |> rvest::html_nodes(xpath = "//content_item")

    # Set all creators as editors if reference is a chapter or edited book
    if (sub.type[["book_type"]] == "edited_book" |
        sub.type[["book_type"]] == "reference" |
        length(book.chapter)) {
      creator.types <- "editor"
    }

    # Add or append book creators
    meta$creators <- AddAppend(
      CrossRefCreator(ref, book.creators, creator.types),
      meta$creators
    )

    # if book.chapter exists set itemType as book chapter
    if (length(book.chapter)) {

      # Change itemType to book chapter
      meta$itemType <- "bookSection"

      # Set book title
      meta$bookTitle <- meta$title

      # Find chapter title
      meta$title <- ReadXpath(
        ref, "//content_item/titles[1]/title[1]"
      )
      # Fetch subtitle
      subtitle <- ReadXpath(
        ref, "//content_item/titles[1]/subtitle[1]"
      )
      # Combine title and subtitle if subtitle exist
      if (length(subtitle)) {
        meta$title <- paste0(meta$title,": ", subtitle)
      }

      # Append creators of chapter
      meta$creators <- AddAppend(
        CrossRefCreator(ref, "//content_item//person_name"),
        meta$creators
      )

      date <-  CrossrefDate(ref, "//content_item")
      if (!is.na(date)) meta$date <- date

      # Fetch chapter language
      language <- ReadAttr(ref, "//content_item", "language")
      if (length(language)) meta$language <- language

      # Fetch chapter DOI
      doi <- ReadXpath(ref, "//content_item/doi_data/doi[1]", FALSE)
      if (length(doi)) {
        meta$extra <- AddAppend(
          paste0("DOI: ",doi), meta$extra, "\n"
        )
      }

      # Fetch chapter url
      url <- ReadXpath(
        ref, "//content_item/doi_data/resource[1]", FALSE
      )
      if (length(url)) meta$url <- url

      # Fetch abstract
      meta$abstractNote <- ReadXpath(
        ref, "//content_item//abstract", collapse = TRUE
      )

    } else {
      # Set book pages if reference is book and not a chapter
      if (length(meta$pages)) meta$numPages <- as.character(meta$pages)
    }

    # Else if ref.type is a conference paper
  } else if (ref.type == "conference") {

    meta$itemType <- "conferencePaper"

    # Fetch title
    meta$title <- ReadXpath(ref, "//conference_paper/titles/title")
    # Fetch subtitle
    subtitle <- ReadXpath(ref, "//conference_paper/titles/subtitle")
    # Combine title and subtitle if subtitle exist
    if (length(subtitle)) {
      # Set short-title
      meta$shortTitle <- meta$title
      meta$title <- paste0(meta$title,": ", subtitle)
    }

    # Fetch abstract
    meta$abstractNote <- ReadXpath(
      ref, "//conference_paper//abstract", collapse = TRUE
    )

    meta$creators <- CrossRefCreator(ref, "//contributors/person_name")

    # Check if article has an organization field
    if (length(ReadXpath(ref, "//contributors/organization"))) {
      meta$creators <- AddAppend(
        CrossRefCreator(ref, "//contributors/organization"),
        meta$creators)
    }

    meta$date <- CrossrefDate(ref, "conference_paper")
    if (is.na(meta$date)) meta$date <- CrossrefDate(
      ref, "//proceedings_metadata"
    )

    meta$publisher <- ReadXpath(
      ref, "//proceedings_metadata//publisher_name"
    )

    meta$ISBN <- CheckIsbn(
      ReadXpath(ref, "//proceedings_metadata/isbn")
    )

    meta$conferenceName <- ReadXpath(
      ref, "//event_metadata/conference_name"
    )

    meta$proceedingsTitle <- ReadXpath(
      ref, "//proceedings_metadata/proceedings_title"
    )


    meta$DOI <- ToString(
      ReadXpath(ref, "//conference_paper/doi_data/doi", FALSE)
    )
    meta$url <- ToString(
      ReadXpath(ref, "//conference_paper/doi_data/resource", FALSE)
    )

  } else if (ref.type == "posted_content" & sub.type == "preprint") {

    # Set itemtype as preprint
    meta$itemType <- meta$genre <- "preprint"
    # Fetch title
    meta$title <- ReadXpath(ref, "//posted_content/titles/title")
    # Fetch subtitle
    subtitle <- ReadXpath(ref, "//posted_content/titles/subtitle")
    # Combine title and subtitle if subtitle exist
    if (length(subtitle)) {
      # Set short-title
      meta$shortTitle <- meta$title
      meta$title <- paste0(meta$title,": ", subtitle)
    }
    # Fetch creators
    meta$creators <- CrossRefCreator(ref, "//contributors/person_name")
    # Fetch data
    meta$date <- CrossrefDate(ref, "//posted_content", "/posted_date")
    # Fetch abstract
    meta$abstractNote <- ReadXpath(
      ref, "//posted_content//abstract", collapse = TRUE
    )
    # Fetch repository
    meta$repository <- ReadXpath(ref, "//posted_content/group_title")
    # Fetch DOI
    meta$DOI <- ToString(
      ReadXpath(ref, "//posted_content/doi_data/doi", FALSE)
    )
    # Fetch URL
    meta$url <- ToString(
      ReadXpath(ref, "//posted_content/doi_data/resource", FALSE)
    )

    # Fetch identifier of published paper if any
    preprint.of <- ReadAttr(
      ref, "//posted_content//intra_work_relation", "relationship-type"
    )
    if (any(!is.na(preprint.of))) {
      published.paper <- ReadXpath(
        ref, "//posted_content//intra_work_relation", FALSE
      ) |> dplyr::nth(max(0, grep("isPreprintOf", preprint.of)))

      if (!is.na(published.paper)) {

        meta$extra <- AddAppend(
          paste0("Published work: ",
                 published.paper), meta$extra, "\n"
        )
      }
    }

    # Else itemType not supported
  } else {

    meta$itemType <- NULL

  } # end of itemTypes

  return (list(data = meta, log = log))

}
