#' @title Wrangle Cristin metadata into Zotero-type format
#' @description Converts Cristin metadata into a Zotero-style tibble.
#' @param data Tibble containing metadata from Cristin
#' @param meta A list for storing data elements, Default: list()
#' @param use.identifiers Use if ISBN/DOI identifiers if enabled, Default: TRUE
#' @return A Zotero-type tibble.
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @export
CristinWrangler <- \(data, meta = list(), use.identifiers = TRUE) {

  # Define visible bindings to avoid R CMD check warnings
  title <- date_published <- year_published <- firstName <- creatorType <-
    lastName <- filter.name <- external.data <- id <- name <- results <-
    key <- . <- error <- NULL

  # X marks the spot
  x <- data

  # Set key, version and collections if present
  meta$key <- GoFish(x$key, NULL)
  meta$version <- GoFish(x$version, NULL)
  meta$collections <- GoFish(x$collections, NULL)

  # Set item type
  meta$itemType <- unname(x$category)

  # Fetch and clean title
  meta$title <- GoFish(x$title[x$original_language])
  if (is.na(meta$title)) {
    meta$title <- GoFish(x$title[!is.na(x$title)])
  }
  meta$title <- CleanText(meta$title)

  # Get contributors from contributors URL
  httr.get <- Online(
    httr::RETRY("GET",
                sprintf("https://api.cristin.no/v2/results/%s/contributors",
                        x$cristin_result_id),
                quiet = TRUE),
    silent = TRUE,
    message = "Cristin contributors"
  )

  # Create Zotero-type creator matrix if contributors were fetched successfully
  if (!httr.get$error) {
    get.creators <- httr.get$data |> JsonToTibble()

    if (any(nrow(get.creators))) {

      cristin.creators <- lapply(seq_len(nrow(get.creators)), \(i) {
        list(
          type = GoFish(
            get.creators[i, ]$affiliations[[1]]$role_code
          ),
          name = GoFish(
            c(get.creators[i, ]$surname, get.creators[i, ]$first_name)
          )
        )
      })

      meta$creators <- list(ZoteroCreator(cristin.creators))
    }
  }

  # Fetch abstract and additional metadata fields
  meta$abstractNote    <- ToString(
    GoFish(x$summary[, x$original_language], ""),
    "\n"
  )
  meta$thesisType      <- GoFish(x$type)
  meta$institution     <- GoFish(x$publisher$name)
  meta$seriesTitle     <- GoFish(x$series$name)
  meta$reportNumber    <- GoFish(x$issue)
  meta$presentationType <- GoFish(x$type)
  if (is.null(meta$place)) {
    meta$place <- GoFish(x$event$location)
  }
  meta$meetingName     <- GoFish(x$event$name)
  meta$university      <- GoFish(x$publisher$name)

  meta$date <- GoFish(x$date_published)
  if (is.na(meta$date)) {
    meta$date <- GoFish(x$year_published)
  }
  meta$numPages        <- as.character(GoFish(x$number_of_pages))
  meta$language        <- GoFish(x$original_language)
  meta$ISBN            <- GoFish(
    CheckIsbn(x$international_standard_numbers[[1]]$value)
  )

  meta$publisher       <- GoFish(x$publisher$name)
  if (is.null(meta$place)) {
    meta$place <- GoFish(x$publisher$place)
  }
  meta$series          <- GoFish(x$series$name)
  meta$seriesNumber    <- GoFish(x$issue)
  meta$publicationTitle<- GoFish(x$journal$name)
  meta$pages           <- GoFish(
    ToString(x$pages[1:2][!is.na(x$pages[1:2])], "-")
  )
  meta$volume          <- GoFish(x$volume)
  meta$issue           <- GoFish(x$issue)

  # Check for NVI
  nvi <- GoFish(x$nvi)

  # Extract DOI from links
  meta$DOI <- CheckDoi(x$links[[1]][x$links[[1]]$url_type == "DOI", ]$url) |>
    GoFish()

  # Extract URL, preferring certain types
  url_types <- c("FULLTEKST", "SAMMENDRAG", "DATA",
                 "PROSJEKT", "OMTALE", "ARKIV")
  meta$url <- GoFish(
    x$links[[1]]$url[x$links[[1]]$url_type %in% url_types][[1]]
  )
  if (is.na(meta$url)) {
    meta$url <- GoFish(x$links[[1]]$url[[1]])
  }

  # If DOI/ISBN is missing, try scraping the Cristin App
  if (any(is.na(GoFish(meta$DOI))) && (any(is.na(GoFish(meta$ISBN))))) {
    cristin.url <- paste0(
      "https://app.cristin.no/results/show.jsf?id=",
      x$cristin_result_id
    )
    httr.get <- Online(
      httr::RETRY("GET", cristin.url, quiet = TRUE),
      silent = TRUE,
      message = "Cristin external URLs"
    )
    if (!httr.get$error) {
      cristin.urls <- httr.get$data |>
        rvest::read_html() |>
        rvest::html_nodes(".fact-box-section .img-with-text-textbox a") |>
        rvest::html_text() |>
        GoFish(NULL)

      if (any(is.na(GoFish(meta$DOI)))) {
        meta$DOI <- GoFish(CheckDoi(cristin.urls, TRUE), NULL)
      }
      if (any(is.na(GoFish(meta$ISBN)))) {
        meta$ISBN <- GoFish(CheckIsbn(cristin.urls))
      }
    }
  }

  # If DOI is still missing and the URL looks like a DOI, set it as DOI
  if (any(is.na(GoFish(meta$DOI)))) meta$DOI <- CheckDoi(meta$url)
  # Similar of ISBN
  if (any(is.na(GoFish(meta$ISBN)))) {
    meta$ISBN <- GoFish(CheckIsbn(meta$url))
  }

  # Compile tags from scientific disciplines and keywords
  tags <- GoFish(c(
    as.character(unlist(x$classification$scientific_disciplines[[1]]$name)),
    as.character(unlist(x$classification$keywords[[1]]$name))
  ))
  meta$tags <- if (any(!is.na(tags))) tibble::tibble(tag = tags) else NA

  # Append extra information
  if (!is.na(nvi)) {
    meta$extra <- AddAppend(
      sprintf("NVI: %s", nvi),
      meta$extra,
      "\n"
    )
  }
  meta$extra <- AddAppend(
    sprintf("Cristin: %s", x$cristin_result_id),
    meta$extra,
    "\n"
  )

  # Record the access date
  meta$accessDate <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%S%z")

  # Create zotero-type matrix
  meta <- GoFish(ZoteroFormat(meta, fix.columns = FALSE), NULL)

  # if use.identifiers try to enhance metadata using isbn/doi
  if (any(nrow(meta)) && use.identifiers) {
    meta <- ZoteroEnhancer(meta)
  }

  # Fallback: Do a part-of check if it the item is part of (a book) and the
  # Above is not enabled or did not find the book.
  if (any(nrow(meta)) && !is.na(GoFish(x$part_of$url))) {
    if (any(is.na(GoFish(meta$bookTitle))) && meta$itemType == "bookSection") {

      # If the item is part of a whole, fetch the external reference
      external.data <- Cristin(
        id = basename(x$part_of$url), silent = TRUE, force.type = "book"
      )$results |>
        GoFish(NULL)

      # Update metadata
      if (!is.null(external.data)) {
        meta <- ZoteroEnhancer(meta, external.data)
      }
    }
  }

  # Fix mismatch between creatorType and itemType
  if (any(nrow(GoFish(meta$creators[[1]])))) {
    if ("editor" %in% GoFish(meta$creators[[1]]$creatorType)) {
      editor.items <- c("book", "bookSection", "conferencePaper",
                        "dictionaryEntry", "document", "encyclopediaArticle",
                        "journalArticle", "preprint")
      hasBookTitle <- GoFish(meta$bookTitle, FALSE)
      if (meta$itemType == "report" && !hasBookTitle) {
        meta$creators[[1]]$creatorType[
          meta$creators[[1]]$creatorType == "editor"
        ] <- "serieseditor"
      } else if (!(meta$itemType %in% editor.items) && hasBookTitle) {
        meta$itemType <- "bookSection"
      } else if (!(meta$itemType %in% editor.items)) {
        meta$itemType <- "book"
      }
    }
  }

  # Create zotero-type matrix
  meta <- GoFish(ZoteroFormat(meta, check.structure = TRUE), NULL)

}
