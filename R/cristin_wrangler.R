#' @title Wrangle Cristin metadata into Zotero-type format
#' @description The little engine that could, at least try, to wrangle metadata
#'   from Cristin into a Zotero-type matrix. References can be augmented, or
#'   replaced with metadata gathered through ISBN or DOI identifiers, and
#'   through Crossref queries if all other fails. Or, hope the best and place
#'   your misguided faith in the ability of researchers to correctly register
#'   their own publications.
#' @param data Tibble containing metadata from Cristin
#' @param use.identifiers Use if ISBN/DOI identifiers if enabled, Default: TRUE
#' @param crossref.search Query Crossref database based on title, authors, and
#'   date if enabled, Default: FALSE
#' @param autosearch Results could be automatically evaluated (based on some
#'   logic) or you could inspect them manually if set to FALSE, Default: TRUE
#' @param override Put your faith in the algorithms and the identifiers (i.e.,
#'   DOI/ISBN) and override what is reported in Cristin, Default: FALSE
#' @param silent Running silent, running deep, Default: FALSE
#' @param polite Please store you email in `.Renviron` to query Crossref,
#'   Default: TRUE
#' @param log A list for storing log elements, Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Simple `Cristin` search by id with import set as FALSE
#'   example <- Cristin(id = "840998", zotero.import = FALSE)
#'   # Check if results i supported using `CristinSupported`
#'   example.supported <- CristinSupported(example$results)
#'   # Use `ZoteroIndex` to print `CristinWrangler`
#'   ZoteroIndex(CristinWrangler(example.supported$data)$results) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
#' }
#' @seealso
#'  \code{\link[tidyr]{unnest}}, {\link[tidyr]{nest}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{across}},
#'  \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{bind}},
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[tibble]{tibble}}
#' @rdname CristinWrangler
#' @export
CristinWrangler <- \(data,
                     use.identifiers = TRUE,
                     crossref.search = FALSE,
                     autosearch = TRUE,
                     override = FALSE,
                     silent = FALSE,
                     polite = TRUE,
                     log = list()) {

  # Visible bindings
  title <- date_published <- year_published <- firstName <- creatorType <-
    lastName <- filter.name <- external.data <- results <- id <- name <-
    key <- . <- NULL

  # Converting to Zotero-type format
  log <-  LogCat(
    sprintf(
      "Converting %s from Cristin to Zotero format" ,
      Pluralis(nrow(data), "reference", "references")
    ),
    silent = silent,
    log = log
  )

  # List of initial states
  initial.state <- list(
    external.data = external.data,
    crossref.search = crossref.search
  )

  # Start time for query
  query.start <- Sys.time()

  # Cycle through references
  ## Should perhaps vectorize, but for loop seems more informative tbh
  for (i in seq_len(nrow(data))) {

    # Define data
    x <- data[i,]
    meta <- list()
    external.data <- initial.state$external.data
    crossref.search <- initial.state$crossref.search

    # Check if key exist
    if (!any(is.na(GoFish(x$key)))) {
      meta$key <- x$key
      meta$version <- x$version
      meta$collections <- GoFish(unlist(x$collections),NULL)
    }

    # Set item type
    meta$itemType <- x$category

    # Fetch title
    meta$title <- GoFish(x$title[x$original_language])
    if (is.na(meta$title)) meta$title <- GoFish(x$title[!is.na(x$title)])
    meta$title <- CleanText(meta$title)

    # Get contributors from contributors url
    ## Cristin has for some reason placed them in a different table
    get.creators <- JsonToTibble(
      httr::RETRY("GET", sprintf(
        "https://api.cristin.no/v2/results/%s/contributors",
        x$cristin_result_id), quiet = TRUE)
    )

    # Create zotero type creator matrix
    if (nrow(get.creators) > 0) {
      meta$creators <- ZoteroCreator(
        lapply(seq_len(nrow(get.creators)), \(i) {
          list(type = GoFish(
            get.creators[i, ]$affiliations[[1]]$role_code
          ),
          name = GoFish(c(
            get.creators[i,]$surname,
            get.creators[i,]$first_name
          )))
        })
      )
    } else {
      meta$creators <- NA
    }

    # Fetch abstract
    meta$abstractNote <- ToString(
      GoFish(x$summary[, x$original_language],""),
      "\n"
    )
    # Fetch type of thesis
    meta$thesisType <- GoFish(x$type)
    # Fetch institution
    meta$institution <- GoFish(x$publisher$name)
    # Fetch series title
    meta$seriesTitle <- GoFish(x$series$name)
    # Fetch report number
    meta$reportNumber <- GoFish(x$issue)
    # Fetch presentation type
    meta$presentationType <- GoFish(x$type)
    # Fetch presentation place
    if (is.null(meta$place)) meta$place <- GoFish(x$event$location)
    # Fetch presentation type/name/event
    meta$meetingName <- GoFish(x$event$name)
    # Fetch university
    meta$university <- GoFish(x$publisher$name)
    # Fetch data
    meta$date <- GoFish(x$date_published)
    if(is.na(meta$date)) meta$date <- GoFish(x$year_published)
    # Fetch number of pages
    meta$numPages <- as.character(GoFish(x$number_of_pages))
    # Fetch language
    meta$language <- x$original_language
    # Fetch ISBN
    meta$ISBN <- GoFish(
      ToString(x$international_standard_numbers[[1]]$value)
    )
    # Fetch publisher
    meta$publisher <- GoFish(x$publisher$name)
    # Fetch publication place
    if (is.null(meta$place)) meta$place <- GoFish(x$publisher$place)
    # Fetch series name
    meta$series <- GoFish(x$series$name)
    # Fetch series number
    meta$seriesNumber <- GoFish(x$issue)
    # Fetch title of journal
    meta$publicationTitle <- GoFish(x$journal$name)
    # Fetch pages
    meta$pages <- GoFish(
      ToString(x$pages[1:2][!is.na(x$pages[1:2])], "-")
    )
    # Fetch volume of journal
    meta$volume <- GoFish(x$volume)
    # Fetch issue of journal
    meta$issue <- GoFish(x$issue)

    # Check NVI
    nvi <- GoFish(x$journal$nvi_level)
    if (is.na(nvi)) nvi <- GoFish(x$journal$publisher$nvi_level)
    if (is.na(nvi)) nvi <- GoFish(x$publisher$nvi_level)
    if (is.na(nvi)) nvi <- GoFish(x$series$nvi_level)

    # Extract DOI
    meta$DOI <- GoFish(
      strsplit(
        x$links[[1]][x$links[[1]]$url_type=="DOI",]$url[[1]], ",")[[1]][1]
    )

    # Extract URLS (Fulltext preferred )
    urls <- c("FULLTEKST","SAMMENDRAG","DATA","PROSJEKT","OMTALE", "ARKIV")
    # Fetch url
    meta$url <- GoFish(
      x$links[[1]]$url[x$links[[1]]$url_type %in% urls][[1]]
    )
    # Take any url if empty
    if (is.na(meta$url)) meta$url <- GoFish(x$links[[1]]$url[[1]])

    # Try to scrape Cristin App if DOI is empty
    if (is.na(meta$DOI) & use.identifiers) {
      # Define result url
      cristin.url <- paste0(
        "https://app.cristin.no/results/show.jsf?id=", x$cristin_result_id
      )
      # Find results links
      cristin.urls <- httr::RETRY("GET", cristin.url) |>
        rvest::read_html() |>
        rvest::html_nodes(".fact-box-section .img-with-text-textbox a") |>
        rvest::html_text()

      # Filter out URLs
      if (length(cristin.urls)) {
        # Set DOI if exist
        if (any(grepl("^.*(10\\..*)", cristin.urls))) {
          meta$DOI <- cristin.urls[grepl("^.*(10\\..*)", cristin.urls)][[1]]
        }
      }
    }

    # Check if url is in fact a DOI and set as DOI if DOI is empty
    if (is.na(meta$DOI)) {
      if (grepl("^.*(10\\..*)", meta$url)) meta$DOI <- meta$url
    }

    # Remove URL from DOI
    if (!is.na(meta$DOI)) {
      meta$DOI <- Trim(gsub(
        "^.*(10\\..*)",
        "\\1",
        meta$DOI
      ))
    }

    # Find tags
    ## What do you mean it's convoluted?
    tags <- GoFish(
      c(as.character(
        unlist(x$classification$scientific_disciplines[[1]]$name)
      ),
      as.character(
        unlist(x$classification$keywords[[1]]$name)
      )
      )
    )
    meta$tags <- if (any(!is.na(tags))) tibble::tibble(tag = tags) else NA

    # Check if items is part of a whole
    if (!is.na(GoFish(x$part_of$url))) {
      # Fetch reference book from Cristin
      external.data <- Cristin(id = basename(x$part_of$url),
                               silent = TRUE,
                               force.type = "book",
                               remove.na = FALSE,
                               zotero.check = FALSE,
                               use.identifiers = use.identifiers,
                               crossref.search = crossref.search,
                               autosearch = autosearch,
                               override = override)$results

      # Change itemType if book metadata is not empty
      if (all(is.na(GoFish(external.data)))) {

        # Change creator-type of external.data
        external.data$creators[[1]] <- external.data$creators[[1]] |>
          dplyr::mutate(
            creatorType = dplyr::case_when(
              creatorType == "author" ~ "editor",
              TRUE ~ creatorType
            )
          )

        # Set metadata as bookChapter
        meta$itemType <- "bookSection"
        # Append creators of book to zotero type creator matrix
        meta$creators <- AddAppend(
          meta$creators,
          external.data$creators[[1]]
        ) |>
          dplyr::distinct()
        # Set book title
        meta$bookTitle <- GoFish(external.data$title)
        # Fetch ISBN
        meta$ISBN <- GoFish(external.data$ISBN)
        # Fetch edition
        meta$edition <- GoFish(EditionFix(external.data$edition))
        # Set NVI if NVI is NA
        if (is.na(nvi) & "extra" %in% names(external.data)) {
          nvi <- ZoteroId("NVI", external.data$extra)
        }
        # Fetch publisher
        meta$publisher <- GoFish(external.data$publisher)
        # Fetch publication place
        meta$place <- GoFish(external.data$place)

        # Fetch url
        if (is.na(meta$url)) meta$url <- GoFish(external.data$url)
        # Set any DOI as extra
        if (!is.na(meta$DOI)) meta$extra <-  sprintf(
          "DOI: %s", meta$DOI
        )
      }
    }

    # If NVI found add to extra
    if (!is.na(nvi)) meta$extra <- AddAppend(
      sprintf("NVI: %s", nvi), meta$extra, "\n"
    )
    # Set Cristin reference in extra
    meta$extra <- AddAppend(
      sprintf("Cristin: %s", x$cristin_result_id), meta$extra, "\n"
    )

    # Fix instances where creatorType does not match itemType
    if ("editor" %in% GoFish(meta$creators$creatorType)) {

      editor.items <- c("book", "bookSection", "conferencePaper",
                        "dictionaryEntry", "document", "encyclopediaArticle",
                        "journalArticle","preprint")

      if (meta$itemType == "report" & is.na(GoFish(meta$bookTitle))) {
        meta$creators$creatorType[
          meta$creators$creatorType == "editor"] <- "serieseditor"
      } else if (!meta$itemType %in% editor.items &
                 !is.na(GoFish(meta$bookTitle))) {
        meta$itemType <- "bookSection"
      } else if (!meta$itemType %in% editor.items) {
        meta$itemType <- "book"
      }
    }

    # Check ISBN/DOI if use.identifiers is set to TRUE
    ## Omit if itemtype not supported (yet)
    not.supported <- c("thesis", "newspaperArticle", "presentation")
    if (use.identifiers &
        !meta$itemType %in% not.supported &
        is.null(external.data)) {

      # Use CrossRef if DOI is defined
      if (!is.na(meta$DOI)) {

        external.data <- ZoteroDoi(meta$DOI)
        # Stop check function for running search again
        if (!is.null(external.data)) crossref.search <- FALSE

      } # End DOI

      # Use MARC 21 if ISBN is defined and external data is NULL
      ## Only use if part_of is NA
      if (!is.na(meta$ISBN) &
          is.null(external.data)) {

        external.data <- ZoteroIsbn(
          GoFish(strsplit(meta$ISBN, ",")[[1]][[1]])
        )

      } # End ISBN

      # Search Crossref if enabled and external data is NULL
      # Items to search for

      meta.search <- c("book", "bookSection", "journalArticle", "report")
      if (crossref.search &
          is.null(external.data) &
          meta$itemType %in% meta.search &
          any(!is.na(meta$creators))) {

        external.data <- ZoteroMatch(
          title = meta$title,
          authors = meta$creators,
          date = meta$date,
          crossref.search = TRUE,
          autosearch = autosearch,
          cristin.data = meta,
          external.data = external.data,
          polite = polite,
          log = log
        )
        # Stop check function for running search again
        crossref.search <- FALSE

      } # End search data

      # Test whether external data matches metadata from Cristin
      if (!is.null(external.data) &
          any(!is.na(meta$creators)) &
          !override) {

        # Set external.data as creator if nrow >= Cristin data
        ## Usually happens if Cristin metadata exclude editors
        if (nrow(external.data$creators[[1]]) >= nrow(meta$creators)) {
          creators <- external.data$creators[[1]]
        } else {
          creators <- meta$creators
        }

        external.data <- ZoteroMatch(
          title = meta$title,
          authors = creators,
          date = meta$date,
          haystack = list(
            title = list(external.data$title),
            authors = external.data$creators,
            date = list(external.data$date)
          ),
          crossref.search = crossref.search,
          autosearch = autosearch,
          cristin.data = meta,
          external.data = external.data,
          polite = polite,
          log = log
        )

      } # End check data

      # Augment external data with Cristin data
      if (!is.null(external.data)) {

        # Fix any stray tags
        if ("tags" %in% names(external.data)) {
          if (is.character(external.data$tags)) {
            external.data$tags <- list(
              tibble::tibble(tag = external.data$tags)
            )
          }
        } else {
          external.data$tags <- NA
        }

        # Append if any tags
        if (is.data.frame(external.data$tags)) {
          meta$tags <- GoFish(AddAppend(
            external.data$tags, meta$tags
          ))
        } else if (is.data.frame(meta$tags)) {
          meta$tags <- GoFish(AddAppend(
            meta$tags, external.data$tags
          ))
        }

        # Create Zotero-type matrix from Cristin data
        meta <- GoFish(ZoteroFormat(meta), NULL)

        # Fill in any NA in external.data with cristin.data
        external.data <- dplyr::bind_cols(
          lapply(names(external.data), \(x) {
            if (is.na(external.data[,x])) {
              external.data[,x] <- GoFish(meta[,x])
            } else {
              external.data[,x]
            }
            return (external.data[,x])
          })
        )

        # Find unique creators
        external.data$creators[[1]] <- AddMissing( # Add names if missing
          external.data$creators[[1]],
          c("name",
            "firstName",
            "lastName"),
          na.type = NA_character_,
          location = NULL) |>
          # Coalesce firstName and name as filter.name
          dplyr::mutate(id = dplyr::row_number(),
                        filter.name = gsub(
                          " .*$",
                          "",
                          dplyr::coalesce(firstName, name)
                        )) |>
          # Find unique rows based on type, lastName and filter.name
          dplyr::distinct(
            creatorType,
            lastName,
            filter.name,
            .keep_all = TRUE,
            name
          ) |>
          dplyr::select(-c(id, filter.name))

        # Add or append extra data
        external.data$extra <- AddAppend(
          meta$extra, external.data$extra, "\n"
        )

        # Set external data as metadata
        meta <- external.data

      } # End combine external and Cristin data

    } # End use.identifiers


    # Set meta as Cristin if not defined by external data
    if (!is.data.frame(meta)) meta <- GoFish(ZoteroFormat(meta), NULL)

    # Set accessDate
    meta$accessDate <-  as.character(Sys.time())

    # Remove any missing creators
    if (any(!is.na(GoFish(meta$creators[[1]])))) {
      meta$creators[[1]] <-  meta$creators[[1]] |>
        dplyr::filter_all(dplyr::any_vars(!is.na(.)))
    }
    # Sett as NULL if no creators
    if (is.null(GoFish(meta$creators[[1]], NULL))) meta$creators <- NA

    # Remove hyphens from ISBN
    if (!is.na(GoFish(meta$ISBN))) meta$ISBN <-  Trim(
      gsub('[^[:alnum:] ]', "", meta$ISBN)
    )

    # Add or append results
    results <- AddAppend(meta, results)

    # Add ID to ETA message
    eta.message <- paste(
      "Next ID:", data[i+1,]$cristin_result_id
    )

    # Estimate time of arrival
    log.eta <-
      LogCat(
        Eta(query.start, i, nrow(data), eta.message),
        silent = silent,
        flush = TRUE,
        log = log,
        append.log = FALSE
      )
  }
  # Add to log
  log <- append(log,log.eta)

  return (
    list(
      results = results,
      log = log
    )
  )

}
