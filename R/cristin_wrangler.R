#' @title Wrangle Cristin metadata into Zotero-type format
#' @description The little engine that could, at least try, to wrangle metadata
#' from Cristin into a Zotero-type matrix. References can be augmented, or
#' replaced with metadata gathered through ISBN or DOI identifiers, and through
#' Crossref queries if all other fails. Or, hope the best and place your
#' misguided faith in the ability of researchers to correctly register their
#' own publications.
#' @param data Tibble containing metadata from Cristin
#' @param zotero List with Zotero information used to check for duplicates,
#' Default: NULL
#' @param zotero.check Should the function look for duplicates?, Default: TRUE
#' @param use.identifiers Use if ISBN/DOI identifiers if enabled, Default: TRUE
#' @param crossref.search Query Crossref database based on title, authors,
#'  and date if enabled, Default: FALSE
#' @param autosearch Results could be automatically evaluated (based on some
#' logic) or you could inspect them manually if set to FALSE, Default: TRUE
#' @param remove.na Cristin contains many, more or less, obscure categories,
#' and not all are (yet) supported. By default these are removed, however, if
#' this option is set to FALSE unsupported categories are treated according to
#' replace.na, Default: TRUE
#' @param replace.na May the odds be in your favor and replace unsupported
#' categories with a predefined itemType if remove.na is set to
#' false, Default: 'book'
#' @param force.type Force all items to a predefined itemType, Default: NULL
#' @param override Put your faith in the algorithms and the identifiers
#' (i.e., DOI/ISNB) and override what is reported in Cristin, Default: FALSE
#' @param silent Running silent, running deep, Default: FALSE
#' @param polite Please store you email in `.Renviron` to query Crossref,
#' Default: TRUE
#' @param log A list for storing log elements, Default: list()
#' @return A Zotero-type matrix (tibble)
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Simple `Cristin` search by id with import set as FALSE
#'     example <- Cristin(id = "840998", zotero.import = FALSE)
#'     # Use `ZoteroIndex` to print `CristinWrangler`
#'     ZoteroIndex(CristinWrangler(example$results)$results)$name
#'   }
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
                     zotero.check = TRUE,
                     zotero = NULL,
                     use.identifiers = TRUE,
                     crossref.search = FALSE,
                     autosearch = TRUE,
                     remove.na = TRUE,
                     replace.na = "book",
                     force.type = NULL,
                     override = FALSE,
                     silent = FALSE,
                     polite = TRUE,
                     log = list()) {

  # Visible bindings
  title <- date_published <- year_published <- firstName <- creatorType <-
    lastName <- filter.name <- . <- external.data <- results <-
    unsupported.id <- removed.id <- id <- name <- key <- NULL

  # Define cristin category type
  data$type <- GoFish(data$category$name$en)

  # Check which data is supported
  supported <- CristinSupported(data)

  # Supported data
  data <- supported$data

  # Checking references message
  log <-  LogCat(
    "Checking whether references are supported (See `CristinSupported()`).",
    silent = silent,
    log = log
  )

  # Number of unsupported types
  n.unsupported <- length(supported$unsupported)

  # Either remove or convert references if n.unsupported > 0
  if (n.unsupported > 0 | !is.null(force.type)) {

    unsupported.id <- data[is.na(data$category), ]$cristin_result_id

    # Remove not supported types if remove.na is set to TRUE
    if (remove.na & is.null(force.type)) {

      data <- data[!is.na(data$category), ]

      # Removal of unsupported references message
      message <- sprintf(
        "Removed %s unsupported %s (see `$unsupported.id`).",
        n.unsupported,
        Pluralis(n.unsupported, "reference", "references", FALSE)
      )
      # Set all categories as force.type if defined
    } else if (!is.null(force.type)) {

      data$category <- force.type
      message <- sprintf("Forced all items to %s", force.type)

      # Replace unsupported categories with replace.na
    } else {
      data$category <- data$category |>
        {\(.) {replace(., is.na(.), replace.na)}}()

      # Convert unsupported references message
      message <- sprintf(
        "Converted %s unsupported %s to %s (see `$unsupported.id`)",
        n.unsupported,
        Pluralis(n.unsupported, "reference", "references", FALSE),
        replace.na
      )
    }

    # Log removal
    log <-  LogCat(message,
                        silent = silent,
                        log = log)
  }

  # Checking missing data message
  log <-  LogCat(
    "Looking for missing data.",
    silent = silent,
    log = log
  )

  # How many remaining references
  n.data <- max(0,nrow(data))

  # filter out data missing title, and/or date
  data <- data |>
    tidyr::unnest(title, names_sep = "_") |>
    dplyr::filter(!dplyr::if_all(c(dplyr::starts_with("title")), is.na),
                  GoFish(!is.na(date_published)) |
                    GoFish(!is.na(year_published))) |>
    tidyr::nest(title = dplyr::starts_with("title"), .names_sep = "_")

  removed.id <- supported$data$cristin_result_id[
    !supported$data$cristin_result_id %in% data$cristin_result_id
  ]

  # Log message if any data is filtered out
  if (max(0,nrow(data)) < n.data) {

    log <-  LogCat(
      sprintf("Removed %s with missing data (see `$removed.id`).",
              Pluralis(n.data-max(0, nrow(data)), "item", "items")),
      silent = silent,
      log = log
    )
    # How many remaining references
    n.data <- max(0,nrow(data))
  }

  # Check whether references exists in Zotero library if zotero.check is TRUE
  if (zotero.check & !is.null(zotero)) {

    check <- ZoteroCheck(data,
                        id = "cristin_result_id",
                        id.type = "Cristin",
                        created = "created",
                        last.modified = "last_modified",
                        zotero = zotero,
                        silent = silent,
                        log = log)

    # Set new data
    data <- check$data

    # Add to log
    log <- append(log, check$log)

  }

  # How many remaining references
  n.data <- max(0,nrow(data))

  # Converting to zotery-type format
  log <-  LogCat(sprintf("Converting %s from Cristin to Zotero format" ,
                              Pluralis(n.data,
                                            "reference", "references")),
                      silent = silent,
                      log = log)

  # List of initial states
  initial.state <- list(
    external.data = external.data,
    crossref.search = crossref.search
  )

  # Start time for query
  query.start <- Sys.time()

  # Run conversion if nrow(data) > 0
  if (n.data > 0) {

    # Cycle through references
    ## Should perhaps vectorize, but for loop seems more informative tbh
    for (i in 1:nrow(data)) {

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

      # Set part_of to NA if force.type is defined
      if (!is.null(force.type)) x$part_of <- NA

      # Fetch title
      meta$title <- GoFish(x$title[[1]][[x$original_language]])
      if (is.na(meta$title)) meta$title <- GoFish(
        GoFish(x$title[[1]][!is.na(x$title[[1]])][[1]])
      )
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
        meta$creators <- ZoteroCreator(lapply(1:nrow(get.creators), \(i) {
          list(type = GoFish(get.creators[i, ]$affiliations[[1]]$role_code),
               name = GoFish(c(
                 get.creators[i,]$surname,
                 get.creators[i,]$first_name
               )))
        }))
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
                                 zotero.check = FALSE,
                                 use.identifiers = use.identifiers,
                                 crossref.search = crossref.search,
                                 autosearch = autosearch,
                                 override = override)$export

        # Change itemType if book metadata is not NULL
        if (!is.null(external.data)) {
          # Set metadata as bookChapter
          meta$itemType <- "bookSection"
          # Append creators of book to zotero type creator matrix
          meta$creators <- AddAppend(
            meta$creators,
            external.data$creators[[1]]
          )
          # Set book title
          meta$bookTitle <- GoFish(external.data$title)
          # Fetch ISBN
          meta$ISBN <- GoFish(external.data$ISBN)
          # Fetch edition
          meta$edition <- GoFish(EditionFix(external.data$edition))
          # Set NVI if NVI is NA
          if (is.na(nvi) & grepl("NVI: ", external.data$extra)) {
            nvi <- gsub(".*NVI: (\\w+)*", "\\1", external.data$extra)
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
            dplyr::select(-c(id,filter.name))

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

  } # End convert data

  # Create return list
  results <- list(
    results = results,
    unsupported.id = paste0(unsupported.id, collapse = ", "),
    removed.id = paste0(removed.id, collapse = ", "),
    log = log
  )

  return (results)

}
