#' @title Search or match items using CrossRef
#' @description Use the CrossRef API to match data (e.g., Cristin metadata with
#'   Crossref metadata) or search by title, authors and date
#' @param title Title of reference
#' @param authors creators of the reference
#' @param date publication date of the reference
#' @param haystack Potential matches for search term (i.e., needle), Default:
#'   NULL
#' @param haystack.size Number of items in the haystack, Default: 3
#' @param crossref.search Search CrossRef if needle not found in haystack,
#'   Default: FALSE
#' @param autosearch Match automatically or compare needle with haystack,
#'   Default: FALSE
#' @param cristin.data Metadata from Cristin, Default: NULL
#' @param external.data Metadata from external source (e.g., CrossRef), Default:
#'   NULL
#' @param polite Will use an email stored in `.Renviron`, Default: TRUE
#' @param silent Running silent, running deep, Default: FALSE
#' @param log A list for storing log elements, Default: list()
#' @return A Zotero-type matrix (tibble) if match is found otherwise NULL
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Conduct an autosearch in CrossRef using title, authors and date
#'   example <- ZoteroMatch(
#'     title = "Nonreplicable publications",
#'     authors = "Serra-Garcia & Gneezy",
#'     date = "2021",
#'     autosearch = TRUE
#'   )
#'
#'   # Print index using `ZoteroIndex`
#'   if (any(nrow(example$data))) {
#'   ZoteroIndex(example$data) |>
#'     dplyr::select(name) |>
#'     print(width = 80)
#'   }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}},
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{coalesce}},
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[utils]{adist}}, \code{\link[utils]{head}}
#'  \code{\link[purrr]{map}}
#' @rdname ZoteroMatch
#' @export
ZoteroMatch <- \(title,
                 authors,
                 date,
                 haystack = NULL,
                 haystack.size = 3,
                 crossref.search = FALSE,
                 autosearch = FALSE,
                 cristin.data = NULL,
                 external.data = NULL,
                 polite = TRUE,
                 silent = FALSE,
                 log = list()) {

  # Visual bindings
  result <- NULL

  # Set cross.ref.search to TRUE if all matching data is missing
  if (is.null(haystack) & is.null(cristin.data) & is.null(external.data)) {
    crossref.search <- TRUE
  }

  # Function to create search parameters
  SearchParameters <- \(title, authors, date) {

    # Set metadata as null if any parameter is na
    if (any(is.na(GoFish(title))) |
        all(is.na(GoFish(authors))) |
        any(is.na(GoFish(date)))) {
      meta <- NULL
      # Else create list with search parameters
    } else {

      # Vector of authors names
      names <- c("lastName","name","family","surname")
      # Remove any numbering before title (common among book chapters)
      title <- CleanText(title)
      # Full computer friendly title
      clean.title <- ComputerFriendly(title)
      # Create short computer friendly title
      short.title <- ComputerFriendly(
        title, remove.after = TRUE
      )
      # Create author vector if tibble
      if (is.data.frame(authors)) {
        authors <- authors |>
          # Add missing column names from names
          (\(.) AddMissing(., names, NA_character_))() |>
          # Select only columns from names
          dplyr::select(dplyr::one_of(names)) |>
          # Keep only !is.na
          dplyr::transmute(dplyr::coalesce(!!!rlang::syms(names))) |>
          # Create vector
          dplyr::pull()
      }
      # Create computer friendly author vector
      clean.authors <- authors |> ComputerFriendly()
      # Single computer friendly author string
      string.authors <- ToString(clean.authors, "_")
      # Extract year (4 digits) from date
      date <- gsub('.*(\\d{4}).*', '\\1', ToString(date, "_"))

      meta <- list(
        title = title,
        clean.title = clean.title,
        short.title = short.title,
        authors = authors,
        clean.authors = clean.authors,
        string.authors = string.authors,
        date = date
      )
    }
    return (meta)
  }

  # Create list with search parameters
  needle <- SearchParameters(title, authors, date)

  if (is.null(haystack) & crossref.search) {

    # Ask user not to be unfriendly is email is missing
    if (polite & is.null(Sys.getenv("CROSSREF_EMAIL"))) {
      log <- LogCat(
        "Sys.getenv(\"CROSSREF_EMAIL\") is empty.
        Please specify email, or alternatively set polite to FALSE",
        fatal = TRUE,
        log = log
      )
    }

    # Create query list
    # # Only select three rows
    query <- list(
      query.bibliographic = paste(
        needle$clean.title,
        needle$string.authors,
        needle$date,
        sep = ","
      ),
      rows = haystack.size,
      mailto = Sys.getenv("CROSSREF_EMAIL")
    )

    # Query CrossRef
    httr.get <- Online(
      httr::RETRY(
        "GET",
        "http://api.crossref.org/works/",
        query = query,
        quiet = TRUE
      ),
      silent = silent,
      message = "Zotero Match"
    )
    log <- append(log, httr.get$log)

    # Log and return error if status code != 200
    if (httr.get$error) {
      return (httr.get)
    }

    # Convert results to tables
    haystack.data <- jsonlite::fromJSON(
      ParseUrl(httr.get$data, "text")
    )$message$items

    # Select search parameters from haystack data
    haystack <- list(
      title = haystack.data$title,
      authors = haystack.data$author,
      date = haystack.data$published$`date-parts`
    )

  }

  # Create search parameters
  haystack <- lapply(seq_along(lengths(haystack)[1]), \(i) {
    SearchParameters(
      title = haystack$title[[i]],
      authors = haystack$authors[[i]],
      date = haystack$date[[i]]
    )
  })

  # Remove empty elements from haystack data (if exist)
  if (exists("haystack.data", inherits = FALSE)) {
    empty <- which(lengths(haystack) == 0)
    if (length(empty)) haystack.data <- haystack.data[-empty,]
  }

  # Remove empty elements from haystack
  haystack <- haystack[lengths(haystack) != 0]

  # Number of elements in haystack
  haystack.seq <- seq_len(length(haystack))

  # Return if haystack has no length
  if (!length(haystack)) {
    log <-  LogCat(
      "Found no potential matches",
      silent = silent,
      log = log
    )
    return (list(data = NULL, log = log))
    # Else log number potential matches
  } else {
    log <-  LogCat(
      sprintf(
        "Found %s" ,
        Pluralis(length(haystack), "potential match", "potential matches")
      ),
      silent = silent,
      log = log
    )
  }

  if (autosearch) {

    # Try to find best match with all authors
    author.match <- unlist(lapply(haystack, function (x) {
      needle$string.authors == x$string.authors
    }))

    # Try to find the best match using length and first author
    if (!any(author.match)) {
      author.match <- unlist(lapply(haystack, function (x) {
        # Check if authors is of equal length
        length <- max(0,length(needle$clean.authors)) ==
          max(0,length(x$clean.authors))
        # Check if first author name is equal
        names <- grepl(
          needle$clean.authors[[1]], x$clean.authors[[1]]
        )
        all(length & names)
      }))
    }

    # Try to find best match with all authors regardless of order
    if (!any(author.match)) {
      author.match <- unlist(lapply(haystack, function (x) {
        all(x$clean.authors %in% needle$clean.authors)
      }))
    }

    # Try to find best match of the three results using title
    title.match <- unlist(lapply(haystack, function (x) {
      # Check if titles are equal
      check <- grepl(needle$clean.title, x$clean.title)
      return (check)
    }))

    # Second attempt on title if no match is found
    # Calculate distance between title and search
    if (!any(title.match)) {
      title.match <- unlist(lapply(haystack, function (x) {
        needle <- strsplit(needle$clean.title, "_")[[1]]
        # Calculate mean distance between elements in title vs. search
        dist <- mean(
          utils::adist(needle, x$clean.title,
                       fixed = TRUE,
                       partial = TRUE,
                       useBytes = TRUE)
        )
        # Why not a distance of, oh, one?
        dist.ratio <- dist < 1

        # Find difference in number of words in titles
        needle <- length(needle)
        haystack <- length(strsplit(x$clean.title, "_")[[1]])
        length.ratio <- needle %in% c(haystack-2, haystack, haystack+2)

        all(dist.ratio, length.ratio)

      }))
    }

    # Final attempt on title if no match is found
    # Calculate distance between short.title and search
    if (!any(title.match)) {
      title.match <- unlist(lapply(haystack, function (x) {
        needle <- strsplit(needle$short.title, "_")[[1]]
        # Calculate mean distance between elements in title vs. search
        dist <- mean(
          utils::adist(needle, x$short.title,
                       fixed = TRUE,
                       partial = TRUE,
                       useBytes = TRUE)
        )
        # Why not a distance of, oh, one?
        dist.ratio <- dist < 1

        # Find difference in number of words in titles
        needle <- length(needle)
        haystack <- length(strsplit(x$short.title, "_")[[1]])
        length.ratio <- needle %in% c(haystack-2, haystack, haystack+2)

        all(dist.ratio, length.ratio)

      }))
    }

    # Try to find best match of the three results using date
    date.match <- grepl(needle$date, purrr::map(haystack, "date"))

    # Try date-range (+- 1 year) if match is not found on date
    if (!any(date.match)) {
      date.match <- unlist(lapply(haystack, \(x) {
        year <- as.numeric(x$date)
        needle$date %in% c(year-1, year, year+1)
      }))
    }

    # Best match equals TRUE in all cases
    matches <- which(author.match & title.match & date.match)
    best.match <- if (!length(matches)) NA else matches[[1]]

  } else {

    # Set initial search parameters
    needle.search <- sprintf(
      "\n\nSearch title: %s\nAuthors: %s \nYear: %s \n",
      needle$title, ToString(needle$authors), needle$date
    )

    # Set search results
    haystack.search <- lapply(seq_along(haystack), \(i) {
      sprintf(
        "\n\n%s) Crossref title: %s\nAuthors: %s \nYear: %s", i,
        haystack[[i]]$title,
        ToString(haystack[[i]]$authors),
        haystack[[i]]$date
      )
    })

    # Display
    cat(needle.search)
    for (i in haystack.seq) {
      cat(haystack.search[[i]])
    }
    cat(
      sprintf(
        "\n\nEnter 1-%s to select a result, or press enter to abort\n\n",
        utils::tail(haystack.seq,1)
      )
    )

    # Display prompt to answer
    answer <- as.integer(readline(prompt="Your input: " ))

    # Find best match
    best.match <- if (answer %in% haystack.seq) answer else NA

  }

  # Find reference based on DOI if best.match is found and DOI is defined
  if (!is.na(best.match) & exists("haystack.data", inherits = FALSE)) {

    # Find metadata from Crossref using DOI
    doi <- haystack.data[best.match,"DOI"]
    doi <- ZoteroDoi(doi)
    result <- doi$data
    log <- append(log, doi$log)
    if (!is.null(result)) {

      # Set doi in extra if book
      if (result$itemType == "book" | result$itemType == "bookSection") {
        result$extra <- paste0("DOI: ", doi)
      }

      # Set a caution in meta$extra
      if (autosearch) {
        result$extra <- AddAppend("CAUTION: DOI automatically retrieved",
                                  result$extra, "\n")
      }

    }

    # Else search Crossref if DOI not defined
  } else if (crossref.search &
             is.na(best.match) &
             !exists("haystack.data", inherits = FALSE)) {

    result <- ZoteroMatch(
      title = title,
      authors = authors,
      date = date,
      crossref.search = crossref.search,
      autosearch = autosearch,
      cristin.data = cristin.data,
      external.data = external.data,
      polite = polite,
      log = log
    )

    # Else external data matches Cristin data. Return external data
  } else if (!is.na(best.match)) {
    result <- external.data
  } else if (is.na(best.match)) {
    log <-  LogCat(
      "Found no matches",
      silent = silent,
      log = log
    )

  }

  return (list(data = result, log = log))

}
