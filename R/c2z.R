################################################################################
#####################################Import#####################################
################################################################################

#' @importFrom dplyr arrange bind_rows case_when coalesce distinct filter
#' group_by mutate na_if one_of pull row_number select slice_head transmute
#' ungroup full_join join_by any_of
#' @importFrom httr GET RETRY add_headers content http_error
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom purrr map pmap pmap_chr
#' @importFrom rlang syms
#' @importFrom rvest html_attr html_attrs html_children html_name html_nodes
#' html_text read_html
#' @importFrom stats setNames
#' @importFrom tibble add_column as_tibble remove_rownames tibble is_tibble
#' @importFrom utils adist flush.console head tail write.csv
#' @importFrom rlang :=
NULL
#> NULL

################################################################################
#################################Internal Data##################################
################################################################################

#' @title List with empty zotero-items
#' @description Each tibble in the list represents a zotero-item
#' @format A list with 36 tibbles with zero rows and various columns
#' @details Used to create Zotero-items from list of metadata
#' @rdname zotero.types
#' @keywords internal
"zotero.types"
#> NULL

################################################################################
###############################Internal Functions###############################
################################################################################

#' @title CreateHeader
#' @keywords internal
#' @noRd
CreateHeader <- \(title,
                  level,
                  md.headers = FALSE,
                  id = "",
                  class = "") {

  if (any(is.na(GoFish(title)))) {
    return (character(0))
  }

  if (md.headers) {
    header <- sprintf(
      "<div id=\"%s\" class=\"%s\">\n\n%s %s\n\n</div>",
      id,
      class,
      strrep("#", level),
      title
    )
  } else {
    header <- sprintf(
      "<h%1$s id=\"%3$s\" class=\"%4$s\">%2$s</h%1$s>",
      level,
      title,
      id,
      class
    )
  }

  return (header)
}

#' @title CristinWeb
#' @keywords internal
#' @noRd
CristinWeb <- \(data,
                k.paths = seq_len(2),
                md.headers = TRUE,
                lang = "nn") {

  # Visible bindings
  html <- month.key <- year.month <- years <- months <- NULL
  collapse <- archive <- TRUE

  CreateWeb <- \(data, paths) {

    # Languages
    # Set language to en if not nb or nn
    if (!lang %in% c("nb", "nn", "no")) {
      lang <- "en"
      affiliation <- "Affiliated"
      more <- "Read more"
    } else {
      lang <- "no"
      affiliation <- "Affiliert"
      more <- "Les meir"
    }

    # Combine indivial pages
    pages <- dplyr::bind_rows(data$bib.pages) |>
      dplyr::bind_rows()

    # Filter data that all paths are not NA and split data by paths
    data <- data |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(paths), ~ !is.na(.x))) |>
      dplyr::group_split(!!!rlang::syms(paths))

    # Loop through data lists
    monthlies <- lapply(data, \(data) {

      # Create title
      if (any(grepl("month", paths))) {

        title <- data$lang.month[[1]]
        collapse <- FALSE

      } else if (any(grepl("year", paths))) {

        title <- data$year[[1]]

      } else {

        title <- data[1, paths] |>
          (\(x) ToString(tail(unlist(x), 1)) )()

      }

      # Define md.tags
      md.tags <- sprintf("bookCollapseSection: %s\ntitle: \"%s\"", collapse, title)

      # Create data pahts
      path <- data[1, paths] |>
        ComputerFriendly() |>
        (\(x) file.path(do.call(file.path, as.list(x[!is.na(x)]))))()

      # Vector with full name paths
      full.path <- data[1, paths] |>
        (\(x) x[!is.na(x)] )() |>
        list()

      # Define period for data
      period <- paste(unique(c(min(data$year), max(data$year))), collapse = "-")

      # Clear all path names to avoid redundant headers
      data[, paths] <- NA

      # Split all data by year and month
      data <- data |>
        dplyr::mutate(year = replace(year, duplicated(year), NA)) |>
        dplyr::group_split(year.month) |>
        rev()

      # Create html for each month
      for (i in seq_along(data)) {

        bib <- data[[i]] |>
          dplyr::mutate(
            # Remove duplicated values in units
            dplyr::across(
              dplyr::starts_with("path"), ~
                replace(.x, duplicated(.x), NA)
            ),
            dplyr::across(
              c(dplyr::starts_with("path")), ~ replace(.x, .x %in% affiliation, NA)
            ),
            # Create headers for each row
            headers = purrr::pmap_chr(
              dplyr::across(dplyr::starts_with("path")), ~ {
                x <- c(...)
                lapply(seq_along(x), \(i) {
                  if (!is.na(x[[i]])) {
                    CreateHeader(x[[i]], i, md.headers, class = "csl-bib-headers")
                  }
                }) |>
                  ToString("\n") |>
                  GoFish()
              }
            )
          )

        # Check if year is in current month
        year <- CreateHeader(
          bib$year[[1]],
          1,
          md.headers,
          class = "csl-bib-year"
        )

        # Define month
        month <- CreateHeader(
          bib$lang.month[[1]],
          2,
          md.headers,
          class = "csl-bib-month"
        )

        # Define HTML bibliography
        bib <- ToString(
          c(GoFish(year, NULL),
            GoFish(month, NULL),
            Interleave(bib$headers, Trim(bib$bib.web))
          ), "\n")

        # Define body
        html <- c(html, bib)

      } # End loop

      # Combine bodies
      html <- sprintf(
        "<div class=\"csl-bib-container\">%s</div>",
        ToString(html, "\n")
      )

      # Create tibble with data
      data <- tibble::tibble(
        period = period,
        title = title,
        path = path,
        full.path = full.path,
        md.tags = md.tags,
        html = html,
        archive = TRUE
      )

      return (data)

    })

    # Combine archive data and individuals pages
    data <- dplyr::bind_rows(monthlies, pages)

    return (data)

  }

  # Create the various paths according to k.paths
  paths <- unlist(lapply(k.paths, \(i) {
    lapply(0:2, \(j) {
      c(paste0("path", seq_len(i)), c("year", "lang.month")[seq_len(j)])
    })
  }), recursive = FALSE)

  # Finalize pages
  pages <- lapply(paths, \(x) CreateWeb(data, x)) |>
    dplyr::bind_rows() |>
    dplyr::arrange(!archive)


  return (pages)

}

#' @title CristinMail
#' @keywords internal
#' @noRd
CristinMail <- \(data,
                 subject = NULL,
                 header = NULL,
                 footer = NULL,
                 replace.style = TRUE,
                 width = 700,
                 lang = "nn") {

  # Visible bindings
  body <- month.key <- year.month <- year <- NULL

  # Languages
  # Set language to en if not nb or nn
  if (!lang %in% c("nb", "nn", "no")) {
    lang <- "en"
    affiliation <- "Affiliated"
  } else {
    lang <- "no"
    affiliation <- "Affiliert"
  }

  # Keep only latest entry
  bib <- data |>
    dplyr::filter(!is.na(month.key)) |>
    dplyr::mutate(
      dplyr::across(
        c(year, dplyr::starts_with("path")), ~
          replace(.x, duplicated(.x), NA)
      ),
      dplyr::across(
      c(dplyr::starts_with("path")), ~ replace(.x, .x %in% affiliation, NA)
      )
    ) |>
    dplyr::group_split(year.month) |>
    rev() |>
    purrr::pluck(1) |>
    dplyr::mutate(
      # Create headers for each row
      headers = purrr::pmap_chr(
        dplyr::across(dplyr::starts_with("path")), ~ {
          x <- c(...)
          lapply(seq_along(x), \(i) {
            if (!is.na(x[[i]])) {
              if (i == 1) {
                sprintf("<h1 style=\"text-align:center;\">%s</h1>", x[[i]])
              } else {
                sprintf("<h%1$s>%2$s</h%1$s>", i, x[[i]])
              }
            }
          }) |>
            ToString("\n") |>
            GoFish()
        }
      )
    )

  # Return NULL if no data
  if (!any(nrow(bib))) {
    return (NULL)
  }

  # replace style from bibliography
  ## CSS is poorly supported in many email clients
  if (replace.style) {
    bib$bib.email <- gsub(
      "*.style=(\"|')[^(\"|')]*(\"|')",
      "",
      bib$bib.email
    )
    # Add space between items
    bib$bib.email <- gsub(
      "class=\"csl-entry\"",
      "class=\"csl-entry\" style=\"margin: 0.5em 0;\"",
      bib$bib.email
    )
  }

  # Define subject
  if (is.null(subject)) {
    subject <- sprintf(
      "Nyleg registrerte publikasjonar i Cristin (%s, %s)",
      tolower(Month(bib$month[[1]], lang = lang)),
      bib$year[[1]]
    )
  }

  # Define header
  if (is.null(header)) {
    header <- sprintf(
      "Dette dokumentet inneheld nyleg registrerte publikasjonar i Cristin ved
      %s for %s, %s. Kvar publikasjon er oppf\u00f8rt med lenkje til resultatet
      i Cristin, samt lenkje til referansen i Zotero-arkivet.",
      ToString(data$path1[[1]]),
      tolower(Month(bib$month[[1]], lang = lang)),
      bib$year[[1]]
    )
  }

  # Define footer
  if (is.null(footer)) {
    footer <- "<div style=\"font-size:80%;margin: 2em auto 0 auto; text-align:
    center;\">CRIStin Bot nyttar seg av<i>
    <a href=\"https://oeysan.github.io/c2z/\">c2z</a></i></div>"
  }

  # Create HTML body with references from Cristin
  body <- Trim(sprintf(
    "<table width=\"%1$s\" cellpadding=\"0\" cellspacing=\"0\">
    <tr><td>
    <div style=\"max-width:%1$spx;\">
    %2$s%3$s%4$s
    </div>
    </td></tr>
    </table>",
    width,
    header,
    ToString(Interleave(bib$headers, bib$bib.email), "\n"),
    footer
  ))

  return (list(subject = subject, body = body))

}

#' @title CristinId
#' @keywords internal
#' @noRd
CristinId <- \(id, error = NULL) {

  # Visible bindings
  cristin_person_id <- NULL

  # Find contributors to Cristin reference
  httr.get <- Online(
    httr::RETRY(
      "GET",
      sprintf(
        "https://api.cristin.no/v2/results/%s/contributors",
        id),
      quiet = TRUE
    ),
    silent = TRUE,
    message = "Cristin contributors"
  )

  # Return Null not found
  if (httr.get$error) {
    return (error)
  }

  # Find names from Cristin profile
  cristin <- httr.get$data |>
    JsonToTibble() |>
    dplyr::select(cristin_person_id) |>
    unlist()

}

#' @title InnCristin
#' @keywords internal
#' @noRd
InnCristin <- \(name) {

  # Query user url
  httr.get <- Online(
    httr::RETRY(
      "GET",
      "https://api.cristin.no/v2/persons/",
      query = list(
        name = paste(name, collapse=" "),
        institution = 209
      ),
      quiet = TRUE
    ),
    silent = TRUE
  )

  # Return NA if not found
  if (httr.get$error) {
    return (NA)
  }

  id <- GoFish(
    httr::content(httr.get$data)[[1]]$cristin_person_id[[1]]
  )

  return (id)

}

#' @title InnCards
#' @keywords internal
#' @noRd
InnCards <- \(url, error = NULL) {

  # Query user url
  httr.get <- Online(
    httr::RETRY(
      "GET",
      url,
      quiet = TRUE,
      httr::accept("text/html")
    ),
    silent = TRUE,
    message = "Cristin contributors"
  )

  # Return Null not found
  if (httr.get$error) {
    return (error)
  }

  # Find card
  card <- httr.get$data |>
    rvest::read_html() |>
    rvest::html_nodes(".vrtx-hinn-person-card") |>
    toString() |>
    Trim()

  # Add user url to adress
  card <- sub(
    "#vrtx-hinn-addresses",
    paste0(url, "#vrtx-hinn-addresses"),
    card
  ) |>
    (\(x) tibble::tibble(card = x))()

  return (card)

}

#' @title InnUsers
#' @keywords internal
#' @noRd
InnUsers <- \(i = 1, lang = "no") {

  # Languages
  # Set language to en if not nb or nn
  if (!lang %in% c("nb", "nn", "no")) {
    lang <- "en"
    card.url <- "https://www.inn.no/english/find-an-employee/"
  } else {
    lang <- "no"
    card.url <- "https://www.inn.no/finn-en-ansatt/"
  }

  httr.get <- Online(
    httr::RETRY(
      "GET",
      card.url,
      query = list(page = i),
      quiet = TRUE,
      httr::accept("text/html")
    ),
    silent = TRUE,
    message = "Cristin contributor"
  )

  # Return Null not found
  if (httr.get$error) {
    return (NULL)
  }

  # Find urls
  urls <- httr.get$data |>
    rvest::read_html() |>
    rvest::html_nodes(".vrtx-person-listing li span:first-child a") |>
    rvest::html_attr('href')

  # Return Null not found
  if (!length(urls)) {
    return (NULL)
  }

  names <- httr.get$data |>
    rvest::read_html() |>
    rvest::html_nodes(".vrtx-person-listing li span:first-child a") |>
    rvest::html_text() |>
    Trim() |>
    (\(x) {
      dplyr::bind_rows(
        lapply(strsplit(x, ", "), \(x) {
          setNames(x, c("last.name", "first.name"))
        })
      )
    })()


  cristin.ids <- lapply(seq_len(nrow(names)), \(i) {
    tibble::tibble(cristin.id = InnCristin(names[i, ]))
  }) |>
    dplyr::bind_rows()


  users <- tibble::tibble(names, cristin.ids, url = urls)

  remaining.pages <- httr.get$data |>
    rvest::read_html() |>
    rvest::html_nodes(".vrtx-paging-wrapper a") |>
    rvest::html_text() |>
    (\(x) if ((i+1) %in% x) InnUsers(i+1, lang))()

  users <- dplyr::bind_rows(users, remaining.pages)

  return (users)

}

#' @title Interleave
#' @keywords internal
#' @noRd
Interleave <- function(a, b, rm.na = TRUE) {
  interleave <- order(c(seq_along(a), seq_along(b)))
  ab <- c(a, b)[interleave]
  if (rm.na) ab <- ab[!is.na(ab)]
  return(ab)
}

#' @title AncestorPath
#' @keywords internal
#' @noRd
AncestorPath <- \(data, path.key, name = FALSE) {

  # Return NULL if not in data
  if (!path.key %in% data$key) {
    return (NULL)
  }

  # Find parent key
  parent = data[data$key == path.key, ]$parentCollection
  # Use parent name if name is TRUE else use key
  id <- if(name) data[data$key == path.key, ]$name else path.key

  # Return id if parentCollection is FALSE
  if(parent == "FALSE")  {
    path <- id
    # Else continue searching for ancestor
  } else {
    path <- c(AncestorPath(data, parent, name), id)
  }

  return (path)

}

#' @title DescendingPath
#' @keywords internal
#' @noRd
DescendingPath <- \(data, path.key, name = FALSE) {

  # Return NULL if not in data
  if (!path.key %in% data$key) {
    return (NULL)
  }

  # Function to travel trough the lineage of the collections
  Descending <- \(data, path.key, name) {

    # Find parent key
    child <- data[data$parentCollection == path.key, ]$key
    # Use child name if name is TRUE else use key
    id <- if(name) {
      data[data$parentCollection == path.key, ]$name
    } else {
      child
    }

    # Return id if child key not defined in parentCollection
    path <- unlist(lapply(seq_along(child), \(i) {
      if (!child[i] %in% data$parentCollection) {
        id[i]
      } else {
        c(id[i], Descending(data, child[i], name))
      }
    }))

    return (path)

  }

  # Define ancestor
  ancestor <- if(name) data[data$key == path.key, ]$name else path.key

  return (c(ancestor, Descending(data, path.key, name)))

}

#' @title FindPath
#' @keywords internal
#' @noRd
FindPath <- \(data, path.name, case.insensitive = TRUE) {

  # Visible bindings
  path <- item <- name <- parentCollection <- NULL

  for (i in seq_along(path.name)) {

    # Find parent, if k=1 there are no parents, strangely enough
    parent <- if (i == 1) "FALSE" else parent

    # Search for item using name and parentCollection if data is defined
    if (!is.null(data)) {
      item <- data |>
        dplyr::filter(grepl(
          path.name[i], name, ignore.case = case.insensitive) &
            parentCollection == parent
        ) |>
        dplyr::arrange(version) |>
        dplyr::slice(1)
    }

    # Create item if not found
    if (!any(nrow(item))) {
      item <- tibble::tibble(
        key = ZoteroKey(),
        version = 0,
        name = path.name[i],
        parentCollection = parent
      )

      # Add to data
      data <- dplyr::bind_rows(data, item)
    }

    # Define parrent
    parent <- item$key
    # Add key to path
    path <- c(path, item$key)

  }

  return (list(data = data, path = path))

}

#' @title Month
#' @keywords internal
#' @noRd
Month <- \(i = NULL,
           lang = NULL,
           abbreviation = FALSE) {

  # Return Null
  if (!GoFish(as.numeric(i) %in% seq(12), FALSE)) {
    return (NULL)
  } else {
    i <- as.numeric(i)
  }

  # Define months by norwegian if lang = no
  if (any(lang %in% c("no", "nn", "nb"))) {

    # Month names in Norwegian
    month.name <- c(
      "Januar",
      "Februar",
      "Mars",
      "April",
      "Mai",
      "Juni",
      "Juli",
      "August",
      "September",
      "Oktober",
      "November",
      "Desember"
    )

    # Abbreviated month names in Norwegian
    month.abb <- c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "Mai",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Okt",
      "Nov",
      "Des"
    )

  }

  # Set month as either month or abbreviated monther by integer
  month <- if (abbreviation) month.abb[i] else month.name[i]

  return (month)

}

#' @title ChangeDate
#' @keywords internal
#' @noRd
ChangeDate <- \(date, i, type = "months") {

  # Return NULL if not a date
  if (is.na(GoFish(as.Date(date)))) {
    return (NULL)
  }

  # sequence date by +- date (e.g., months) subtract tail
  date <- seq(date, by = paste(i, type), length = 2)[2]

  return (date)
}

#' @title FloorDate
#' @keywords internal
#' @noRd
FloorDate <- \(date) {

  # Return null if no dash in date
  if (!grepl("-", date)) {
    return (NULL)
  }

  # Convert date to character and split by dash
  date <- strsplit(as.character(date), "-")[[1]]
  # Remove days if any
  if (any(length(date) == 3)) date <- head(date, -1)

  # Paste and add day 1
  date <- date |>
    paste(collapse = "-") |>
    paste0("-01") |>
    as.Date() |>
    GoFish(NULL) # Set as null if date is erronous


  return (date)

}

#' @title CeilingDate
#' @keywords internal
#' @noRd
CeilingDate <-  \(date) {

  # Return NULL if not a date
  if (is.na(GoFish(as.Date(date)))) {
    return (NULL)
  }

  # Floor date, add month, subtract one day
  date <- FloorDate(date) |>
    ChangeDate(1, "months") |>
    ChangeDate(-1, "days")

  return (date)

}

#' @title ZoteroId
#' @keywords internal
#' @noRd
ZoteroId <- \(id.type,
              data,
              sep = "\n",
              return.type = NA) {

  # Define search pattern
  search <- sprintf(".*%s: (.*)", id.type)
  id <- unlist(lapply(data, \(x) {
    # Split vector by sep (e.g., linebreak)
    x <- unlist(strsplit(x, sep))
    # Extract by id.type for elements containing id.type
    x <- gsub(search, "\\1", x[grepl(id.type, x)])
    # Set missing vector elements as return.type (e.g., NA)
    if (!length(x)) x <- return.type

    return (x)
  }))

  return (id)

}

#' @title Online
#' @keywords internal
#' @noRd
Online <- \(query,
            message = NULL,
            reference = NULL,
            silent = FALSE,
            log = list()) {

  # Visible bindings
  data <- NULL
  error <- TRUE

  # Check query/url and set 404 error upon error
  query <- GoFish(query, stats::setNames(list(404), "status_code"))

  # Set data as query upon success else NULL
  if (query$status_code %in% 200:299) {
    data <- query
    error <- FALSE
  }

  # Some status codes
  status.codes <- c(
    "Invalid type/field (unparseable JSON)" = 400,
    "The target library is locked" = 409,
    "Precondition Failed (e.g.,
    the provided Zotero-Write-Token has already been submitted)" = 412,
    "Request Entity Too Large" = 413,
    "Forbidden (check API key)" = 403,
    "Resource not found" = 404,
    "Everything is awesome" = 200,
    "Doom of the Noldor" = 204
  )

  # Define message
  if (is.null(message)) {
    message <- names(status.codes[status.codes %in% query$status_code])
    # Set unknown error if error.code is undefined
    if (!length(message)) message <- "No message. Sorry."
  } else {
    append.message <- if (error) "not OK" else "OK"
    message <- sprintf("%s is %s", message, append.message)
  }

  # Create feedback
  feedback <- sprintf("Status %s: %s.", query$status_code, message)

  # Add reference if any
  if (!is.null(reference)) {
    feedback <- sprintf(
      "%s (`%s`: %s)",
      feedback,
      as.character(sys.call(-1)[[1]]),
      reference
    )
  }

  # Clean feedback
  feedback <- Trim(gsub("\r?\n|\r", " ", feedback))

  # Log message
  log <-  LogCat(
    feedback,
    silent = silent,
    log = log
  )

  return (
    list(
      error = error,
      code = query$status_code,
      data = data,
      log = log
    )
  )

}


#' @title FixCreators
#' @keywords internal
#' @noRd
FixCreators <- \(data = NULL) {


  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  data <- AddMissing(
    data, c("firstName", "lastName", "name"), na.type = ""
  ) |>
    dplyr::mutate_if(is.character, list(~dplyr::na_if(., ""))) |>
    dplyr::mutate(
      lastName = dplyr::case_when(
        !is.na(name) ~ NA_character_,
        TRUE ~ lastName
      ),
      firstName = dplyr::case_when(
        !is.na(name) ~ NA_character_,
        TRUE ~ firstName
      ),
      name = dplyr::case_when(
        is.na(firstName) & !is.na(lastName) ~ lastName,
        !is.na(lastName) & is.na(lastName) ~ firstName,
        is.na(firstName) & is.na(lastName) ~ name,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(dplyr::where(~sum(!is.na(.x)) > 0)) |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))

  return (data)

}

#' @title ZoteroCreator
#' @keywords internal
#' @noRd
ZoteroCreator <- \(data = NULL) {

  # Visible bindings
  creators <- NULL

  # Run if not empty
  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  # Check that first element of data is a list
  if (!is.list(data[[1]])) data <- list(data)

  # Define creator types
  types <- c(
    author = "author",
    editor = "editor",
    translator = "translator",
    aut = "author",
    edt = "editor",
    red = "editor",
    trl = "translator",
    editorialboard = "editor",
    programmer = "programmer",
    curator = "author",
    serieseditor = "seriesEditor"
  )

  # Create zotero-type matrix
  creators <- dplyr::bind_rows(
    lapply(data, \(x) {

      # Remove commas and dots from names
      name <- Trim(gsub("\\.|\\,", " ", x$name))

      # Set as Cher if only one name is given
      if (length(name) == 1) {
        name <- c(name = name)
      } else {
        name <- c(lastName = name[1], firstName = name[2])
      }

      # Define creator type according to match in creator types
      type <- as.character(types[names(types) %in% tolower(x$type)])
      # Set as contributor if not found
      if (!length(type)) type <- "contributor"

      # Combine creatorType and names
      lapply(type, \(type) c(creatorType = type, name))

    })
  )

  return (creators)

}

#' @title ZoteroToJson
#' @keywords internal
#' @noRd
ZoteroToJson <- \(data = NULL) {

  # Run if not empty
  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  # Convert data to JSON
  data <- jsonlite::toJSON(data)
  # Convert character FALSE to logical false
  data <- gsub("\"FALSE\"", "false", data)
  # Remove empty elements
  data <- gsub(",\"\\w+?\":\\{\\}", "", data)

  return (data)

}

#' @title ZoteroFormat
#' @keywords internal
#' @noRd
ZoteroFormat <- \(data = NULL,
                  format = NULL,
                  prefix = NULL) {

  # Run if not empty
  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  # Visible bindings
  creators <- NULL

  multiline.items <- c("tags",
                       "extra",
                       "abstractNote",
                       "note",
                       "creators",
                       "relations")
  double.items <- c("version", "mtime")
  list.items <- c("collections", "relations", "tags")

  # Check if metadata and not in a data frame
  if (!is.data.frame(data) &
      (any(format == "json", is.null(format)))) {

    # Check that first element of data is a list
    if (is.list(data[[1]])) data <-  unlist(data, recursive = FALSE)

    # Check all element in the meta list
    data.list <- lapply(seq_along(data), \(i) {

      # Define data
      x <- data[[i]]
      names <- names(data[i])

      # Make certain fields not in multiline are strings
      if (!names %in% multiline.items) x <- ToString(x)

      # Add to list if element is a data frame
      ## Make certain list.items is in a list
      if (is.data.frame(x) | names %in% list.items) {
        x <- if (all(is.na(GoFish(as.character(unlist(x)))))) NA else list(x)

        # Make certain double.items are double
      } else if (names %in% double.items) {
        x <- as.double(x)
        # Else make certain remaining items are character
      } else {
        x <- as.character(x)
      }

      return (x)

    })
    # Name elements
    names(data.list) <- names(data)
    # Keep number of columns fixed for simple conversion to tibble/JSON
    ## Replace empty elements with NA
    data.list[lengths(data.list) == 0] <- NA
    # Set key and initial version is missing
    if (!"key" %in% names(data.list)) {
      data.list <- c(key = ZoteroKey(), version = 0, data.list)
    }
    # Remove elements not in category if item
    if (!"parentCollection" %in% names(data.list)) {
      data.list <- data.list[names(data.list) %in%
                               c("key", "version", ZoteroTypes(data.list$itemType))]
    }

    # Format as tibble and remove empty elements
    data <- tibble::as_tibble(data.list[lengths(data.list) != 0])

  }

  # Set data as tibble if data frame
  if (is.data.frame(data)) {

    data <- tibble::as_tibble(data) |>
      # Replace empty string with NA
      dplyr::mutate_if(is.character, list(~dplyr::na_if(., ""))) |>
      dplyr::mutate(
        # Make certain tags is a data.frame within a list
        dplyr::across(
          dplyr::any_of("tags"), ~ purrr::map(tags, ~ {
            if (all(!is.na(.x))) as.data.frame(.x)
          })
        ),
        # Make certain that parentCollection is a character
        dplyr::across(dplyr::any_of("parentCollection"), as.character),
        # Add prefix if defined
        prefix = GoFish(prefix),
        # Fix creators
        creators = GoFish(purrr::map(creators, FixCreators))
      ) |>
      # Remove empty columns
      dplyr::select(dplyr::where(~sum(!is.na(.x)) > 0))

    # Else convert to string
  } else {
    data <- ToString(data, "\n")
  }

  return (data)

}

#' @title ZoteroUrl
#' @keywords internal
#' @noRd
ZoteroUrl <- \(url,
               collection.key = NULL,
               use.collection = TRUE,
               item.key = NULL,
               use.item = FALSE,
               api = NULL,
               append.collections = FALSE,
               append.items = FALSE,
               append.file = FALSE,
               append.top = FALSE) {

  # Set ute.item to FALSE if item.key is NULL
  if(is.null(item.key)) use.item <- FALSE
  # Default is not key
  use.key <- FALSE

  # Add item.key if defined and use.item set to TRUE
  if (!is.null(item.key) & use.item) {
    url <- paste0(url,"items/",item.key,"/")
    use.key <- TRUE
    # Else add collection key if defined
  } else if (!is.null(collection.key) & use.collection) {
    url <- paste0(url, "collections/", collection.key, "/")
    use.key <- TRUE
    # Else set append.items to TRUE if no keys
  } else if (!append.collections) {
    append.items <- TRUE
  }

  #  Add top if append.top set to TRUE
  if (use.key & append.top) {
    url <- paste0(url,"top")
  }

  #  Add file if append.file set to TRUE
  if (use.key & append.file) {
    url <- paste0(url,"file")
  }

  # If not using specific item or top level
  if (!use.item & !append.top & !append.file) {
    #  Add items if append.items set to TRUE
    if (append.items) {
      url <- paste0(url,"items")
      # Else add collections if append.collection set to TRUE
    } else if (append.collections) {
      url <- paste0(url,"collections")
    }
  }

  # Add API if defined
  if (!is.null(api)) {
    if (grepl("Sys.getenv", api, perl = TRUE)) api <- eval(parse(text=api))
    url <- sprintf("%s?key=%s", url, api)
  }

  return (url)

}

#' @title Pad
#' @keywords internal
#' @noRd
Pad <- \(string, sep = "-", max.width = 80) {

  # Find remaining character
  remaining.char <- max(0, max.width - nchar(string)) / 2
  head.char <- paste0(rep(sep, floor(remaining.char)), collapse = "")
  tail.char <- paste0(rep(sep, ceiling(remaining.char)), collapse = "")

  # Add pad
  padded <- paste0(head.char, string, tail.char)

  return (padded)

}

#' @title Eta
#' @keywords internal
#' @noRd
Eta <- \(start.time,
         i ,
         total,
         message = NULL,
         flush = TRUE,
         sep = "\u2014",
         max.width = 80) {

  # Estimate time of arrival
  eta <- Sys.time() + ( (total - i) * ((Sys.time() - start.time) / i) )

  # Format ETA message
  eta.message <- sprintf(
    "Progress: %.02f%% (%s/%s). ETA: %s",
    (i * 100) / total,
    i,
    total,
    format(eta,"%d.%m.%Y - %H:%M:%S")
  )
  # Arrived message
  if (i == total) {

    final <- sprintf(
      "Process: %.02f%% (%s/%s). Elapsed time: %s",
      (i * 100) / total,
      i,
      total,
      format(
        as.POSIXct(
          as.numeric(difftime(Sys.time(), start.time, units = "secs")),
          origin = "1970-01-01", tz = "UTC"
        ),
        '%H:%M:%S'
      )
    )

    arrived <- sprintf(
      "Task completed: %s", format(eta, "%d.%m.%Y - %H:%M:%S")
    )
    eta.message <- c(final, arrived)
    # Else results to ETA message if requested.
  } else if (length(message)) {
    eta.message <- sprintf("%s. %s", message, eta.message)
  }

  # Pad ETA message to avoid spilover-effect
  eta.message[1] <- Pad(eta.message[1], sep, max.width)

  return (eta.message)

}

#' @title SplitData
#' @keywords internal
#' @noRd
SplitData <- \(data, limit) {

  # Split metadata into acceptable chunks (k > 50)
  if (nrow(data)>limit) {
    data <- split(
      data,
      rep(
        seq_len(ceiling(nrow(data)/limit)),
        each=limit,
        length.out=nrow(data)
      )
    )
  } else {
    data <- list(data)
  }

  return (data)

}

#' @title ComputerFriendly
#' @keywords internal
#' @noRd
ComputerFriendly <- \(x, sep = "_", remove.after = FALSE) {

  # Remove after common line identifers
  if (remove.after) {
    character.vector <- c(".",",",":",";","-","--",
                          "\u2013","\u2014","\r","\n","/","?")
    remove.after <- paste0("\\", character.vector, collapse=".*|")
    x <- gsub(remove.after, "", x)
  }

  # Try to replace accents and foreign letters
  s <- iconv(x, "utf8", "ASCII//TRANSLIT")
  # Ignore cyrillic and similiar languages
  if (any(grepl("\\?\\?\\?", s))) {
    s <- s
  } else {
    # Replace foreign and whitespace with sep
    s <- gsub("\\W+", sep, s, perl = TRUE)
    # Trim and set to lower or set as x if only ??? (e.g., russian)
    s <- Trim(tolower(s))
  }

  return (s)

}

#' @title JsonToTibble
#' @keywords internal
#' @noRd
JsonToTibble <- \(data) {

  # Parse url
  data.parsed <-ParseUrl(data, "text")

  # Return NULL if data.pased is empty
  if (is.null(data.parsed)) {
    return(NULL)
  }

  # Parse raw data as JSON
  data <- jsonlite::fromJSON(data.parsed)

  # Convert nested elements in list as data.frame
  if (!is.data.frame(data)) {
    data <- lapply(data, \(x) {
      if (is.list(x) | length(x)>1) x <- list(x)
      return (x)
    })
  }
  # Convert and return as tibble
  return(tibble::as_tibble(data))

}

#' @title GoFish
#' @keywords internal
#' @noRd
GoFish <- \(data, type = NA) {

  data <- suppressWarnings(
    tryCatch(data, silent = TRUE, error=function(err) logical(0))
  )
  if (!length(data) | all(is.na(data))) data <- type

  return (data)

}

#' @title ToString
#' @keywords internal
#' @noRd
ToString <- \(x, sep = ", ") {

  x <- paste(unlist(GoFish(x,NULL)), collapse = sep)

  if (x == "") x <- NULL

  return (x)

}

#' @title AddMissing
#' @keywords internal
#' @noRd
AddMissing <- \(data,
                missing.names,
                na.type = NA_real_,
                location = 1) {

  missing <- stats::setNames(
    rep(na.type, length(missing.names)), missing.names
  )
  data <- tibble::add_column(data, !!!missing[
    setdiff(names(missing), names(data))], .before = location)

  return (data)

}

#' @title Trim
#' @keywords internal
#' @noRd
Trim <- \(x, multi = TRUE) {

  if (multi) {
    x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl = TRUE)
  } else {
    x <- gsub("^\\s+|\\s+$", "", x)
  }

  return(x)

}

#' @title TrimSplit
#' @keywords internal
#' @noRd
TrimSplit <- \(x,
               sep = ",",
               fixed = FALSE,
               perl = FALSE,
               useBytes = FALSE) {

  x <- Trim(unlist(strsplit(x, sep, fixed, perl, useBytes)))

  return(x)

}

#' @title Pluralis
#' @keywords internal
#' @noRd
Pluralis <- \(data, singular, plural = NULL, prefix = TRUE) {

  if (is.null(plural)) plural <- paste0(singular, "s")
  word <- if (data==1) singular else plural

  if (prefix) word <- paste(data, word)

  return (word)

}

#' @title AddAppend
#' @keywords internal
#' @noRd
AddAppend <- \(data = NULL, old.data = NULL, sep = NULL) {

  data <- GoFish(data, NULL)

  old.data <- GoFish(old.data, NULL)

  if (!is.null(old.data) & !is.null(data)) {
    data <- if (is.data.frame(data)) {
      dplyr::bind_rows(old.data, data) |>
        dplyr::distinct()
    } else if (is.double(data) | (is.list(data))) {
      c(old.data, data)
    } else if(is.character(data)) {
      paste(old.data, data, sep = sep)
    }
  }

  return (data)

}

#' @title LogCat
#' @keywords internal
#' @noRd
LogCat <- \(message = NULL,
            fatal = FALSE,
            flush = FALSE,
            trim = TRUE,
            width = 80,
            log = list(),
            append.log = TRUE,
            silent = FALSE) {

  # Trim message if trim is set to TRUE
  if (trim) message <- Trim(gsub("\r?\n|\r", " ", message))

  # if fatal stop function
  if (fatal) {
    stop(message, call. = FALSE)
  }

  # Print text if silent is set to FALSE
  if (!silent) {
    # flush console after each message if flush and trim is set to TRUE
    if (flush & trim) {
      cat("\r" , message[[1]], sep="")
      # Remove padding
      utils::flush.console()
      # if arrived is in message insert new line
      if (length(message)>1) {
        cat("\n")
      }
      # else trim message and print if trim is set to TRUE
    } else if (trim) {
      cat(message,"\n")
      # else print message as is
    } else {
      print(message, width = width)
      cat("\n")
    }
  }

  # Remove padding
  message <- Trim(gsub("\u2014", "", message))

  # Append to log if append.log set to TRUE else static
  log <- if (append.log) append(log, message) else message

  return (log)

}

#' @title SaveData
#' @keywords internal
#' @noRd
SaveData <- \(data,
              save.name,
              extension,
              save.path = NULL,
              append = FALSE) {

  # Define path and file
  file <- sprintf("%s.%s", save.name, extension)
  if (!is.null(save.path)) {
    # Create folder if it does not exist
    dir.create(file.path(save.path), showWarnings = FALSE, recursive = TRUE)
    file <- file.path(save.path, file)
  }

  # save as csv
  if (extension == "csv") {
    utils::write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
    # Save as text file
  } else {
    write(data, file = file, append = append)
  }

  return (file)

}

#' @title CleanText
#' @keywords internal
#' @noRd
CleanText <- \(x, multiline = FALSE) {

  # Trim original vector
  x <- Trim(x)

  # List of characters to remove
  character.vector <- c(".",",",":",";","-","--","\u2013","\u2014",
                        "[","]","{","}","=","&","/")
  remove.characters <- paste0("\\", character.vector, collapse="|")

  # Remove first character if unwanted
  first.character <- gsub(remove.characters, "", substring(x, 1, 1))
  # Put Humpty togheter again
  if (max(0,nchar(Trim(gsub("(\\s).*", "\\1", x)))) == 1) {
    x <- paste(first.character, Trim(substring(x, 2)))
  } else {
    x <- paste0(first.character, Trim(substring(x, 2)))
  }

  # Remove last character if unwanted
  last.character <- gsub(remove.characters, "", substr(x, nchar(x), nchar(x)))
  # Put Humpty togheter again
  x <- paste0(Trim(substr(x, 1, nchar(x)-1)), last.character)

  # Remove any #. (e.g., 1. Title)
  x <- Trim(gsub("^\\s*\\d+\\s*\\.", "", x))

  # Remove \r\n if multiline is set to FALSE
  if (!multiline) x <- Trim(gsub("\r?\n|\r", " ", x))

  # Remove HTML/XML tags
  x <- Trim(gsub("<.*?>|\ufffd|&lt;|&gt", "", x))

  # Remove any abstract from beginning of string
  if (any(grepl("abstract", tolower(substr(x, 1, 8))))) {
    x <- substring(x, 9)
  }

  # Remove NA
  x <- x[! x %in% c("NANA")]

  return (x)

}

#' @title Mime
#' @keywords internal
#' @noRd
Mime <- \(x, mime = FALSE, charset = FALSE) {

  # Remove charset information
  if (grepl("charset",x)) {
    x <- unlist(strsplit(x,";"))
    x.charset <- x[2]
    x <- x[1]
  }

  # Define Mime types
  data <- c(bib = "application/x-bibtex",
            csv = "text/csv",
            json = "application/json",
            html = "text/html;charset",
            json = "application/vnd.citationstyles.csl+json",
            xml = "application/mods+xml",
            pdf = "application/pdf",
            ris = "application/x-research-info-systems",
            rdf = "application/rdf+xml",
            xml = "text/xml",
            txt = "text/x-wiki")

  # Set data as either Mime or extension
  data <- if (!mime) names(data[data == x]) else data[[x]]

  # If no data set as json / txt
  if (!length(data)) {
    data <- if (mime) "application/json" else "txt"
  }

  # Append charset if Mime and charset is set to TRUE
  if (!mime & charset) data <- paste0(data,x.charset)

  return (data)

}

#' @title ReadCss
#' @keywords internal
#' @noRd
ReadCss <- \(data, css, clean.text = TRUE) {

  data <- data |>
    rvest::html_nodes(css) |>
    rvest::html_text()

  if (clean.text) data <- data |> CleanText()

  return (data)

}

#' @title ReadXpath
#' @keywords internal
#' @noRd
ReadXpath <- \(data, xpath, clean.text = TRUE) {

  data <- data |>
    rvest::html_nodes(xpath = xpath) |>
    rvest::html_text()

  if (clean.text) data <- data |> CleanText()

  return (data)

}

#' @title ReadAttr
#' @keywords internal
#' @noRd
ReadAttr <- \(data, xpath, attr) {

  data <- data |>
    rvest::html_nodes(xpath = xpath) |>
    rvest::html_attr(attr)

  return (data)

}

#' @title ParseUrl
#' @keywords internal
#' @noRd
ParseUrl <- \(x,
              format = "text",
              as = NULL,
              type = NULL,
              encoding = "UTF-8") {

  # Define parse method
  if (is.null(as)) {

    formats <- c(raw = "raw",
                 coins = "parsed",
                 csv = "parsed",
                 bookmarks = "parsed",
                 json = "text",
                 csljson = "text",
                 bibtex = "text",
                 biblatex = "text",
                 mods = "text",
                 refer = "text",
                 rdf_bibliontology = "text",
                 rdf_dc = "text",
                 rdf_zotero = "text",
                 ris = "text",
                 tei = "text",
                 wikipedia = "text",
                 txt = "text",
                 text = "text")

    as <- formats[format]
    if (is.na(as)) as <- "text"
  }

  # Parse data
  result <- httr::content(
    x, as, type, encoding, show_col_types = FALSE
  )

  # Remove empty json
  if (as == "text") {
    if (result == "[]") result <- NULL
  }

  return (result)

}

#' @title EditionFix
#' @keywords internal
#' @noRd
EditionFix <- \(edition) {

  # Some ISBN lists two edtions (e.g., 2nd ed. and global ed.)
  if (length(edition)>1) {
    edition <-   edition <- GoFish(
      sort(unique(unlist(lapply(edition, \(x) {
        x[grepl("\\d", EditionFix(x))]
      }))))[[1]]
    )
  }

  if (length(edition)) {
    # Convert English letters [1:20] to numeric
    English2Numeric <- \(word) {
      rank <- (c("first", "second", "third", "fourth", "fifth", "sixth",
                 "seventh", "eighth", "ninth", "tenth", "eleventh",
                 "twelfth", "thirteenth", "fourteenth", "fifteenth",
                 "sixteenth", "seventeenth", "eighteenth",
                 "nineteenth", "twentieth"))
      pos <- which(lapply(rank, \(x) grepl(x, tolower(word))) == TRUE)
      if (length(pos)) word <- pos[[1]]
      return (word)
    }

    # Convert Norwegian letters [1:20] to nureric
    Norwegian2Numeric <- \(word) {
      rank <- (c("f\u00f8rste", "andre", "tredje", "fjerde", "femte", "sjette",
                 "syvende", "\u00e5ttende", "niende", "tiende", "ellevte",
                 "tolvte", "trettende", "fjortende", "femtende",
                 "sekstende", "syttende", "attende",
                 "nittende", "tjuende"))
      pos <- which(lapply(rank, \(x) grepl(x, tolower(word))) == TRUE)
      if (length(pos)) word <- pos[[1]]
      return (word)
    }

    # Replace written edition with numeric
    if (!is.null(edition)) {
      if (!grepl("\\d", edition)) {
        edition <- English2Numeric(edition)
        edition <- Norwegian2Numeric(edition)
      }
      if (grepl("\\d", edition)) {
        # Extract numeric only from edition
        edition <-  suppressWarnings(
          as.numeric(gsub("[^0-9-]", "", edition))
        )
      }
    }

    # set edition as NA if first edition
    if (edition == "1") edition <- NA
  }

  return (as.character(edition))

}
