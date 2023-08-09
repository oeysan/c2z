#' @title Create a month-to-month bibliography of selected units
#' @description Create a bibliography in a newsletter format from month-to-month
#' From either a specified unit or a set of units (e.g., A University ->
#' Faculties -> Departments -> Groups). The function has the ability to create
#' HTML output designed for emails and/or webpages.
#' @param zotero What Zotero library to use
#' @param unit.key What unit to search for
#' @param style Citation style to use for appended bibliography and/or
#'   citations, Default: 'apa-single-spaced'
#' @param locale Desired language format of bibliography, Default: 'nn-NO'
#' @param start.date Results created from specified date (YYYY-MM),
#' Default: Sys.Date()
#' @param end.date Results created before specified date (YYYY-MM),
#' Default: NULL
#' @param use.filter Filter out specific items (otherwise supported by
#' `CristinWrangler`), Default: TRUE
#' @param filter Filter out specific item types, will default to item types
#' usually associated with NVI, Default: NULL
#' @param nvi Filter out Cristin items not 1/2 in NVI, Default: TRUE
#' @param user.cards Get business cards of contributors (currently INN),
#' Default: FALSE
#' @param use.identifiers Use if ISBN/DOI identifiers if enabled, Default: TRUE
#' @param zotero.check Check if Cristin items exists in Zotero library
#' , Default: FALSE
#' @param lang Set Cristin language (nn, nb, en), Default: 'nn'
#' @param post Post new items to specified Zotero library, Default: FALSE
#' @param create.email Create HTML output for email, Default: FALSE
#' @param create.web Create HTML output for web, Default: FALSE
#' @param subject Add a custom subject to email, Default: NULL
#' @param header Add a custom header to email, Default: NULL
#' @param footer Add a custom footer to email, Default: NULL
#' @param replace.style Email clients (e.g., Outlook) is poor at rendering CSS,
#' and the default setting is to remove hanging indent, Default: TRUE
#' @param width Width of email in pixels, Default: 700
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @param log A list for storing log elements, Default: list()
#' @return A list with bibliography and information for defined units
#' @details Please see
#' \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Dummy example
#'   newsletter <- CristinMonthly(
#'     Zotero(
#'       id = "4827927",
#'       api = "RqlAmlH5l1KPghfCseAq1sQ1"
#'     ),
#'     "209.0.0.0"
#'   )
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{arrange}},
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{bind_rows}},
#'  \code{\link[dplyr]{bind_cols}}, \code{\link[dplyr]{select}},
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{case_when}},
#'  \code{\link[dplyr]{context}}, \code{\link[dplyr]{distinct}},
#'  \code{\link[dplyr]{pull}}, \code{\link[dplyr]{mutate-joins}},
#'  \code{\link[dplyr]{join_by}}, \code{\link[dplyr]{rename}},
#'  \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}},
#'  \code{\link[dplyr]{mutate_all}}
#'  \code{\link[purrr]{pmap}}, \code{\link[purrr]{map}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[stats]{setNames}}
#' @rdname CristinMonthly
#' @export
CristinMonthly <- \(zotero,
                    unit.key,
                    style = "apa-single-spaced",
                    locale = "nn-NO",
                    start.date = Sys.Date(),
                    end.date = NULL,
                    use.filter = TRUE,
                    filter = NULL,
                    nvi = TRUE,
                    user.cards = FALSE,
                    use.identifiers = TRUE,
                    zotero.check = FALSE,
                    lang = "nn",
                    post = FALSE,
                    create.email = FALSE,
                    create.web = FALSE,
                    subject = NULL,
                    header = NULL,
                    footer = NULL,
                    replace.style = TRUE,
                    width = 700,
                    silent = FALSE,
                    log = list()) {

  # Visible bindings
  paths <- items <- multidepartmental <- duplicates <- duplicate.check <-
    email <- web <- key <- cristin.url <- zotero.url <- bib.item <- extra <-
    identifier <- duplicate <- core.key <- parentCollection <- name <-
    year.key <- desc <- core <- month.key <- title <- short.title <-
    destination.key <- root.path <- lang.month <- bib <- bib.body <-
    abstract <- NULL

  # Update user info @ inn if user.cards = TRUE
  if (user.cards) {

    # Log
    zotero$log <-  LogCat(
      "Updating INN database, please wait...",
      silent = silent,
      log = zotero$log
    )

    inn.users <- InnUsers()

  }

  # Function to create HTML bibliography
  GetBib <- \(...) {

    # Trim arguments
    args <- list(...)
    for (i in seq_along(args)) {
      arg <- if (is.character(args[[i]])) Trim(args[[i]]) else args[[i]]
      assign(names(args[i]), arg)
    }

    # Function to add URLs to bibliography
    AddMore <- \(cristin.url,
                 zotero.url,
                 bib.body,
                 item,
                 abstract,
                 url,
                 pages.url,
                 pages.path,
                 key) {

      # Visible bindings
      add.abstract <- add.personas <- pages <- NULL
      button.abstract <- button.personas <- button.url <- ""

      # Add url to buttons if any
      if (any(!is.na(url))) {
        button.url <- sprintf(
          "{{< button href=\"%s\" >}}%s{{< /button >}}",
          url,
          url.text)
      }

      # Create abstract if any
      if (!is.na(abstract)) {
        add.abstract <- sprintf(
          "<div class=\"cls-bib-abstract\"><h1>%s</h1>%s</div>",
          abstract.text,
          abstract
        )

        button.abstract <- sprintf(
          "{{< button class=\"abstract\" href=\"%s\" >}}%s{{< /button >}}",
          pages.url,
          abstract.text
        )
      }

      # Find buisiness cards at if user.cards i set to TRUE
      if (user.cards) {

        # Find Cristin id for contributors
        cristin.contributors <- CristinId(sub("^.*=", "", cristin.url))

        # Finn business cards of contributors at INN
        inn.cards <- inn.users |>
          dplyr::filter(cristin.id %in% cristin.contributors) |>
          dplyr::select(url) |>
          (\(x) if(nrow(x)) ToString(lapply(x$url, InnCards), ""))()

        if (!is.null(inn.cards)) {
          add.personas <- sprintf(
            "<div class=\"cls-bib-cards\"><h1>%s</h1>%s</div>",
            card.text,
            inn.cards
          )
          button.personas <- sprintf(
            "{{< button class=\"personas\" href=\"%s\" >}}%s{{< /button >}}",
            pages.url,
            card.text
          )
        }
      }

      # Create extras (e.g., Cristin & Zotero link) for email
      item.email <- sprintf(
        " [<a href=\"%s\">Cristin</a>&nbsp;|&nbsp;<a href=\"%s\">Zotero</a>]",
        cristin.url,
        zotero.url
      )
      # Create extras (e.g., Cristin & Zotero link) for web
      item.web <-  sprintf(
        "<div class=\"cls-bib-button\">
        {{< button href=\"%s\" >}}Cristin{{< /button >}}
        {{< button href=\"%s\" >}}Zotero{{< /button >}}%s%s%s
        </div>",
        cristin.url,
        zotero.url,
        button.url,
        button.personas,
        button.abstract
      )

      # Add extras to reference
      item.email <- sub("</div>", paste0(item.email, "</div>"), item)
      item.web <- sub("</div>", paste0(item.web, "</div>"), item)

      # Create individual pages if any abstract or business card
      if (!is.null(add.abstract) | !is.null(add.personas)) {

        page.html <- Trim(ToString(
          c(sprintf(bib.body, item.web),
            add.abstract,
            add.personas),
          "\n"))

        # Create page
        pages <- tibble::tibble(
          path = pages.path,
          key = key,
          html = page.html,
          archive = FALSE
        )

      }

      item <- tibble::tibble(
        email = item.email,
        web = item.web,
        pages = list(pages)
      )

      return(item)

    }

    # Find items in collection
    items <- dplyr::filter(zotero$items, grepl(month.key, collections))

    # Find corresponding bibliography
    bib <- zotero$bibliography |>
      dplyr::filter(key %in% items$key)

    # Arrange items
    items <- dplyr::arrange(items, match(key, bib$key))

    # Add Url to bibliography
    bib <- bib |>
      dplyr::mutate(
        cristin.url = paste0(
          "https://app.cristin.no/results/show.jsf?id=",
          purrr::pmap_chr(
            list(items$extra), ~
              ZoteroId("Cristin", .x, return.type = "")
          )
        ),
        zotero.url = sprintf(
          "http://zotero.org/%s/items/%s", items$prefix, key
        ),
        abstract = items$abstractNote,
        url = items$url,
        bib.item = purrr::pmap(
          list(cristin.url,
               zotero.url,
               bib.body,
               bib.item,
               abstract,
               url,
               pages.url = file.path(
                 paste0("/", tolower(pub.text)),
                 key
               ),
              pages.path = paste0("/", tolower(pub.text)),
              key = key
          ),
          ~ AddMore(...)
        )
      )

    # Combine to email bibliography
    email <- sprintf(
      bib$bib.body[[1]],
      ToString(lapply(bib$bib.item, \(x) x$email), "\n")
    )

    # Combine to web bibliography
    web <- sprintf(
      bib$bib.body[[1]],
      ToString(lapply(bib$bib.item, \(x) x$web), "\n")
    )

    # Combine individual pages
    pages <- dplyr::bind_rows(lapply(bib$bib.item, \(x) x$pages))

    # Combine all elements
    bibliography <- tibble::tibble(
      bib.email = email,
      bib.web = web,
      bib.pages = list(pages)
    )

    return(bibliography)

  }

  # Find collections
  zotero <- Zotero(
    user = zotero$user,
    id = zotero$id,
    api = zotero$api,
    library = TRUE,
    get.items = FALSE,
    silent = TRUE,
    log = zotero$log
  )

  # Define collections
  collections <- zotero$collections

  # Languages
  # Set language to en if not nb or nn
  if (!lang %in% c("nb", "nn", "no")) {
    lang <- "en"
    affiliation <- "Affiliated"
    more.text <- "Read more"
    abstract.text <- "Abstract"
    card.text <- "Contributors"
    url.text <- "Webpage"
    pub.text <- "Publications"
  } else {
    lang <- "no"
    affiliation <- "Affiliert"
    more.text <- "Les meir"
    abstract.text <- "Samandrag"
    card.text <- "Bidragsytarar"
    url.text <- "Nettside"
    pub.text <- "Publikasjonar"
  }

  # Define units
  units <- CristinUnits(unit.key, recursive = TRUE, lang = lang)

  # Post monthly data to Zotero library if post is TRUE
  if (post) {

    # use filter if use.filter = TRUE
    if (use.filter) {
      # use predined filter if filter is not defined
      if (is.null(filter)) {
        filter <- c(
          "ACADEMICREVIEW",
          "ARTICLEJOURNAL",
          "ARTICLE",
          "ANTHOLOGYACA",
          "CHAPTER",
          "CHAPTERACADEMIC",
          "CHAPTERARTICLE",
          "COMMENTARYACA",
          "MONOGRAPHACA"
        )
      }
    }

    # Format start.date as beginning of defined month
    start.date <- FloorDate(start.date)
    # Set end.date as start.date plus a month if end.date is NULL
    if (is.null(end.date)) {
      end.date <- ChangeDate(start.date, 1)
      # Else format end.date as beginning of defined month
    } else {
      end.date <- FloorDate(end.date)
      if (start.date >= end.date) end.date <- ChangeDate(start.date, 1)
    }

    # Create tibble with k periods equaling 1 month
    period <- seq(start.date, end.date, by = "months") |>
      (\(.) {
        tibble::tibble(
          start.date = .[1:(length(.)-1)],
          end.date = (.[2:length(.)] - 1) # Set end.date as last day of month
        )
      })()

    # Add period date to units data
    post.data <- dplyr::bind_rows(
      lapply(seq_len(nrow(units)), \(i) {
        dplyr::bind_cols(units[i, ], period)
      })
    )

    # Log number of units and periods
    log <-  LogCat(
      sprintf(
        "Cristin query based on %s from %s" ,
        Pluralis(nrow(units), "unit"),
        paste(
          format(start.date, format="%m.%Y"),
          format(end.date, format="%m.%Y"),
          sep = " to "
        )
      ),
      silent = silent,
      log = log
    )

    # Log
    zotero$log <-  LogCat(
      "Searching for new collections",
      silent = silent,
      log = zotero$log
    )

    # Start time for query
    query.start <- Sys.time()

    for (i in seq_len(nrow(post.data))) {

      # Current unit
      unit <- post.data[i, ] |>
        dplyr::select(dplyr::starts_with("path")) |>
        (\(x) x[!is.na(x)])()
      # Current month number
      month.i <- as.numeric(format(post.data$start.date[[i]], format="%m"))
      # Current month
      month <- Month(month.i, lang)
      # Current year
      year <- as.numeric(format(post.data$start.date[[i]], format="%Y"))
      # Define path name
      path.name <-    c(
        unit,
        # year
        year,
        # Month (e.g., 01: Januar)
        sprintf("%02d: %s", month.i, month)
      )

      # Find path to collection
      find.path <- FindPath(collections, path.name)

      # Add to path list
      paths <- append(paths, list(find.path$path))

      # Update data
      collections <- find.path$data

      # Estimate time of arrival
      log.eta <- LogCat(
        Eta(query.start, i, nrow(post.data)),
        silent = silent,
        flush = TRUE,
        log = log,
        append.log = FALSE
      )

    } # End for loop

    # Add to log
    zotero$log <- append(zotero$log, log.eta)

    # Append path to post data
    post.data$path <- paths

    # Fetch items if zotero.check is TRUE
    if (zotero.check) {

      # Find core location (i.e., where all items in units are stored)
      core.location <- collections |>
        dplyr::filter(version > 0 & parentCollection == FALSE) |>
        dplyr::select(key) |>
        dplyr::pull()

      # Fetch items
      if (length(core.location)) {
        zotero <- Zotero(
          zotero = zotero,
          collection.key = core.location,
          get.collections = length(core.location),
          library = TRUE,
          silent = TRUE,
          log = zotero$log
        )
      }

      # Set duplicate.check as zotero list
      duplicate.check <- zotero
    }

    # Log number of units and periods
    zotero$log <-  LogCat(
      "Searching Cristin",
      silent = silent,
      log = zotero$log
    )

    # Start time for query
    query.start <- Sys.time()

    # Cycle through post.data searching Cristin for each row
    ## Should perhaps vectorize, but for loop seems more informative tbh
    for (i in seq_len(nrow(post.data))) {

      # Searching Cristin
      cristin <- Cristin(
        unit = post.data$id[[i]],
        created_since = post.data$start.date[[i]],
        created_before = post.data$end.date[[i]],
        filter = filter,
        use.identifiers = FALSE,
        force = TRUE,
        silent = TRUE,
        zotero = duplicate.check,
      )$results

      # Remove items not in NVI if nvi is TRUE
      if (nvi & any(nrow(cristin))) {
        cristin <- AddMissing(cristin, "extra") |>
          dplyr::mutate(nvi = purrr::pmap_chr(
            list(extra), ~ ZoteroId("NVI", .x)
          )) |>
          dplyr::filter(nvi == 1 | nvi == 2) |>
          dplyr::select(-c(nvi))
      }

      # Run if any Cristin items remain
      if (any(nrow(cristin))) {

        # Search Cristin again if use.identifiers
        if (use.identifiers) {

          # Find remaining ids
          cristin.id <- as.vector(ZoteroId(
            "Cristin", cristin$extra, return.type = NULL
          ))

          # Search remaining ids
          cristin <- dplyr::bind_rows(
            lapply(cristin.id, \(x) {
              Cristin(
                id = x,
                use.identifiers = TRUE,
                force = TRUE,
                silent = TRUE,
                zotero = duplicate.check
              )$results
            })
          )
        } # End identifiers

        # Accumulate results
        items <- dplyr::bind_rows(
          items,
          # Append collections key to Cristin data
          dplyr::mutate(
            cristin, collections =  list(post.data$path[[i]])
          )
        )

      } # End Cristin results

      # Estimate time of arrival
      log.eta <- LogCat(
        Eta(query.start, i, nrow(post.data)),
        silent = silent,
        flush = TRUE,
        log = log,
        append.log = FALSE
      )

    } # End for loop

    # Add to log
    zotero$log <- append(zotero$log, log.eta)

    # Log number of units and periods
    zotero$log <- LogCat(
      sprintf(
        "Found %s new, or modfied, %s",
        max(0,nrow(items)),
        Pluralis(max(0,nrow(items)), "item", prefix = FALSE)
      ),
      silent = silent,
      log = zotero$log
    )

    # Find multidepartmental and duplicate items if any items
    if (any(nrow(items))) {
      items <- AddMissing(
        items, c("ISBN", "DOI", "extra"), na.type = NA_character_
      ) |>
        # Group by extra and identify identical extra fields
        dplyr::group_by(extra) |>
        dplyr::mutate(
          # Merge collections for identical results
          collections = dplyr::case_when(
            dplyr::n() > 1 ~ list(unique(unlist(collections))),
            TRUE ~ collections
          ),
          # Identify number of identifical fields
          # These are most likely multidepartmental
          multidepartmental = dplyr::n() - 1
        )  |>
        # Remove duplicates
        dplyr::distinct(extra, .keep_all = TRUE) |>
        dplyr::ungroup() |>
        # Create an identifier field based on DOI and ISBN
        dplyr::mutate(
          identifier = dplyr::case_when(
            !is.na(ISBN) ~ Trim(gsub('[^[:alnum:] ]', "", ISBN)),
            !is.na(DOI) ~ DOI,
            TRUE ~ NA
          ),
          # Create short.title to check for duplicates
          short.title = ComputerFriendly(title, remove.after = TRUE)
        ) |>
        # Group by identifiers and find duplicates
        dplyr::group_by(identifier, short.title) |>
        dplyr::mutate(
          # Add to extra field the duplicated id
          extra = dplyr::case_when(
            dplyr::n() > 1 & !is.na(identifier) ~ AddAppend(
              paste("Duplicate ID:", identifier), extra, "\n"
            ),
            TRUE ~ extra
          ),
          # Find number of duplicated datapoints
          duplicate = dplyr::case_when(
            dplyr::n() > 1 & !is.na(identifier) ~ 1,
            TRUE ~ 0
          )
        ) |>
        dplyr::ungroup()

    # Log number multidepartmental items
    if (sum(items$multidepartmental)) {
      zotero$log <-  LogCat(
        sprintf(
          "Merged %s multidepartmental %s (see `$multidepartmental`)",
          sum(items$multidepartmental),
          Pluralis(
            sum(items$multidepartmental), "publication", prefix = FALSE
          )
        ),
        silent = silent,
        log = zotero$log
      )

      # Find Cristin ID of multidepartmental items
      multidepartmental <- items |>
        dplyr::filter(multidepartmental > 0) |>
        dplyr::pull(extra) |>
        (\(x) ZoteroId("Cristin", x))()
    }

    # Log number of duplicated items
    if (sum(items$duplicate)) {
      zotero$log <-  LogCat(
        sprintf(
          "Found %s possible duplicated %s (see `$duplicates`)" ,
          sum(items$duplicate),
          Pluralis(
            sum(items$duplicate), "publication", prefix = FALSE
          )
        ),
        silent = silent,
        log = zotero$log
      )

      # Find Cristin ID of duplicated items
      duplicates <- items |>
        dplyr::filter(duplicate > 0) |>
        dplyr::pull(extra) |>
        (\(x) ZoteroId("Cristin", x))()

    }

    # Remove added columns
    zotero$items <- items |>
      dplyr::select(-c(multidepartmental, duplicate, identifier, short.title))

    # Remove collections not found in Cristin query
    zotero$collections <- collections |>
      dplyr::filter(key %in% unique(unlist(zotero$items$collections)))

    # Post Cristin items to specified collections
    zotero <- ZoteroPost(zotero, silent = silent, force = TRUE)

    } # End add items

    # Define updated collections
    collections <- AddMissing(collections, "prefix", NA_character_) |>
      dplyr::rows_update(zotero$collections, by = "key") |>
      dplyr::filter(version > 0) |>
      dplyr::distinct(key, .keep_all = TRUE)

  } # End post

  # Find Zotero locations of units
  unit.paths <- units |>
    dplyr::mutate(
      core.key = purrr::pmap_chr(
        dplyr::across(core),
        ~ tail(FindPath(collections, c(...))$path, 1)
      ),
      destination.key = purrr::pmap_chr(
        dplyr::across(dplyr::starts_with("path")),
        ~ tail(FindPath(collections, c(...))$path, 1)
      ),
      n.units = purrr::pmap_dbl(
        dplyr::across(dplyr::starts_with("path")),
        ~ sum(!is.na(c(...))))
    )

  # Find items
  zotero <- Zotero(
    collection.key = unit.paths$core.key[[1]],
    user = zotero$user,
    id = zotero$id,
    api = zotero$api,
    library = TRUE,
    library.type = "data,bib",
    style = style,
    locale = locale,
    silent = silent,
    log = zotero$log
  )

  # Return if no collections
  if (is.null(collections) | is.null(zotero$items)) {
    return (zotero)
  }

  # Log monthly bibliographies for Cristin
  zotero$log <-  LogCat(
    "Creating monthly bibliographies",
    silent = silent,
    log = zotero$log
  )

  # Create monthly bibliographies for Cristin per unit
  cristin.monthly <- dplyr::left_join(
    unit.paths,
    collections,
    dplyr::join_by(destination.key == parentCollection)
  ) |>
    dplyr::rename(year = name, year.key = key) |>
    dplyr::left_join(
      collections, dplyr::join_by(year.key == parentCollection)
    ) |>
    dplyr::rename(month = name, month.key = key) |>
    dplyr::select(
      -c(dplyr::starts_with("prefix"), dplyr::starts_with("version"))
    ) |>
    dplyr::filter(!is.na(month.key)) |>
    dplyr::arrange(
      desc(year),
      desc(month),
      dplyr::across(
        dplyr::starts_with("path"),
        ~ data.frame(!is.na(.x), !.x %in% affiliation, .x)
      )
    ) |>
    dplyr::mutate(
      year.month = paste0(year, "_" , month),
      month = as.numeric(gsub(":.*", "", month)),
      lang.month = purrr::map_chr(month, ~ Month(.x, lang)),
      root.path = purrr::pmap_chr(dplyr::across(dplyr::starts_with("path")), ~ {
        lapply(c(...), \(x) {
          ComputerFriendly(x)
        }) |>
          (\(x) do.call(file.path, x))()
      }),
      bib = purrr::pmap(
        list(month.key = month.key,
             root.path = root.path,
             year = year,
             month = lang.month,
             user.cards = user.cards), ~
          GetBib(...),
        .progress = !silent
      )
    ) |>
    tidyr::unnest(bib)

  return(
    list(
      units = units,
      monthly = cristin.monthly,
      multidepartmental = multidepartmental,
      duplicates = duplicates,
      email = email,
      web = web,
      zotero = zotero
    )
  )

}
