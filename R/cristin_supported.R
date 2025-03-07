#' @title Check for supported export formats from Cristin to Zotero
#' @description Used to filter Cristin categories that are not supported
#' @param data Zotero-type matrix, Default: NULL
#' @param items Items in library to check for
#' @param zotero.check Should the function look for duplicates?, Default: TRUE
#' @param remove.na Cristin contains many, more or less, obscure categories, and
#'   not all are (yet) supported. By default these are removed, however, if this
#'   option is set to FALSE unsupported categories are treated according to
#'   replace.na, Default: TRUE
#' @param replace.na May the odds be in your favor and replace unsupported
#'   categories with a predefined itemType if remove.na is set to false,
#'   Default: 'book'
#' @param force.type Force all items to a predefined itemType, Default: NULL
#' @param remove.duplicates Remove duplicates if TRUE, Default: TRUE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @param log A list for storing log elements, Default: list()
#' @return Zotero supported items with unsupported categories as NA
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' # Supported Cristin items
#' print(CristinSupported(), n = 5)
#' @seealso
#'  \code{\link[tibble]{tibble}}
#' @rdname CristinSupported
#' @export
CristinSupported <- \(data = NULL,
                      zotero.check = TRUE,
                      items = NULL,
                      remove.na = TRUE,
                      replace.na = "book",
                      force.type = NULL,
                      remove.duplicates = TRUE,
                      silent = FALSE,
                      log = list()) {

  # Visible bindings
  unsupported.id <- missing.id <- title <- date_published <- year_published <-
    firstName <- creatorType <- lastName <- filter.name <- external.data <-
    results <- id <- name <- category <- key <- . <- NULL

  # Set replace.na as force.type if defined
  if (!is.null(force.type)) replace.na <- force.type

  # Currently supported references
  supported.types <- c(
    anthologyaca= "book",
    booktransl = "book",
    monographaca= "book",
    commentaryaca= "book",
    textbook= "book",
    nonfictionbook= "book",
    popularbook= "book",
    chapteracademic= "bookSection",
    populararticle= "bookSection",
    chapter= "bookSection",
    chapterarticle= "bookSection",
    foreword= "bookSection",
    introduction= "bookSection",
    software= "computerProgram",
    article= "journalArticle",
    academicreview= "journalArticle",
    editorial= "journalArticle",
    bookreview= "journalArticle",
    articlejournal= "journalArticle",
    readeropinion= "newspaperArticle",
    articlefeature= "newspaperArticle",
    articlepopular= "newspaperArticle",
    academiclecture= "presentation",
    poster= "presentation",
    lecture= "presentation",
    lecturepopular= "presentation",
    otherpres= "presentation",
    report= "report",
    doctordissertat= "thesis",
    thesismaster= "thesis",
    thesissecond= "thesis",
    thesismagister= "thesis",
    medicalthesis= "thesis"
  )

  # If data is null return list of supported references
  if (is.null(data)) {
    data <- tibble::tibble(
      cristin.category = toupper(names(supported.types)),
      zotero.types = supported.types
    )

    return (data)

  }

  # Check whether references exists in Zotero library if zotero.check is TRUE
  if (any(nrow(data)) & zotero.check & !is.null(items)) {

    check <- ZoteroCheck(
      data,
      id = "cristin_result_id",
      id.type = "Cristin",
      created = "created",
      last.modified = "last_modified",
      items = items,
      remove.duplicates = remove.duplicates,
      silent = silent,
      log = log
    )

    # Set data as checked data
    data <- check$data

    # Add to log
    log <- check$log

  }

  # Return if all all duplicates
  if (!any(nrow(data))) {
    return(list(data = data, log = log))
  }

  # Define Cristin category type
  data$type <- GoFish(data$category$name$en)

  # Define Cristin category code
  data$code <-  GoFish(data$category$code)

  # Checking references message
  log <-  LogCat(
    "Checking whether references are supported. See `CristinSupported()`",
    silent = silent,
    log = log
  )

  # Find supported data
  supported.data <- data |>
    dplyr::filter(
      tolower(category$code) %in% names(supported.types)
    ) |>
    # Replace Cristin categories with supported Zotero types
    dplyr::mutate(
      category = supported.types[
        match(tolower(category$code), names(supported.types))
      ]
    )

  # Find unsupported data
  unsupported.data <- data |>
    dplyr::filter(
      !tolower(category$code) %in% names(supported.types)
    )

  # Run if there are unsupported data
  if (nrow(unsupported.data)) {

    # Find Cristin ids of unsupported references
    unsupported.id <- unsupported.data$cristin_result_id

    # Remove unsupported data if remove.na = TRUE
    if (remove.na) {

      # Removal of unsupported references message
      unsupported.message <- sprintf(
        "Removed %s unsupported %s",
        nrow(unsupported.data),
        Numerus(nrow(unsupported.data), "reference", "references", FALSE)
      )

      # Else replace with replace.na
    } else if (!is.null(replace.na)) {

      # Combine unsupported and supported data and replace category
      supported.data <- dplyr::bind_rows(
        supported.data,
        unsupported.data |>
          dplyr::mutate(category = replace.na,
                        part_of = NA)
      )

      # Converted unsupported references message
      unsupported.message <- sprintf(
        "Set %s unsupported %s to `%s`",
        nrow(unsupported.data),
        Numerus(
          nrow(unsupported.data), "reference", "references", FALSE
        ),
        replace.na
      )

    }

    # Log unsupported data
    log <-  LogCat(
      paste(unsupported.message, "See `$unsupported.id`"),
      silent = silent,
      log = log
    )

  }

  # Set data as supported data
  data <- supported.data

  # Checking missing data message
  log <-  LogCat(
    "Looking for missing data",
    silent = silent,
    log = log
  )

  # filter out data missing title, and/or date
  missing.data <- data |>
    tidyr::unnest(title, names_sep = "_") |>
    dplyr::filter(!dplyr::if_all(c(dplyr::starts_with("title")), is.na),
                  GoFish(!is.na(date_published)) |
                    GoFish(!is.na(year_published))) |>
    tidyr::nest(title = dplyr::starts_with("title"), .names_sep = "_")

  # Check if missing.data has fewer entries than data
  if (nrow(data) > nrow(missing.data)) {

    # Find Cristin ids of unsupported references
    missing.id <- missing.data$cristin_result_id

    # Log message if any data is filtered out
    log <-  LogCat(
      sprintf(
        "Removed %s with missing data. See `$missing.id`",
        Numerus(nrow(data)-nrow(missing.data), "item", "items")
      ),
      silent = silent,
      log = log
    )
    # Set new data
    data <- missing.data

  }

  return (
    list(
      data = data,
      unsupported.id = unsupported.id,
      missing.id = missing.id,
      log = log
    )
  )

}
