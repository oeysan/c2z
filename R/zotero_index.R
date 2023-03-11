#' @title Create an index of Zotero items
#' @description The function creates a index containing key information about the present Zotero items
#' @param data Tibble containing Zotero-type metadata (e.g., from Cristin)
#' @return A tibble
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}},
#'  \code{\link[dplyr]{across}}, \code{\link[dplyr]{na_if}},
#'  \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{arrange}},
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{context}},
#'  \code{\link[dplyr]{select}}
#'  \code{\link[tidyselect]{everything}}
#'  \code{\link[purrr]{pmap}}
#' @rdname ZoteroIndex
#' @export
ZoteroIndex <- \(data) {

  # Visible bindings
  creators <- title <- parentItem <- key <- itemType <- name <-
    dateModified <- initials <- creatorType <- first.creator <- NULL

  # Function to find and format (last-) name
  CreatorName <- \(data, i, initials = FALSE, full.initials = FALSE) {

    # initial return
    name <- NA_character_

    # Run if creators list is not empty
    if (any(lengths(data))) {

      # Check if creator has first name
      name <- GoFish(data[i, ]$lastName)
      # Else do a Cher
      if (is.na(name)) name <- GoFish(data[i, ]$name)
      # Add initials if initials = TRUE and exists
      if (initials & !is.na(GoFish(data[i, ]$firstName))) {
        # Split first.name by space and keep first letter of each part
        initials <- strsplit(data[i, ]$firstName, " ")[[1]] |>
          substr(1, 1)
        # If full.initials is set to FALSE keep only first initial
        if (!full.initials) initials <- initials[[1]]
        # Append initials to name
        name <- sprintf("%s. %s", paste(initials, collapse = ". "), name)
      }

    }

    return (name)

  }

  EtAl <- \(data, initials = FALSE, full.initials = TRUE) {

    # initial return
    creators <- NA_character_

    # Run if creators list is not empty
    if (any(lengths(data))) {

      # Filter out authors if author exist in tibble
      data <- data |>
        dplyr::filter(dplyr::case_when(
          "author" %in% creatorType ~ grepl("author", creatorType),
          TRUE ~ !grepl("contributor", creatorType)
        ))

      # Use only first authors if only one authors
      if (nrow(data) == 1) {
        creators <- CreatorName(data, 1, initials, full.initials)
        # Else separate authors by & if two authors
      } else if (nrow(data) == 2) {
        creators <- sprintf(
          "%s & %s",
          CreatorName(data, 1, initials, full.initials),
          CreatorName(data, 2, FALSE, FALSE)
        )
        # Else first author followed by et al if more than two authors
      } else if (nrow(data) > 2) {
        creators <- sprintf(
          "%s et al.",
          CreatorName(data, 1, initials, full.initials)
        )
      }

    }

    return (creators)

  }


  # Function to create a title (without a, an, the prefix)
  TitleOrder <- \(title) {

    # Run if not empty
    if (all(!is.na(GoFish(title)))) {

      check.title <- unlist(strsplit(title, " "))

      # If first words is a, an or the move word to end of string.
      stop.words <- c("a", "an", "the")
      if (tolower(check.title[1]) %in% stop.words) {

        # Set second word to upper-case
        check.title[2] <- paste0(
          toupper(substr(check.title[2], 1, 1)),
          substr(check.title[2], 2,
                 nchar(check.title[2]))
        )

        title <- sprintf(
          "%s, %s",
          Trim(ToString(check.title[-c(1)], " ")),
          check.title[1]
        )
      }

    }

    return (title)

  }

  # Function to remove html characters from note
  NoteContent <- \(note, i = 20) {

    if (all(!is.na(note))) {
      # Remove html characters
      note <- gsub("<.*?>|[\r\n]", "", note)
      # Run if note is not empty
      if (!is.na(note)) {
        # Add ellipsis and trim string if longer than i
        if (nchar(note) > i) note <- paste0(substr(note, 1, i), "...")
      }
    }

    return (TitleOrder(note))

  }

  # Add columns to avoid errors
  index <- AddMissing(data,
                      c("key",
                        "itemType",
                        "parentItem",
                        "creators",
                        "note",
                        "date",
                        "title"),
                      na.type = "") |>
    # Replace empty string with NA
    dplyr::mutate_if(is.character, list(~na_if(.,""))) |>
    dplyr::mutate(
      # Create creators citation with first initial only
      initials = purrr::pmap_chr(
        list(creators), ~ CreatorName(.x, 1, TRUE, FALSE)
      ),
      first.creator = purrr::pmap_chr(
        list(creators), ~ CreatorName(.x, 1)
      ),
      title = case_when(
        # Create title from note content
        itemType == "note" ~ purrr::pmap_chr(list(note), NoteContent),
        # Else run TitleOrder function
        TRUE ~ purrr::pmap_chr(list(title), TitleOrder)
      ),
      # Extract year (four digits) from date field
      date = sub('.*?(\\d{4}).*', '\\1', date)
    ) |>
    group_by(first.creator) |>
    mutate(
      creators = case_when(
        is.na(first.creator) ~ "N.A.",
        dplyr::n_distinct(initials) > 1 ~ purrr::pmap_chr(
          list(creators), ~ EtAl(.x, TRUE, TRUE)
        ),
        TRUE ~ purrr::pmap_chr(list(creators), EtAl)
      )
    ) |>
    # Ungroup fo fix titles
    ungroup () |>
    # Order data as title to create correct sequence of publications
    dplyr::arrange(title) |>
    # Group by authors and date
    dplyr::group_by(creators, date) |>
    dplyr::mutate(
      # Edit date field
      date = dplyr::case_when(
        is.na(date) ~ "n.d.",
        # Append a,b,c etc. to date for publications by authors with same year
        dplyr::n() > 1 ~ paste0(date, letters[dplyr::row_number()]),
        # Do not change date if no duplicates are found
        TRUE ~ date
      )
    ) |>
    # Ungroup tibble to move on to the extras (i.e., notes, attachments)
    dplyr::ungroup() |>
    # Group by parent.item
    dplyr::group_by(parentItem) |>
    dplyr::mutate(
      title = dplyr::case_when(
        # Append standalone note if note type items has no parent
        is.na(parentItem) & itemType == "note" ~ sprintf(
          "%s (Standalone Note)", title
        ),
        # Else use default title
        is.na(parentItem) ~ title,
        # Else set appendix number and type as prefix to items with parents
        !is.na(parentItem) & dplyr::n() > 1 ~
          sprintf("Appendix #%s: %s (%s)",
                  dplyr::row_number(),
                  title,
                  itemType),
        # Else if there are not multiple extras set single appendix as prefix
        TRUE ~ sprintf("Appendix: %s (%s)", title, itemType)
      )) |>
    # Ungroup to add author and date fields to extras
    dplyr::ungroup() |>
    dplyr::mutate(
      creators = dplyr::case_when(
        # Use authors from parent
        !is.na(parentItem) ~ creators[match(parentItem, key)],
        TRUE ~ creators
      ),
      date = dplyr::case_when(
        # Else use date from parent
        !is.na(parentItem) ~ date[match(parentItem, key)],
        TRUE ~ date
      )
    ) |>
    # Name column
    dplyr::mutate(name = sprintf("%s (%s) %s", creators, date, title)) |>
    # Select columns
    dplyr::select(key, parentItem, itemType, name, creators, date, title) |>
    # Sort by name with attachments/notes at bottom of table
    dplyr::arrange(
      itemType == "attachment" | itemType == "note"
    )

  return (index)

}
