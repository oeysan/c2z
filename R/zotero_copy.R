#' @title Copy collections and items from a Zotero library
#' @description Replace key identifiers with new ones while keeping existing
#'   structure and relationship between collections and items
#' @param zotero A list with information on the specified Zotero library (e.g.,
#'   id, API key, collections, and items)
#' @param copy.collections Try to copy specified collections, Default: TRUE
#' @param copy.items Try to copy specified items?, Default: TRUE
#' @param copy.extras Try to copy specified extras (i.e., attachments and
#'   notes)?, Default: TRUE
#' @param remove.missing Deleted missing extras, Default: TRUE
#' @param change.library Stage changing of library (e.g., from a group to a
#'   personal library), Default: FALSE
#' @param copy.user New user type (The functions will use `group` as prefix if
#'   FALSE), Default: TRUE
#' @param copy.id New id, Default: NULL
#' @param copy.api New API key, Default: NULL
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return A list with information on the specified Zotero library (e.g., copied
#'   collections and items)
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \donttest{
#'   # Fetching collections and items from default group
#'   zotero = Zotero(
#'     user = FALSE,
#'     id = "4827927",
#'     api = "RqlAmlH5l1KPghfCseAq1sQ1",
#'     library = TRUE
#'   )
#'
#'   # Display collections
#'   print(zotero$collections, width = 80)
#'
#'   # Display items
#'   print(zotero$items, width = 80)
#'
#'   # Copy items
#'   example <- ZoteroCopy(
#'     zotero,
#'   )
#'
#'   # Display new keys and version for collections
#'   example$collections |>
#'     dplyr::select(key, version, parentCollection)
#'
#'   # Display new keys and version for items
#'   example$items |>
#'     dplyr::select(key, version)
#' }
#' @seealso
#'  \code{\link[httr]{GET}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{case_when}},
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}},
#'  \code{\link[dplyr]{across}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[purrr]{pmap}}
#'  \code{\link[tidyselect]{all_of}}
#' @rdname ZoteroCopy
#' @export
ZoteroCopy <- \(zotero,
                copy.collections = TRUE,
                copy.items = TRUE,
                copy.extras = TRUE,
                remove.missing = TRUE,
                change.library = FALSE,
                copy.user = TRUE,
                copy.id = NULL,
                copy.api = NULL,
                silent = FALSE) {

  # Visible bindings
  collections <- itemType <- key <- parentItem <- filename <- md5 <-
    mtime <- size <- prefix <- NULL

  # Function to replace keys
  ReplaceKeys <- \(key, new.keys, old.keys, return.value = NA) {
    key <- new.keys[match(key,old.keys)]
    key <- key[!is.na(key)]
    if (!length(key)) key <- return.value
    return (key)
  }

  # Function to check attachments
  CheckAttachment <- \(item.key, zotero) {
    # Set file as NA
    # Check file
    file <- NA
    check <- httr::RETRY(
      "GET",
      ZoteroUrl(zotero$url,
                item.key = item.key,
                api = zotero$api,
                use.item = TRUE,
                append.file = TRUE),
      quiet = TRUE)
    # Return file if status is 200
    if (check$status_code==200) file <- check
    return (file)
  }

  if (copy.collections & !is.null(zotero$collections)) {

    # Add to log
    zotero$log <- LogCat("Copying collections",
                         silent = silent,
                         log = zotero$log)

    # Defne keys
    new.keys <- replicate(nrow(zotero$collections), ZoteroKey())
    old.keys <- zotero$collections$key

    # Replace keys
    zotero$collections <- zotero$collections |>
      dplyr::arrange(version) |>
      dplyr::mutate(
        key = new.keys, # Set new keys
        version = 0, # Set initial version
        # Replace parent keys
        parentCollection = dplyr::case_when(
          parentCollection == "FALSE" ~ "FALSE",
          TRUE ~ key[match(parentCollection, old.keys)]
        )
      )

    # Set new collection main key
    if (!is.null(zotero$collection.key)) {
      zotero$collection.key <- new.keys[match(
        zotero$collection.key,
        old.keys
      )]
    }

    # Replace collections in items
    if (copy.items & !is.null(zotero$items)) {
      zotero$items <- zotero$items |>
        dplyr::mutate(
          collections = purrr::pmap(
            list(collections), ~ ReplaceKeys(., new.keys, old.keys, NULL)
          )
        )
    }
  }

  if (copy.items & !is.null(zotero$items)) {

    # Add to log
    zotero$log <- LogCat("Copying items",
                         silent = silent,
                         log = zotero$log)

    # Define keys
    new.keys <- replicate(nrow(zotero$items), ZoteroKey())
    old.keys <- zotero$items$key

    # Check attachments if check.attachments is set to TRUE
    if (copy.extras & "filename" %in% names(zotero$items)) {

      # Add to log
      zotero$log <- LogCat("Checking for missing attachments",
                           silent = silent,
                           log = zotero$log)

      # Define attachments
      zotero$attachments <- zotero$items |>
        dplyr::filter(grepl("attachment", itemType)) |> # Filter attachments
        dplyr::mutate(
          # Check attachments
          file = purrr::pmap(list(key), ~ CheckAttachment(., zotero)),
          # Set new keys
          key = new.keys[match(key, old.keys)],
          parentItem = new.keys[match(parentItem, old.keys)],
          # Find size of attachments
          size = purrr::pmap_chr(
            list(file), \(x)
            if (!all(is.na(x))) x$headers$`content-length` else NA_character_
          )
        ) |>
        # Select needed columns for upload
        dplyr::select(key, parentItem, filename, file,
                      md5, mtime, size, prefix)

    }

    # Replace keys
    zotero$items <- zotero$items |>
      dplyr::mutate(
        # Set new keys
        key = new.keys[match(key, old.keys)],
        dplyr::across(
          tidyselect::any_of("parentItem"), ~ new.keys[match(., old.keys)]
        ),
        version = 0, # Set initial version
      ) |>
      # Remove attachments and notes if copy.extras is set to FALSE
      dplyr::filter(
        if (!copy.extras) !grepl('attachment|note', itemType) else TRUE
      )

    # Remove missing attachments if remove.missing is set to TRUE
    if (remove.missing & !is.null(zotero$attachments)) {

      # Add to log
      zotero$log <- LogCat("Checking for missing attachments",
                           silent = silent,
                           log = zotero$log)

      # Find missing attachments / attachments with missing parents
      missing.attachments <- is.na(zotero$attachments$file)
      missing.parent <- !zotero$attachments$parentItem %in% new.keys

      # Find anyone missing
      any.missing <- missing.attachments | missing.parent

      # Find key of missing elements
      missing.key <- zotero$attachments$key[any.missing]

      # Remove missing from items
      zotero$items <- zotero$items |>
        dplyr::filter(!new.keys %in% missing.key)

      # Remove missing from attachments
      zotero$attachments <- zotero$attachments |>
        dplyr::filter(!any.missing)
      if (nrow(zotero$attachments)==0) zotero$attachments <- NULL

      # Number of missing attachments / attachments with missing parents
      n.missing.parent <- max(0,sum(missing.parent))
      n.missing.attachments <- max(0,sum(missing.attachments))

      # MORE POINTLESS ... you know
      missing.parent.message <- Pluralis(
        n.missing.parent, "attachment", "attachments", FALSE
      )

      # Add to log
      zotero$log <- LogCat(sprintf("Removed %s forsaken %s",
                                   n.missing.parent,
                                   missing.parent.message),
                           silent = silent,
                           log = zotero$log)

      # MORE POINTLESS ... you know
      missing.attachment.message <- Pluralis(
        n.missing.attachments, "attachment", "attachments", FALSE
      )

      # Add to log
      zotero$log <- LogCat(sprintf("Removed %s missing %s",
                                   n.missing.attachments,
                                   missing.attachment.message),
                           silent = silent,
                           log = zotero$log)

    }

    # Create a copy destination if change.library is set to TRUE
    if (change.library) {
      # Remove user
      zotero$user <- NULL
      zotero <- Zotero(user = copy.user,
                       id = copy.id,
                       api = copy.api,
                       silent = silent,
                       log = zotero$log,
                       zotero = zotero)
    }

  }

  return (zotero)

}
