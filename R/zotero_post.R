#' @title Post collections and items to a Zotero library
#' @description Create or update collections and items in a specified library
#' @param zotero A list with information on the specified Zotero library (e.g.,
#' id, API key, collections, and items)
#' @param post.collections Try to copy specified collections, Default: TRUE
#' @param post.items Try to copy specified items?, Default: TRUE
#' @param post.attachments Try to copy specified extras (i.e., attachments and notes)?, Default: TRUE
#' @param post.limit Number of collections/items to post per request (max 50), Default: 50
#' @param force Force is seldom wise, but sometimes..., Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @return A list with information on the specified Zotero library (e.g., posted collections and items)
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Delete everything in a group
#'     example <- ZoteroPost(
#'       Zotero(user = FALSE, id = "4988497", doi = "10.1126/sciadv.abd1705"),
#'       post.collections = FALSE,
#'     )
#'   }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{bind}},
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[httr]{add_headers}}, \code{\link[httr]{RETRY}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[tibble]{as_tibble}}, \code{\link[tibble]{add_column}}
#'  \code{\link[utils]{stack}}
#'  \code{\link[stats]{setNames}}
#' @rdname ZoteroPost
#' @export
ZoteroPost <- \(zotero,
                post.collections = TRUE,
                post.items = TRUE,
                post.attachments = TRUE,
                post.limit = 50,
                force = FALSE,
                silent = FALSE) {

  # Post collections/items function
  Upload <- \(zotero,
              post.collections,
              post.items,
              post.limit,
              force,
              silent) {

    # Visible bindings
    prefix <-  desc <- ind <- status <- values <- NULL

    # Define data as collection if append.collection is set to TRUE
    if (post.collections) {
      data <- zotero[["collections"]]
      post.items <- FALSE
      # Else define data as items
    } else {
      data <- zotero[["items"]]
    }

    # Stop function and send error message if there is no data to POST
    if (all(is.na(GoFish(data)))) {
      zotero$log <- LogCat(
        "There is no data to POST",
        fatal = TRUE,
        log = zotero$log
      )
    }

    # Send error if posting to same prefix and force is set to FALSE
    if ("prefix" %in% names(data)) {
      if (!force &
          zotero$prefix == data$prefix[[1]] &
          data$version[[1]] == 0) {
        zotero$log <- LogCat(
          "You are posting items to their initial location
        (i.e. creating duplicates). Please set force to TRUE if this is your
        intent.",
          fatal = TRUE,
          log = zotero$log
        )
      }
      # Remove prefix from data
      data <- data |> dplyr::select(-prefix)
    }

    #Number of items/collections
    total.data <- nrow(data)

    # Split metadata into acceptable chunks if k > post.limit
    metadata <- SplitData(data, post.limit)

    # JUST SOME MORE POINTLESS LINGUISTICS
    if (post.collections) {
      items <- Pluralis(total.data, "collection", "collections")
    } else {
      items <- Pluralis(total.data, "item", "items")
    }
    posts <- Pluralis(length(metadata), "POST request", "POST requests")

    # Create message
    upload.message <- sprintf("Adding %s to library using %s", items, posts)

    # Add message to log
    zotero$log <- LogCat(
      upload.message, silent = silent,
      log = zotero$log
    )

    # create summary list
    summary <- list()

    # Start time for query
    query.start <- Sys.time()
    # Cycle through metadata
    for (i in 1:length(metadata)) {

      # Create header for JSON with token
      json.header <- httr::add_headers(
        "Content-Type" = "application/json",
        "Zotero-Write-Token" = ZoteroKey(token = TRUE)
      )

      # Convert to JSON
      json.body <- ZoteroToJson(metadata[[i]])

      # Post JSON to url defined in zotero list
      json.post <- httr::RETRY(
        "POST",
        ZoteroUrl(zotero$url,
                  append.collections = post.collections,
                  append.items = post.items,
                  api = zotero$api),
        json.header,
        body = json.body,
        quiet = TRUE
      )

      # Convert results to list
      json.data <- jsonlite::fromJSON(
        ParseUrl(json.post, "text")
      )

      # Fetch id from data, keep only success, unchanged, and failure
      id <- as.numeric(
        unlist(lapply(json.data[-1], \(x) names(x)))
      )+1

      # Create summary
      summary[[i]] <- utils::stack(
        lapply(json.data[-1], as.character)
      ) |>
        tibble::as_tibble() |>
        dplyr::rename("status" = ind) |>
        dplyr::mutate(key = metadata[[i]]$key[id]) |>
        dplyr::select(-values) |>
        dplyr::arrange(desc(status))

      # Update version of successful uploads.
      if (length(json.data$successful)) {
        # Find new version of successful uploads
        new.version <- dplyr::bind_rows(
          lapply(json.data$successful, \(x) {
            data.frame(key = x$data$key,
                       version = x$data$version)
          })
        )

        # Set new version
        metadata[[i]] <- metadata[[i]] |>
          dplyr::rows_update(new.version, by = "key")

      }

      # Set error message for failed uploads
      if (length(json.data$failed)) {
        failed.message <- dplyr::bind_rows(
          lapply(1:length(json.data$failed), \(j) {
            x <- json.data$failed[[j]]
            key <- as.numeric(names(json.data$failed[j]))+1
            data.frame(
              key = metadata[[i]]$key[key],
              error = sprintf("Error %s: %s", x$code, x$message)
            )
          })
        )

        # Set error message
        summary[[i]] <- summary[[i]] |>
          tibble::add_column(error = NA_character_, .before = 1) |>
          dplyr::rows_update(failed.message, by = "key")


      }

      # Estimate time of arrival
      log.eta <-
        LogCat(
          Eta(query.start,
              i,
              length(metadata)),
          silent = silent,
          flush = TRUE,
          log = log,
          append.log = FALSE
        )
    }

    # Append eta log to log
    zotero$log <- append(zotero$log,log.eta)

    # Combine summary
    summary <- dplyr::bind_rows(summary)
    # Combine metadata
    metadata <- dplyr::bind_rows(metadata)
    # Add prefix
    metadata <- metadata |>
      tibble::add_column(prefix = zotero$prefix)

    # Add data
    if (post.collections) {
      summary.names <- "collections"
    } else {
      summary.names <- "items"
    }

    # Summary list
    summary.list  <- stats::setNames(
      list(summary, table(summary$status)),
      paste0(c("post.status.", "post.summary."), summary.names)
    )

    # Add summary to log
    zotero$log <- LogCat(
      summary.list,
      silent = silent,
      trim = FALSE,
      log = zotero$log,
    )

    # Add data
    if (post.collections) {
      zotero$collection.key <- utils::tail(metadata$key,1)
      zotero[["collections"]] <- metadata
    } else {
      zotero[["items"]] <- metadata
    }



    return (zotero)
  }

  # POST attachments function
  UploadAttachments <- \(zotero, silent) {

    # Visible bindings
    key <- status <- NULL

    # MORE...
    n.attachments <- Pluralis(nrow(zotero$attachments),
                              "attachment", "attachments")

    # Add message to log
    zotero$log <- LogCat(
      paste("Uploading", n.attachments),
      silent = silent,
      log = zotero$log
    )

    # Start time for query
    query.start <- Sys.time()
    # Cycle through attachments
    for (i in 1:nrow(zotero$attachments)) {

      # Select attachment
      x <- zotero$attachments[i,]

      # Define query
      query.list <- list(
        md5 = x$md5,
        filename=x$filename,
        filesize=x$size,
        mtime=x$mtime
      )

      # Create header
      json.header <- httr::add_headers(
        "Content-Type" = "application/x-www-form-urlencoded",
        "If-Match" = x$md5
      )

      # Post JSON to url defined in zotero list
      json.post <- httr::RETRY(
        "POST",
        ZoteroUrl(zotero$url,
                  item.key = x$key,
                  api = zotero$api,
                  use.item = TRUE,
                  append.file = TRUE),
        json.header,
        body = x$file[[1]]$content,
        query = query.list,
        quiet = TRUE
      )

      # Add status to attachments
      zotero$attachments[i, "status"] <- json.post$status_code

      # Update item version
      version <- as.double(json.post$headers$`last-modified-version`)
      zotero$items$version[zotero$items$key == x$key] <- version

      # Estimate time of arrival
      log.eta <-
        LogCat(
          Eta(query.start,
              i,
              nrow(zotero$attachments)),
          silent = silent,
          flush = TRUE,
          log = log,
          append.log = FALSE
        )

    }

    # Create summary
    summary <- zotero$attachments |>
      dplyr::select(key, status)

    # Add summary to log
    zotero$log <- LogCat(
      list(post.attachments = summary),
      silent = silent,
      trim = FALSE,
      log = zotero$log,
    )

    # Append eta log to log
    zotero$log <- append(zotero$log,log.eta)

    return (zotero)

  }

  # POST collections if post.collections is set to TRUE
  if (post.collections & !is.null(zotero$collections)) {
    zotero <- Upload(zotero,
                     post.collections = TRUE,
                     post.items = FALSE,
                     post.limit,
                     force,
                     silent)
  }
  # POST items if post.items is set to TRUE
  if (post.items & !is.null(zotero$items)) {
    zotero <- Upload(zotero,
                     post.collections = FALSE,
                     post.items = TRUE,
                     post.limit,
                     force,
                     silent)
  }

  # POST attachments if post.attachments is set to TRUE
  if (post.items & post.attachments & !is.null(zotero$attachments)) {
    zotero <- UploadAttachments(zotero, silent)
  }

  return (zotero)

}
