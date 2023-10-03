#' @title Create Zotero key or write token
#' @description Create an identifier for Zotero items/collections or a write
#'   token to library
#' @param token Create a write token, Default: FALSE
#' @return Will return a 8 character Zotero key or 32 character write token
#' @details Please see
#'   \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' # Create a Zotero key
#' key <- ZoteroKey()
#' # Create a write token
#' token <- ZoteroKey(TRUE)
#' @rdname ZoteroKey
#' @export
ZoteroKey <- \(token = FALSE) {

  # Create JSON token if token is set to TRUE
  if (token) {
    # Key can contain all numeric values and (capital) letters
    key <- c(0:9, letters, LETTERS)
    # With a length of 32 characters
    length <- 32
    # Else create a Zotero key
  } else {
    # Allowed values
    key <- strsplit("23456789ABCDEFGHIJKLMNPQRSTUVWXYZ", "")[[1]]
    # With a length of 8 characters
    length <- 8

  }

  # Collapse to single string
  key <- paste(
    sample(key, length, replace = TRUE),
    collapse = ""
  )

  return(key)

}
