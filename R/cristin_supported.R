#' @title Check for supported export formats from Cristin to Zotero
#' @description Used to filter Cristin categories that are not supported
#' @param data Zotero-type matrix, Default: NULL
#' @return Zotero supported items with unsupported categories as NA
#' @details Please see \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Supported Cristin items
#'     print(CristinSupported(), n = 32)
#'   }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#' @rdname CristinSupported
#' @export
CristinSupported <- \(data = NULL) {

  # Currently supported references
  supported.types <- c(
    anthologyaca= "book",
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

    # Else return list with supported data
  } else {
    # Find supported categories, set unsupported as NA
    supported <- as.character(supported.types[
      match(tolower(data$category$code), names(supported.types))
    ])

    # Unsupported categories
    unsupported <- data$category$code[is.na(supported)]

    # Replace Cristin categories with supported Zotero types
    data$category <- supported

    data <- list(
      data = data,
      supported = supported,
      unsupported = unsupported
    )
  }

  return (data)

}
