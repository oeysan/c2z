% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zotero_match.R
\name{ZoteroMatch}
\alias{ZoteroMatch}
\title{Search or match items using CrossRef}
\usage{
ZoteroMatch(
  title,
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
  use.semantic = FALSE,
  log = list()
)
}
\arguments{
\item{title}{Title of reference}

\item{authors}{creators of the reference}

\item{date}{publication date of the reference}

\item{haystack}{Potential matches for search term (i.e., needle), Default:
NULL}

\item{haystack.size}{Number of items in the haystack, Default: 3}

\item{crossref.search}{Search CrossRef if needle not found in haystack,
Default: FALSE}

\item{autosearch}{Match automatically or compare needle with haystack,
Default: FALSE}

\item{cristin.data}{Metadata from Cristin, Default: NULL}

\item{external.data}{Metadata from external source (e.g., CrossRef), Default:
NULL}

\item{polite}{Will use an email stored in `.Renviron`, Default: TRUE}

\item{silent}{Running silent, running deep, Default: FALSE}

\item{use.semantic}{Prefer metadata from Semantic Scholar, Default: FALSE}

\item{log}{A list for storing log elements, Default: list()}
}
\value{
A Zotero-type matrix (tibble) if match is found otherwise NULL
}
\description{
Use the CrossRef API to match data (e.g., Cristin metadata with
  Crossref metadata) or search by title, authors and date
}
\details{
Please see
  \href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
}
\examples{
\donttest{
  # Conduct an autosearch in CrossRef using title, authors and date
  example <- ZoteroMatch(
    title = "Nonreplicable publications",
    authors = "Serra-Garcia & Gneezy",
    date = "2021",
    autosearch = TRUE
  )

  # Print index using `ZoteroIndex`
  if (any(nrow(example$data))) {
  ZoteroIndex(example$data) |>
    dplyr::select(name) |>
    print(width = 80)
  }
}
}
\seealso{
\code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}},
 \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{coalesce}},
 \code{\link[dplyr]{pull}}
 \code{\link[rlang]{sym}}
 \code{\link[httr]{RETRY}}
 \code{\link[jsonlite]{toJSON, fromJSON}}
 \code{\link[utils]{adist}}, \code{\link[utils]{head}}
 \code{\link[purrr]{map}}
}
