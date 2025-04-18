% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zotero.R
\name{Zotero}
\alias{Zotero}
\title{Connect to Zotero API}
\usage{
Zotero(
  collection.names = NULL,
  collection.key = NULL,
  collection.path = NULL,
  item.key = NULL,
  library = FALSE,
  case.insensitive = TRUE,
  ancestor = FALSE,
  recursive = FALSE,
  create = FALSE,
  limit = 100,
  start = 0,
  search.collections = TRUE,
  get.collections = TRUE,
  get.items = TRUE,
  item.type = NULL,
  all.results = TRUE,
  max.results = NULL,
  collections = NULL,
  items = NULL,
  attachments = NULL,
  metadata = NULL,
  doi = NULL,
  isbn = NULL,
  export = FALSE,
  csl.type = NULL,
  csl.name = "style",
  format = "biblatex",
  save.data = FALSE,
  save.path = NULL,
  bib.name = "references",
  library.type = NULL,
  linkwrap = 1,
  style = "apa",
  locale = "en-US",
  copy = FALSE,
  copy.collections = TRUE,
  copy.items = TRUE,
  copy.extras = TRUE,
  remove.missing = TRUE,
  change.library = FALSE,
  copy.user = TRUE,
  copy.id = NULL,
  copy.api = NULL,
  post = FALSE,
  post.collections = TRUE,
  post.items = TRUE,
  post.attachments = TRUE,
  post.limit = 50,
  post.token = FALSE,
  delete = FALSE,
  delete.collections = FALSE,
  delete.items = FALSE,
  delete.limit = 50,
  ragnarok = FALSE,
  user = TRUE,
  index = FALSE,
  id = NULL,
  token = NULL,
  token.api = NULL,
  api = NULL,
  force = FALSE,
  base.url = "https://api.zotero.org",
  silent = FALSE,
  zotero = NULL,
  log = list()
)
}
\arguments{
\item{collection.names}{Vector of collection names to create or search for,
Default: NULL}

\item{collection.key}{A specified collection key, Default: NULL}

\item{collection.path}{Vector of nested collection keys, Default: NULL}

\item{item.key}{A specified item key, Default: NULL}

\item{library}{Use `ZoteroLibrary` to fetch collections and items, Default:
FALSE}

\item{case.insensitive}{Disregard letter casing when searching for
collections, Default: TRUE}

\item{ancestor}{Trace the lineage of a collection (i.e., find the top-level
collection), Default: FALSE}

\item{recursive}{Find all nested collections, Default: FALSE}

\item{create}{Create missing collections, Default: FALSE}

\item{limit}{Number of results per query (max 100), Default: 100}

\item{start}{Starting position of query (0 = first result), Default: 0}

\item{search.collections}{Search all collections if collection.key fails,
Default: TRUE}

\item{get.collections}{Fetch collections, Default: TRUE}

\item{get.items}{Fetch items, Default: TRUE}

\item{item.type}{Items to search for (NULL = everything), Default: NULL}

\item{all.results}{Find all results in query, Default: TRUE}

\item{max.results}{Do you need a limit?, Default: NULL}

\item{collections}{Predefined collections (as tibble), Default: NULL}

\item{items}{Predefined metadata (as tibble), Default: NULL}

\item{attachments}{Predefined attachments (as tibble), Default: NULL}

\item{metadata}{Predefined metadata in Zotero-format, Default: NULL}

\item{doi}{Use \code{\link{ZoteroDoi}} to fetch DOI metadata, Default: NULL}

\item{isbn}{Use \code{\link{ZoteroIsbn}} to fetch ISBN metadata, Default:
NULL}

\item{export}{Use `ZoteroExport` to export items, Default: FALSE}

\item{csl.type}{Specify a CSL type to Official repository for Citation Style
Language (CSL), Default: NULL}

\item{csl.name}{Name of saved CSL file, Default: 'style'}

\item{format}{Export format of Zotero items, Default: 'biblatex'}

\item{save.data}{Save data (e.g., bibliography) to disk, Default: FALSE}

\item{save.path}{Location to store data on disk, Default: NULL}

\item{bib.name}{Name of exported bibliography, Default: 'references'}

\item{library.type}{Commma-separated data from Zotero (i.e., data, bib,
citation), Default: NULL}

\item{linkwrap}{Set URL (e.g., DOI) as HTML link (1 = yes), Default: 1}

\item{style}{Citation style to use for appended bibliography and/or
citations, Default: apa}

\item{locale}{Desired language format of bibliography, Default: 'en-US'}

\item{copy}{Use `ZoteroCopy` to delete collections and/or items, Default:
FALSE}

\item{copy.collections}{Try to copy specified collections, Default: TRUE}

\item{copy.items}{Try to copy specified items?, Default: TRUE}

\item{copy.extras}{Try to copy specified extras (i.e., attachments and
notes)?, Default: TRUE}

\item{remove.missing}{Deleted missing extras, Default: TRUE}

\item{change.library}{Stage changing of library (e.g., from a group to a
personal library), Default: FALSE}

\item{copy.user}{New user type (The functions will use `group` as prefix if
FALSE), Default: TRUE}

\item{copy.id}{New id, Default: NULL}

\item{copy.api}{New API key. Set API to `NA` if key is not needed, Default:
NULL}

\item{post}{Use `ZoteroPost` to post collections and/or items, Default: FALSE}

\item{post.collections}{Try to copy specified collections, Default: TRUE}

\item{post.items}{Try to copy specified items?, Default: TRUE}

\item{post.attachments}{Try to copy specified extras (i.e., attachments and
notes)?, Default: TRUE}

\item{post.limit}{Number of collections/items to post per request (max 50),
Default: 50}

\item{post.token}{Use Zotero-Write-Token (TRUE) or
If-Unmodified-Since-Version (FALSE), Default: FALSE}

\item{delete}{Use `ZoteroDelete` to delete collections and/or items, Default:
FALSE}

\item{delete.collections}{Try to delete specified collections, Default: TRUE}

\item{delete.items}{Try to delete specified items?, Default: TRUE}

\item{delete.limit}{Number of collections/items to delete per request (max
50), Default: 50}

\item{ragnarok}{Delete EVERYTHING in the specified library, Default: FALSE}

\item{user}{User type (The functions will use `group` as prefix if FALSE),
Default: TRUE}

\item{index}{Create an index of items, Default: FALSE}

\item{id}{User or group ID, Default: NULL}

\item{token}{Name of user or group token as defined in `.Renviron`, Default:
NULL}

\item{token.api}{Name of API token as defined in `.Renviron`, Default:
NULL}

\item{api}{API key to connect with the Zotero library. Set API to `NA` if key
is not needed. See
\href{https://oeysan.github.io/c2z/articles/zotero_api.html}{Zotero API},
Default: NULL}

\item{force}{Force is seldom wise, but sometimes..., Default: FALSE}

\item{base.url}{Base url of the Zotero API, Default: 'https://api.zotero.org'}

\item{silent}{c2z is noisy, tell it to be quiet, Default: FALSE}

\item{zotero}{A list with information on the specified Zotero library (e.g.,
id, API key, collections, and items), Default: NULL}

\item{log}{A list for storing log elements, Default: list()}
}
\value{
A list with information on the specified Zotero library (e.g., id,
  API key, collections, and items)
}
\description{
Wrapper to connect with the Zotero API and the main functions of
  *c2z*
}
\details{
Please see
\href{https://oeysan.github.io/c2z/}{https://oeysan.github.io/c2z/}
}
\examples{
# Create the default Zotero list
example <- Zotero(id = "9913421", api = "Io1bwAiOjB1jPgUNegjJhJxF")
# Print the interesting pars of an otherwise empty list
print(tail(example,5))
}
\seealso{
\code{\link[httr]{http_error}}, \code{\link[httr]{GET}}
}
