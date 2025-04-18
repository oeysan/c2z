% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zotero_enhancer.R
\name{UpdateInsert}
\alias{UpdateInsert}
\title{Upsert and Update Data Frames}
\usage{
UpdateInsert(x, y, key = "key", check.missing = FALSE)
}
\arguments{
\item{x}{A data frame to be updated.}

\item{y}{A data frame containing new values to update \code{x}. Must include the column specified by \code{key}.}

\item{key}{A character string specifying the unique key column used for matching rows. Defaults to \code{"key"}.}

\item{check.missing}{Logical; if \code{TRUE}, performs a cell-by-cell update only when the new value is not missing.
Missing values are defined as \code{NA} for atomic types or an empty list for list columns. If \code{FALSE},
a standard upsert is performed using \code{dplyr::rows_upsert}. Defaults to \code{FALSE}.}
}
\value{
A data frame resulting from updating \code{x} with values from \code{y}.
}
\description{
Combines two data frames by updating rows in \code{x} with values from \code{y} based on a common key,
and inserting new rows from \code{y} that are not present in \code{x}. The function first harmonizes the
column structures of both data frames by adding missing columns and coercing types as necessary.
}
\details{
The function works in several steps:

\enumerate{
  \item It computes the union of all column names from \code{x} and \code{y} and adds any missing columns
        to both data frames using the internal helper function \code{AddColumns}. Missing columns are filled
        with an appropriate \code{NA} value based on their type.
  \item Both \code{x} and \code{y} are reordered to have the same column order.
  \item For each common column (excluding the key), if \code{x}'s column is entirely \code{NA} or if the
        data types differ, coercion is performed to ensure compatibility between \code{x} and \code{y}.
  \item When \code{check.missing} is \code{TRUE}, the function iterates over each common key and updates each
        cell in \code{x} only if the corresponding cell in \code{y} is not missing. Otherwise, it uses
        \code{dplyr::rows_upsert} to perform a standard upsert.
  \item New rows present in \code{y} but not in \code{x} are appended.
}
}
\examples{
\dontrun{
  # Example data frames:
  df1 <- data.frame(
    key = 1:3,
    a = c(NA, 2, NA),
    b = c("x", NA, "z"),
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    key = c(2, 3, 4),
    a = c(5, 6, 7),
    b = c("y", "w", "v"),
    stringsAsFactors = FALSE
  )

  # Standard upsert (check.missing = FALSE):
  result <- UpdateInsert(df1, df2, key = "key", check.missing = FALSE)

  # Cell-by-cell update (check.missing = TRUE):
  result <- UpdateInsert(df1, df2, key = "key", check.missing = TRUE)
}

}
