% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zotero_isbn.R
\name{CheckIsbn}
\alias{CheckIsbn}
\title{Check and Convert ISBN}
\usage{
CheckIsbn(key, split.key = ",")
}
\arguments{
\item{key}{A character string that contains an ISBN or a compound string where the first element is the ISBN.}

\item{split.key}{A delimiter used to split the \code{key}. Default is \code{","}.}
}
\value{
A cleaned and validated ISBN string. If the resulting ISBN does not meet the expected criteria,
  the function returns \code{NULL}.
}
\description{
This function takes an input string that may contain an ISBN (or a compound string
where the first component is the ISBN), cleans it by removing non-alphanumeric characters
(except spaces) and spaces, and processes it with the \code{GoFish} function. If the cleaned
ISBN is in the ISBN10 format, the function converts it to ISBN13. Finally, it validates the ISBN;
valid ISBNs either start with "97" and are 13 characters long or start with "99" and are longer than 7 characters.
}
\details{
The function performs the following steps:
\enumerate{
  \item Splits the input \code{key} using \code{split.key} and selects the first element.
  \item Removes foreign (non-alphanumeric) characters and spaces using nested \code{gsub} calls.
  \item Applies the \code{GoFish} function to the cleaned key (ensure that \code{GoFish} is defined in your environment).
  \item If the key is in ISBN10 format (10 characters), converts it to ISBN13 by prepending \code{978},
        recalculating the check digit, and concatenating the result.
  \item Checks that the final key is valid by confirming that it either starts with "97" (and is 13 characters long)
        or starts with "99" and has more than 7 characters.
}
}
\examples{
\dontrun{
# Example with a compound string where the first part is the ISBN:
isbn_input <- "316148410X, some extra text"
valid_isbn <- CheckIsbn(isbn_input)

# Example converting an ISBN10 to ISBN13:
isbn10 <- "316148410X"
valid_isbn <- CheckIsbn(isbn10)
}

}
