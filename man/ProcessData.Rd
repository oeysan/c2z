% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_data.R
\name{ProcessData}
\alias{ProcessData}
\title{Process data with Parallel Execution and Row/Chunk Processing}
\usage{
ProcessData(
  data,
  func,
  by.rows = TRUE,
  min.multisession = 1,
  n.workers = NULL,
  limit = 100,
  use.multisession = TRUE,
  n.message = 1,
  start.message = NULL,
  process.message = NULL,
  end.message = NULL,
  restore.defaults = TRUE,
  handler = NULL,
  silent = FALSE,
  log = list(),
  ...
)
}
\arguments{
\item{data}{Data to be processed.}

\item{func}{A function that processes either a single row (if \code{by.rows = TRUE}) or an entire chunk
(if \code{by.rows = FALSE}). Additional arguments are passed to \code{func} via \code{...}.}

\item{by.rows}{Logical. If \code{TRUE}, \code{func} is applied to each row individually within each chunk;
if \code{FALSE}, \code{func} is applied to the entire chunk. Default is \code{TRUE}.}

\item{min.multisession}{Integer. The minimum number of rows in \code{data} required to use multisession
(parallel execution). If \code{nrow(data)} is less than this value, parallel processing is disabled.
Default is \code{1}.}

\item{n.workers}{Integer. The number of workers to use for parallel processing.
Defaults to \code{max(1, future::availableCores() - 1)}.}

\item{limit}{Integer. If \code{(nrow(data) / limit)} is less than or equal to \code{n.workers},
the data is split into \code{n.workers}; otherwise, it is split using \code{limit}. Default is \code{100}.}

\item{use.multisession}{Logical. If \code{TRUE} (default) parallel processing is used; otherwise,
sequential processing is employed.}

\item{n.message}{Integer. Display a process message every nth iteration. Defaults to \code{1}. Turn off messages with 0.}

\item{start.message}{Optional character string. A custom message to log at the start of processing.}

\item{process.message}{Optional column name. A custom message to log during processing.}

\item{end.message}{Optional character string. A custom message to log at the end of processing.}

\item{restore.defaults}{Logical. If \code{TRUE} (default), the original future plan and progress handlers
are restored after processing.}

\item{handler}{Character string. The progress handler to be used by the \code{progressr} package.
Defaults to \code{"txtprogressbar"} unless \code{silent} is \code{TRUE}.}

\item{silent}{Logical. If \code{TRUE}, progress messages and logging are suppressed. Default is \code{FALSE}.}

\item{log}{List. An initial log (default is an empty list) to which log messages are appended.}

\item{...}{Additional arguments passed to the processing function \code{func}.}
}
\value{
A list with two components:
\describe{
  \item{results}{A tibble resulting from binding all processed rows from each chunk.}
  \item{log}{A list of log messages generated during processing.}
}
}
\description{
This function processes a tibble (or data frame) in parallel using the \code{future} package
along with \code{progressr} for progress reporting. The data is split into chunks via a helper function
\code{SplitData}. Depending on the \code{by.rows} flag, each chunk is processed either row-by-row
(using \code{lapply}) or as an entire chunk. In either case, the processed results are bound into a single tibble.
}
\details{
The function first checks if the data has enough rows to warrant parallel processing.
It then determines the number of workers and chunks and splits the data using the helper function
\code{SplitData}. The processing is executed with \code{future.apply::future_lapply} in parallel. If \code{by.rows} is \code{TRUE},
each row within a chunk is processed individually and then bound together using \code{dplyr::bind_rows};
if \code{by.rows} is \code{FALSE}, the entire chunk is processed at once. Finally, all chunk results are combined
into a single tibble.
}
\examples{
\dontrun{
example.result <- ProcessData(
  data =  tibble::tibble(
    key = LETTERS[1:3],
    value = seq(3)
  ),
  func = \(data) {
    data$value <- data$value * 10
    data
  },
  by.rows = FALSE,
  min.multisession = 10,
  n.workers = 4,
  limit = 100,
  use.multisession = TRUE,
  start.message = "Starting example data processing",
  end.message   = "Finished example data processing",
  handler = "txtprogressbar",
  silent = FALSE
)
# The result is a single tibble.
processed.tibble <- example.result$results
process.log <- example.result$log
if (any(nrow(processed.tibble))) print(processed.tibble)
}

}
