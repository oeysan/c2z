#' Process Data with Optional Parallel Execution and Progress Reporting
#'
#' This function executes a provided expression with progress reporting and optional
#' parallel processing using the \code{future} and \code{progressr} packages. It logs the
#' start and end times of the process along with a runtime message and supports custom logging,
#' progress handlers, and execution plans.
#'
#' @param func An expression (or a quoted function call) to be evaluated. The default is
#'   \code{substitute(NULL)}. It is evaluated in the parent frame.
#' @param n Optional integer indicating the number of items to be processed, used solely for logging purposes.
#' @param start.message Optional character string specifying a custom start message. If \code{NULL},
#'   a default message is generated based on the processing type and, if provided, the number of items.
#' @param end.message Optional character string specifying a custom end message. If \code{NULL},
#'   a runtime message is generated showing the elapsed time.
#' @param use.multisession Logical. If \code{TRUE} (default), parallel processing using multisession
#'   is employed; otherwise, processing is sequential.
#' @param n.workers Optional integer for the number of workers to be used in
#' multisession mode. If \code{NULL}, it defaults to the number of available
#' cores minus one (with a minimum of one).
#' @param n.chunks Optional integer for the number of chunks to process.
#' If \code{NULL}, it defaults to the number of workers.
#' @param handler The progress handler to be used by the \code{progressr} package. If \code{NULL} and
#'   \code{silent} is \code{FALSE}, it defaults to \code{"txtprogressbar"}. When \code{silent} is \code{TRUE},
#'   the handler is set to \code{"void"}.
#' @param restore.defaults Logical. If \code{TRUE} (default), the current \code{future} plan is saved and restored upon exit.
#' @param silent Logical. If \code{TRUE}, progress output is suppressed. Default is \code{FALSE}.
#' @param log A list to collect log messages. Default is an empty list.
#'
#' @details
#' The function begins by determining whether to use multisession (parallel) or sequential processing.
#' It then logs the start time using a helper function, \code{LogCat}, and sets up the \code{future} plan
#' accordingly. If \code{restore.defaults} is \code{TRUE}, the original plan is restored after execution.
#'
#' A progress handler is configured via the \code{progressr} package. The provided expression is evaluated
#' with progress reporting enabled. Upon completion, the function computes the elapsed time (with millisecond
#' precision) and logs an end message. Finally, it returns a list containing the result of the evaluated expression
#' and the log of messages.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{result}{The output resulting from the evaluation of \code{func}.}
#'   \item{log}{A list of log messages including start and end timestamps along with associated messages.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Example: Process data with a simple sleep expression using multisession processing
#'   result <- Processing(
#'     func = quote({
#'       Sys.sleep(2)  # simulate a time-consuming task
#'       "Task Completed"
#'     }),
#'     n = 100,
#'     use.multisession = TRUE
#'   )
#'   print(result$result)
#'   print(result$log)
#' }
#'
#' @import future
#' @import progressr
#' @export
Processing <- function(func = substitute(NULL),
                        n = NULL,
                        start.message = NULL,
                        end.message = NULL,
                        use.multisession = TRUE,
                        n.workers = NULL,
                        n.chunks = NULL,
                        handler = NULL,
                        restore.defaults = TRUE,
                        silent = FALSE,
                        log = list()) {

  # Process start time
  process.start <- Sys.time()

  # Define workers and future session
  process.type <- if (use.multisession) "multisession" else "sequential"
  if (is.null(n.workers)) n.workers <- max(1, future::availableCores() - 1)
  if (is.null(n.chunks)) n.chunks <- n.workers
  if (use.multisession && !inherits(future::plan(), process.type)) {
    future::plan(process.type, workers = n.workers)
  } else {
    future::plan(process.type)
  }

  if (is.null(start.message)) {
    start.message <- sprintf("Conducting %s processing", process.type)
    if (!is.null(n)) {
      start.message <- paste(
        start.message, sprintf("of %s", Numerus(n, "item"))
      )
    }
  } else {
    start.message <- paste(
      sprintf("%s processing.", stringr::str_to_title(process.type)),
      start.message
    )
  }

  toupper("test")

  log <- LogCat(
    sprintf(
      "%s (began %s)",
      start.message,
      format(process.start, "%d.%m.%Y - %H:%M:%S")
    ),
    silent = silent,
    log = log
  )

  # Setup progress handler
  if (silent) {
    handler <- "void"
  } else if (is.null(handler)) {
    handler <- "txtprogressbar"
  }
  handlers <- progressr::handlers(handler)

  # Execute the process with progress reporting
  results <- progressr::with_progress(eval(func, envir = parent.frame()))

  # Compute end time and runtime message
  process.end <- Sys.time()
  time.diff <- process.end - process.start
  total.seconds <- as.numeric(time.diff, units = "secs")
  hours <- floor(total.seconds / 3600)
  minutes <- floor((total.seconds %% 3600) / 60)
  seconds.whole <- floor(total.seconds %% 60)
  ms <- round((total.seconds - floor(total.seconds)) * 1000)
  runtime <- sprintf(
    "Duration: %02d:%02d:%02d.%03d",
    hours, minutes, seconds.whole, ms
  )

  if (is.null(end.message)) {
    end.message <- runtime
  } else {
    end.message <- sprintf("%s. %s", runtime, end.message)
  }

  log <- LogCat(
    sprintf(
      "%s (ended %s)",
      end.message,
      format(process.end, "%d.%m.%Y - %H:%M:%S")
    ),
    silent = silent,
    log = log
  )

  # Store the current future plan and handlers if restore.defaults is TRUE
  if (restore.defaults) {
    original.plan <- future::plan()
    on.exit(future::plan(original.plan), add = TRUE)
    on.exit(handlers(handlers), add = TRUE)
  }

  return(list(results = results, log = log))
}

#' Process data with Parallel Execution and Row/Chunk Processing
#'
#' This function processes a tibble (or data frame) in parallel using the \code{future} package
#' along with \code{progressr} for progress reporting. The data is split into chunks via a helper function
#' \code{SplitData}. Depending on the \code{by.rows} flag, each chunk is processed either row-by-row
#' (using \code{lapply}) or as an entire chunk. In either case, the processed results are bound into a single tibble.
#'
#' @param data Data to be processed.
#' @param func A function that processes either a single row (if \code{by.rows = TRUE}) or an entire chunk
#'   (if \code{by.rows = FALSE}). Additional arguments are passed to \code{func} via \code{...}.
#' @param by.rows Logical. If \code{TRUE}, \code{func} is applied to each row individually within each chunk;
#'   if \code{FALSE}, \code{func} is applied to the entire chunk. Default is \code{TRUE}.
#' @param min.multisession Integer. The minimum number of rows in \code{data} required to use multisession
#'   (parallel execution). If \code{nrow(data)} is less than this value, parallel processing is disabled.
#'   Default is \code{1}.
#' @param n.workers Integer. The number of workers to use for parallel processing.
#'   Defaults to \code{max(1, future::availableCores() - 1)}.
#' @param n.chunks Integer. The number of chunks into which the data is split.
#'   If \code{NULL}, it defaults to the number of workers.
#' @param limit Integer. If \code{(nrow(data) / limit)} is less than or equal to \code{n.workers},
#'   the data is split into \code{n.chunks}; otherwise, it is split using \code{limit}. Default is \code{100}.
#' @param use.multisession Logical. If \code{TRUE} (default) parallel processing is used; otherwise,
#'   sequential processing is employed.
#' @param start.message Optional character string. A custom message to log at the start of processing.
#' @param end.message Optional character string. A custom message to log at the end of processing.
#' @param restore.defaults Logical. If \code{TRUE} (default), the original future plan and progress handlers
#'   are restored after processing.
#' @param handler Character string. The progress handler to be used by the \code{progressr} package.
#'   Defaults to \code{"txtprogressbar"} unless \code{silent} is \code{TRUE}.
#' @param silent Logical. If \code{TRUE}, progress messages and logging are suppressed. Default is \code{FALSE}.
#' @param log List. An initial log (default is an empty list) to which log messages are appended.
#' @param ... Additional arguments passed to the processing function \code{func}.
#'
#' @return A list with two components:
#' \describe{
#'   \item{results}{A tibble resulting from binding all processed rows from each chunk.}
#'   \item{log}{A list of log messages generated during processing.}
#' }
#'
#' @details
#' The function first checks if the data has enough rows to warrant parallel processing.
#' It then determines the number of workers and chunks and splits the data using the helper function
#' \code{SplitData}. The processing is executed with \code{future.apply::future_lapply} in parallel. If \code{by.rows} is \code{TRUE},
#' each row within a chunk is processed individually and then bound together using \code{dplyr::bind_rows};
#' if \code{by.rows} is \code{FALSE}, the entire chunk is processed at once. Finally, all chunk results are combined
#' into a single tibble.
#'
#' @examples
#' \dontrun{
#' example.result <- ProcessData(
#'   data =  tibble::tibble(
#'     key = LETTERS[1:3],
#'     value = seq(3)
#'   ),
#'   func = \(data) {
#'     data$value <- data$value * 10
#'     data
#'   },
#'   by.rows = FALSE,
#'   min.multisession = 10,
#'   n.workers = 4,
#'   n.chunks = 4,
#'   limit = 100,
#'   use.multisession = TRUE,
#'   start.message = "Starting example data processing",
#'   end.message   = "Finished example data processing",
#'   handler = "txtprogressbar",
#'   silent = FALSE
#' )
#' # The result is a single tibble.
#' processed.tibble <- example.result$results
#' process.log <- example.result$log
#' if (any(nrow(processed.tibble))) print(processed.tibble)
#' }
#'
#' @export
ProcessData <- function(data,
                          func,
                          by.rows = TRUE,
                          min.multisession = 1,
                          n.workers = NULL,
                          n.chunks = NULL,
                          limit = 100,
                          use.multisession = TRUE,
                          start.message = NULL,
                          end.message = NULL,
                          restore.defaults = TRUE,
                          handler = NULL,
                          silent = FALSE,
                          log = list(),
                          ...) {


  # Return data if empty
  if (!any(nrow(data))) {
    return (list(results = NULL, log = c(log, "No results to wrangle.")))
  }

  # Disable multisession if the number of rows in data is below the threshold
  if (nrow(data) < min.multisession) {
    use.multisession <- FALSE
  }

  # Set number of workers and chunks if not provided
  if (is.null(n.workers)) {
    n.workers <- max(1, future::availableCores() - 1)
  }
  if (is.null(n.chunks)) {
    n.chunks <- n.workers
  }

  # Split the data into chunks using the SplitData helper
  if ((nrow(data) / limit) <= n.workers) {
    data.chunks <- SplitData(data, chunks = n.chunks)
  } else {
    data.chunks <- SplitData(data, limit)
  }

  # Use Processing to handle future plan and progress reporting
  process.result <- Processing(
    func = quote({
      p <- progressr::progressor(steps = length(data.chunks))

      chunk.list <- future.apply::future_lapply(seq_along(data.chunks), function(i) {
        chunk <- data.chunks[[i]]
        process.message <- sprintf(
          "Processsed %s of %s batches", i, length(data.chunks)
        )
        if (by.rows) {
          row.list <- lapply(seq_len(nrow(chunk)), function(i) {
            func(chunk[i, , drop = FALSE], ...)
          })
          p(message = paste(process.message, "by rows"))
          # Bind rows from the individual row processing into a single tibble for this chunk
          dplyr::bind_rows(row.list)
        } else {
          chunk.result <- func(chunk, ...)
          p(message = process.message)
          chunk.result
        }
      }, future.seed = TRUE)

      # Combine results from all chunks into one tibble
      dplyr::bind_rows(chunk.list)
    }),
    n = nrow(data),
    start.message = start.message,
    end.message = end.message,
    use.multisession = use.multisession,
    restore.defaults = restore.defaults,
    handler = handler,
    silent = silent
  )

  log <- c(log, process.result$log)
  results <- process.result$results

  return(list(results = results, log = log))
}


