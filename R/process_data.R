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
#' @param n.workers Optional integer for the number of workers to be used in multisession mode. If \code{NULL},
#'   it defaults to the number of available cores minus one (with a minimum of one).
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
#'   result <- ProcessData(
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
ProcessData <- function(func = substitute(NULL),
                        n = NULL,
                        start.message = NULL,
                        end.message = NULL,
                        use.multisession = TRUE,
                        n.workers = NULL,
                        handler = NULL,
                        restore.defaults = TRUE,
                        silent = FALSE,
                        log = list()) {

  # Determine processing type and log start time
  process.type <- if (use.multisession) "multisession" else "sequential"
  process.start <- Sys.time()

  if (is.null(start.message)) {
    start.message <- sprintf("Conducting %s processing", process.type)
    if (!is.null(n)) {
      start.message <- paste(
        start.message, sprintf("of %s", Numerus(n, "item"))
      )
    }
  }

  log <- LogCat(
    sprintf(
      "%s (began %s)",
      start.message,
      format(process.start, "%d.%m.%Y - %H:%M:%S")
    ),
    silent = silent,
    log = log
  )

  # Store the current future plan if restore.defaults is TRUE
  if (restore.defaults) {
    original_plan <- future::plan()
    on.exit(future::plan(original_plan), add = TRUE)
  }

  # Setup future plan
  if (use.multisession) {
    if (is.null(n.workers)) n.workers <- max(1, future::availableCores() - 1)
    if (!inherits(future::plan(), process.type)) {
      future::plan(process.type, workers = n.workers)
    }
  } else {
    future::plan(process.type)
  }

  # Setup progress handler
  if (silent) {
    handler <- "void"
  } else if (is.null(handler)) {
    handler <- "txtprogressbar"
  }
  handlers <- progressr::handlers(handler)

  # Execute the process with progress reporting
  results <- progressr::with_progress(eval(func, envir = parent.frame()))

  # Compute end time and runtime message (including ms)
  process.end <- Sys.time()
  if (is.null(end.message)) {
    time.diff <- process.end - process.start
    total_seconds <- as.numeric(time.diff, units = "secs")
    hours <- floor(total_seconds / 3600)
    minutes <- floor((total_seconds %% 3600) / 60)
    seconds_whole <- floor(total_seconds %% 60)
    ms <- round((total_seconds - floor(total_seconds)) * 1000)
    end.message <- sprintf("Runtime: %02d:%02d:%02d.%03d",
                           hours, minutes, seconds_whole, ms)
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

  # Restore progress handlers on exit
  on.exit(handlers(handlers), add = TRUE)

  return(list(results = results, log = log))
}
