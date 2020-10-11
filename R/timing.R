#' Wait and window
#'
#' Compare a time from the past (`then`) to the current time (`now`, default:
#' `Sys.time()`) and see if `wait` seconds (default: 1 week) has passed.
#' additionally, only return `TRUE` if the current time is between the hours
#' given in `window` (if `window` is null, then always return)
#'
#' @param then a previous time (`POSIXct`)
#' @param now the current time (default: `Sys.time()`)
#' @param wait the seconds to wait between `then` and `now` (default: `PHOTOTWEETR_WAIT`
#' environment variable or one week if unset)
#' @param window hours between which to return `TRUE`
#' @param verbose should the wait/window `message` why? (default: `TRUE`)
#'
#' @return logical
#' @export
wait_and_window <- function(then,
                            now = Sys.time(),
                            wait = as.numeric(Sys.getenv("PHOTOTWEETR_WAIT", one_week)),
                            window = c(8, 22),
                            verbose = TRUE) {
  if (now < then + wait) {
    if (verbose) message("Waiting until at least: ", then + wait)
    return(FALSE)
  }

  hour <- as.numeric(format(now, "%H"))
  if (!is.null(window) && {hour < window[[1]] | hour > window[[2]]}) {
    if (verbose) message("It is not during the window from ", window[[1]], " to ", window[[2]])
    return(FALSE)
  }

  return(TRUE)
}

one_week <- 60 * 60 * 24 * 7

#' A weighted coin
#'
#' Flips a coin that is weighted
#'
#' @param positive the odds of `TRUE` (default: 1)
#' @param negative the offs of `FALSE` (default: 14)
#'
#' @return logical
#' @export
weighted_coin <- function(positive = 1, negative = 14) {
  return(sample(c(TRUE, FALSE), 1, prob = c(positive, negative)))
}
