#' Wait and window
#'
#' Compare a time from the past (`then`) to the current time (`now`, deafult:
#' `Sys.time()`) and see if `wait` seconds (default: 1 week) has passed.
#' Additionaly, only return `TRUE` if the current time is between the hours
#' given in `window` (if `window` is null, then always return)
#'
#' @param then a previous time (`POSIXct`)
#' @param now the current time (default: `Sys.time()`)
#' @param wait the seconds to wait between `then` and `now`
#' @param window hours between which to return `TRUE`
#'
#' @return logical
#' @export
wait_and_window <- function(then,
                            now = Sys.time(),
                            wait = one_week,
                            window = c(8, 22)) {
  if (now < then + wait) {
    return(FALSE)
  }

  hour <- as.numeric(format(then, "%H"))
  if (!is.null(window) && {hour < window[[1]] | hour > window[[2]]}) {
    return(FALSE)
  }

  return(TRUE)
}

one_week <- 60 * 60 * 24 * 7

weighted_coin <- function(positive = 1, negative = 14) {
  return(sample(c(TRUE, FALSE), 1, prob = c(positive, negative)))
}
