#' @importFrom glue glue glue_collapse glue_sql
NULL

#' is null pipe
#'
#' borrowed from rlang
#'
#' @param a first thing
#' @param b what to do if the first thing is `NULL`
#'
#' @return a, unless a is `NULL` then b
#'
#' @name grapes_or_or_grapes
#' @keywords internal
#' @export
"%||%" <- function(a, b) if (!is.null(a)) a else b # nolint
