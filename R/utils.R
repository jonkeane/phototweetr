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

chunker <- function(chunks, max = twitter_max) {
  chunks <- unlist(strsplit(chunks, " "))
  if (all(is.na(chunks))) {
    return(NA)
  }
  chunk_lengths <- vapply(chunks, utf8::utf8_width, integer(1)) + 1
  start <- 1
  chunked_chunks <- list()
  for (i in seq_along(chunk_lengths)) {
    end <- i
    if (sum(chunk_lengths[start:end], chunk_lengths[end+1], na.rm = TRUE) > twitter_max) {

      chunked_chunks[length(chunked_chunks) + 1] <- list(chunks[start:end])
      start <- end + 1
    }

    # clean up at the end
    if (end == length(chunk_lengths)) {
      chunked_chunks[length(chunked_chunks) + 1] <- list(chunks[start:end])
    }
  }

  chunks <- unlist(lapply(chunked_chunks, paste, collapse = " "))

  return(chunks)
}
