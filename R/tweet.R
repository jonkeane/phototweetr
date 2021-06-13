#' Tweet a photo out
#'
#' @param photo_df a dataframe with information about the photo to be tweeted
#' @param path the path to the `phototweetr` directory (the one above "orig"
#' typically)
#' @param ... arguments passed to `rtweet::post_tweet`
#'
#' @return the `photo_df`, updated with tweet time, etc.
#' @export
tweet_photo <- function(photo_df, path = NULL, ...) {
  message(glue("Attempting to tweet photo {photo_df$tweet_file}"))
  if (!is.null(path)) {
    image_path <- file.path(path, photo_df$tweet_file)
  } else {
    image_path <- photo_df$tweet_file
  }

  tweet_info <- unlist(photo_df[,c("title", "comment", "tags", "exposure")])
  tweet_text <- tweet_splitter(tweet_info)
  image_alt <- photo_df$alt_text

  if (length(tweet_text) == 1) {
    tweeted_text <- tweet_collapse(tweet_text)
    tweet_response <- rtweet::post_tweet(status = tweeted_text, media = image_path, ..., media_alt_text = image_alt)
  } else {
    text <- tweet_collapse(tweet_text[[1]])
    tweet_response <- rtweet::post_tweet(status = text, media = image_path, ..., media_alt_text = image_alt)
    reply_id <- httr::content(tweet_response)$id_str

    # Iterate through each reply after the first
    for (tweet in tweet_text[-1]) {
      tweet_response <- rtweet::post_tweet(
        status = tweet_collapse(tweet),
        in_reply_to_status_id = reply_id,
        auto_populate_reply_metadata = TRUE,
        ...
      )
      reply_id <- httr::content(tweet_response)$id_str
    }
    tweeted_text <- paste0(lapply(tweet_text, tweet_collapse), collapse = "\\\\")
  }

  if (httr::status_code(tweet_response) == 200L) {
    photo_df$tweeted <- TRUE
    photo_df$tweeted_text <- tweeted_text
    photo_df$date_tweeted <- Sys.time()
  } else {
    photo_df$tweet_error <- httr::content(tweet_response)
  }

  return(photo_df)
}

#' Ensure that `rtweet` is authenticated
#'
#' This will call `rtweet::create_token()`. If a token is being created, the
#' following environment variables are used/needed:
#'
#' * `rtweet_api_key`
#' * `rtweet_api_secret_key`
#' * `rtweet_access_token`
#' * `rtweet_access_token_secret`
#'
#' @param ... arguments passed to `rtweet::create_token()`
#'
#' @export
auth_rtweet <- function() {
  return(rtweet::rtweet_bot(
    api_key = Sys.getenv("rtweet_api_key"),
    api_secret = Sys.getenv("rtweet_api_secret_key"),
    access_token = Sys.getenv("rtweet_access_token"),
    access_secret = Sys.getenv("rtweet_access_token_secret")
  ))
}

twitter_max <- 280
tweet_breaker <- "\\"

tweet_splitter <- function(text) {
  # TODO: how to count emojis as 2 characters, CJK as 2 characters, etc.
  # https://developer.twitter.com/en/docs/basics/counting-characters
  # currently this uses utf8::utf8_width, which is close, but over counts
  # combining emojis

  # if the text all together is within one tweet, we're done. But ensure that
  # the order is title comment exposure tags
  if (check_length(text)) {
    return(list(text[c("title", "comment", "exposure", "tags")]))
  }

  # first, try to put the photo settings in the next tweet
  # the settings should always be under one tweet, so no need to check those
  if (check_length(text[c("title", "exposure", "tags")]) &
      check_length(text["comment"])) {
    return(list(text[c("title", "exposure", "tags")], text["comment"]))
  }

  # finally, start with the title tags and then comment, and split them in to
  # chunks that are under the limit the tags are second here so the first tweet
  # has at least some tags
  chunks <- as.list(c(
    chunker(text[c("title", "tags")]),
    chunker(text[c("comment")]),
    text["exposure"]
  ))

  return(chunks)
}

chunker <- function(chunks) {
  chunks <- unlist(strsplit(chunks, " "))
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

#' Collapse tweet text
#'
#' @param text text to flatten
#'
#' @return the text flattened with new lines
#' @export
tweet_collapse <- function(text) {
  text <- unlist(text)
  text <- Filter(Negate(is.na), text)

  return(glue::glue_collapse(text, sep = "\n"))
}

check_length <- function(input, max = twitter_max) {
  input <- tweet_collapse(input)

  n_char <- sum(utf8::utf8_width(as.character(input)), na.rm = TRUE)

  return(n_char <= twitter_max)
}

