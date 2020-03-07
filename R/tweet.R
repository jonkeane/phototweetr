#' Tweet a photo out
#'
#' @param photo_df a dataframe with information about the photo to be tweeted
#' @param ... arguments passed to `rtweet::post_tweet`
#'
#' @return the `photo_df`, updated with tweet time, etc.
#' @export
tweet_photo <- function(photo_df, ...) {
  message(glue("Attempting to tweet photo {photo_df$tweet_file}"))
  tweet <- rtweet::post_tweet(
    status = photo_df$tweet_text,
    media = photo_df$tweet_file,
    ...
  )

  if (httr::status_code(tweet) == 200L) {
    photo_df$tweeted <- TRUE
    photo_df$date_tweeted <- Sys.time()
  } else {
    photo_df$tweet_error <- httr::content(tweet)
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
#' ... arguments passed to `rtweet::create_token()`
#'
#' @export
auth_rtweet <- function(...) {
  return(rtweet::create_token(
    app = "phototweetr",
    consumer_key = Sys.getenv("rtweet_api_key"),
    consumer_secret = Sys.getenv("rtweet_api_secret_key"),
    access_token = Sys.getenv("rtweet_access_token"),
    access_secret = Sys.getenv("rtweet_access_token_secret"),
    ...
  ))
}
