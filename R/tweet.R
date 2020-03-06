tweet_photo <- function(photo_df, con) {
  message(glue("Attempting to tweet photo {photo_df$tweet_file}"))
  tweet <- rtweet::post_tweet(
    status = photo_df$tweet_text,
    media = photo_df$tweet_file
  )

  if (httr::status_code(tweet) == 200L) {
    photo_df$tweeted <- TRUE
    photo_df$date_tweeted <- Sys.time()
  } else {
    photo_df$tweet_error <- httr::content(tweet)
  }

  return(update_one(photo_df, con))
}

auth_rtweet <- function() {
  rtweet::create_token(
    app = "phototweetr",
    consumer_key = Sys.getenv("rtweet_api_key"),
    consumer_secret = Sys.getenv("rtweet_api_secret_key"),
    access_token = Sys.getenv("rtweet_access_token"),
    access_secret = Sys.getenv("rtweet_access_token_secret")
  )
}
