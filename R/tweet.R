tweet_photo <- function(photo_df) {
  return(rtweet::post_tweet(
    status = photo_df$tweet_text,
    media = photo_df$tweet_file
  ))
}
