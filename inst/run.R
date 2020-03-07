library("phototweetr")
library(glue)

message(glue(
  "######################################################",
  "Starting phototweeter at {Sys.time()}",
  "######################################################\n",
  .sep = "\n"
  ))

### conect to the things
con <- connect("phototweetr.sql")
on.exit(DBI::dbDisconnect(con))

### process any triage photos
message("Queuing any photos in triage")
triage_photos <- list.files("triage", pattern = "(jpg|png|jpeg)$")
queue(triage_photos, con = con)

### determine if it's time to tweet
last_tweeted <- DBI::dbGetQuery(con, "SELECT date_tweeted FROM tweets;")
last_tweeted <- max(as.POSIXct(last_tweeted$date_tweeted))

if (is.na(last_tweeted)) {
  quit("There are no tweets, please tweet manually. Goodbye.")
} else if (!wait_and_window(last_tweeted)) {
  quit("It's not yet time to tweet again. Goodbye.")
} else if (!weighted_coin()) {
  quit("The weighted coin says this hour is not our hour. Goodbye.")
}

### pick a photo and tweet
message("Finding a photo to tweet")
to_tweet <- DBI::dbGetQuery(con, "SELECT * FROM tweets WHERE tweeted != 0;")
if (nrow(to_tweet) == 0) {
  quit("There are no more tweets queued. Goodbye.")
}

photo_to_tweet <- df[sample(nrow(df),1),]
message(glue("Found one: {photo_to_tweet$orig_file}"))

message("Authenticating with Twitter")
auth_rtweet()

messgage(glue("Tweeting out photo {photo_to_tweet$orig_file}"))
photo_df <- tweet_photo(photo_df)
update_one(photo_df, con)

### goodbye
quit("A photo has been tweeted and updated. Goodbye.")
