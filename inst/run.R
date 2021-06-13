# Must have setup:
# Dirs:
#   * orig - originals (i.e. where lightroom publishes to)
#   * processed - photos that have had their metadata cleaned for twitter

message(glue::glue(
  "",
  "######################################################",
  "Starting phototweeter at {Sys.time()}",
  "######################################################",
  .sep = "\n"
))

message("Installing the freshest phototweeter on main")
remotes::install_github("jonkeane/phototweetr")

message("Loading packages and the DB")
library("phototweetr")

### connect to the things
con <- connect("phototweetr.sql")
on.exit(DBI::dbDisconnect(con))

### process any orig photos
message("Queuing any updated photos")
triage_photos <- file.path("orig", list.files("orig", pattern = "(jpg|png|jpeg)$"))
db_response <- queue(triage_photos, con = con)

### determine if it's time to tweet
last_tweeted <- DBI::dbGetQuery(con, "SELECT date_tweeted FROM tweets;")

if (nrow(last_tweeted) < 1 || all(is.na(last_tweeted$date_tweeted))) {
  message("There are no tweets, please tweet manually. Goodbye.")
  quit("no")
}

last_tweeted <- max(as.POSIXct(last_tweeted$date_tweeted), na.rm = TRUE)

if (!wait_and_window(last_tweeted)) {
  message("It's not yet time to tweet again. Goodbye.")
  quit("no")
} else if (!weighted_coin()) {
  message("The weighted coin says this hour is not our hour. Goodbye.")
  quit("no")
}

### pick a photo and tweet
message("Finding a photo to tweet")
to_tweet <- DBI::dbGetQuery(con, "SELECT rowid, * FROM tweets WHERE tweeted == 0;")
if (nrow(to_tweet) == 0) {
  message("There are no more tweets queued. Goodbye.")
  quit("no")
}

photo_to_tweet <- to_tweet[sample(nrow(to_tweet),1),]
message(glue::glue("Found one: {photo_to_tweet$orig_file}"))

message("Authenticating with Twitter")
token <- auth_rtweet()

message(glue::glue("Tweeting out photo {photo_to_tweet$orig_file}"))
photo_to_tweet <- tweet_photo(photo_to_tweet, token = token)
update_one(photo_to_tweet, con)

### goodbye
message("A photo has been tweeted and updated. Goodbye.")
quit("no")
