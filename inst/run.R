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

# this shouldn't be necesary, but ensure that the seed is unset (in case it
# happens to be set for the user)
set.seed(NULL)

message("Installing the freshest phototweeter on main")
remotes::install_github("jonkeane/phototweetr")
# For now, need the newest
remotes::install_github("schochastics/rtoot")

message("Loading packages and the DB")
library("phototweetr")

### connect to the things
con <- connect("phototweetr.sql")
on.exit(DBI::dbDisconnect(con))

### process any orig photos
message("Queuing any updated photos")
triage_photos <- file.path("orig", list.files("orig", pattern = "(jpg|png|jpeg)$"))
db_response <- queue(triage_photos, con = con)

### verify authentication
rtoot::verify_envvar()

### determine if it's time to toot
last_tooted <- DBI::dbGetQuery(con, "SELECT date_tweeted FROM tweets;")

if (nrow(last_tooted) < 1 || all(is.na(last_tooted$date_tweeted))) {
  message("There are no toots, please toot manually. Goodbye.")
  quit("no")
}

last_tooted <- max(as.POSIXct(last_tooted$date_tweeted), na.rm = TRUE)

if (!wait_and_window(last_tooted)) {
  message("It's not yet time to toot again. Goodbye.")
  quit("no")
} else if (!weighted_coin()) {
  message("The weighted coin says this hour is not our hour. Goodbye.")
  quit("no")
}

### pick a photo and toot
message("Finding a photo to toot")
to_toot <- DBI::dbGetQuery(con, "SELECT rowid, * FROM tweets WHERE tweeted == 0;")
if (nrow(to_toot) == 0) {
  message("There are no more toots queued. Goodbye.")
  quit("no")
}

photo_to_toot <- to_toot[sample(nrow(to_toot),1),]
message(glue::glue("Found one: {photo_to_toot$orig_file}"))

message(glue::glue("Tooting out photo {photo_to_toot$orig_file}"))
photo_to_toot <- toot_photo(photo_to_toot)
update_one(photo_to_toot, con)

### goodbye
message("A photo has been tooted and updated. Goodbye.")
quit("no")
