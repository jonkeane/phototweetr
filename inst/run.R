library("phototweetr")

# conect to the things
con <- connect("phototweetr.sql")
on.exit(DBI::dbDisconnect(con))

auth_rtweet()

# process any triage photos
# determine if it's time to tweet
# pick a photo and tweet
# goodbye
