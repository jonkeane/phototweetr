connect <- function(db_path, clean = FALSE) {
  if (clean) {
    unlink(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    DBI::dbWriteTable(con, "tweets", schema)

    return(con)
  }

  return(DBI::dbConnect(RSQLite::SQLite(), db_path))
}

schema <- data.frame(
  id = integer(0),
  orig_file = character(0),
  tweet_file = character(0),
  tweet_text = character(0),
  date = character(0),
  tweeted = integer(0)
)

add_row_statement <- function(df, con) {
  glue::glue_sql(
    paste0(
      "INSERT INTO tweets (orig_file, tweet_file, tweet_text, date, tweeted)",
      "VALUES({orig_file}, {tweet_file}, {tweet_text}, {date}, {tweeted});"
    ),
    .con = con,
    .envir = df
  )
}
