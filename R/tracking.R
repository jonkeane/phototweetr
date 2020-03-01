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
  date_added = character(0),
  date_tweeted = character(0),
  tweeted = integer(0),
  # these will be null for now, but might be useful
  store_url = character(0),
  direct_url = character(0),
  flickr_url = character(0)
)

add_row_statement <- function(df, con) {
  glue_sql(
    "INSERT INTO tweets (", glue_collapse(glue("{colnames(df)}"), sep = ', '), ")",
    "VALUES(", glue_collapse(glue("{{{colnames(df)}}}"), sep = ', '), ");",
    .con = con,
    .envir = df
  )
}
