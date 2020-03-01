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
  orig_file = character(0),
  tweet_file = character(0),
  tweet_text = character(0),
  date_added = character(0),
  date_tweeted = character(0),
  tweeted = logical(0),
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

queue <- function(photo, con, orig_dir = "orig", proc_dir = "processed") {
  # extract exif
  exif_data <- exifr::read_exif(photo)

  # move file to orig, processed and clean
  orig <- file.path(orig_dir, basename(photo))
  proced <- file.path(proc_dir, basename(photo))
  file.copy(photo, orig)
  file.copy(photo, proced)
  sanitize_exif(proced, exif_data)
  file.remove(photo)

  # add to DB
  df <- data.frame(
    orig_file = orig,
    tweet_file = proced,
    tweet_text = format_exif(exif_data),
    date_added = Sys.time(),
    tweeted = FALSE
  )
  statement <- add_row_statement(df, con)
  return(DBI::dbExecute(con, statement))
}
