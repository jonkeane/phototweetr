#' Connect to the sqlite tracking DB
#'
#' @param db_path path to the db
#' @param clean logical, should the db be unlinked and re-initialized
#'
#' @return connection
#' @export
connect <- function(db_path, clean = FALSE) {
  if (clean) {
    unlink(db_path)
  }

  if(!file.exists(db_path)) {
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
  tweeted = integer(0),
  tweet_error = character(0),
  # these will be null for now, but might be useful
  tweet_geo = character(0),
  store_url = character(0),
  direct_url = character(0),
  flickr_url = character(0)
)

#' Queue photos for tweeting
#'
#' Queue photo(s) for tweeting. This inserts them into the db but does not
#' actually tweet them out (yet)
#'
#' @param photo path to the photo
#' @param con the databse to use
#' @param proc_dir directory to place the processed files
#'
#' @return `TRUE` silently, if successful
#' @export
queue <- function(photo, con, orig_dir = "orig", proc_dir = "processed") {
  df <- process_many(photo, proc_dir = proc_dir)

  if (is.null(df)) {
    return(NULL)
  }

  # TODO: what to do if the photo is already there?

  # add to DB
  return(DBI::dbWriteTable(con, "tweets", df, append = TRUE))
}

process_many <- function(photo, orig_dir, proc_dir) {
  return(do.call(rbind, lapply(photo, process_one, proc_dir = proc_dir)))
}

process_one <- function(photo, proc_dir) {
  # extract exif
  exif_data <- exifr::read_exif(photo)

  # move file to orig, processed and clean
  proced <- file.path(proc_dir, basename(photo))
  file.copy(photo, proced)
  sanitize_exif(proced, exif_data)

  return(data.frame(
    orig_file = photo,
    tweet_file = proced,
    tweet_text = format_exif(exif_data),
    date_added = Sys.time(),
    tweeted = FALSE,
    stringsAsFactors = FALSE
  ))
}

#' Update a photo in the tracking db
#'
#' @param df dataframe with updated photo information (with a single row)
#' @param con a sqlite connection
#'
#' @export
update_one <- function(df, con) {
  if (!"rowid" %in% colnames(df)) {
    stop("The dataframe in `df` must have a rowid column.", call. = FALSE)
  }
  DBI::dbExecute(con, glue_sql("DELETE FROM tweets WHERE rowid = {df$rowid};"))
  DBI::dbAppendTable(con, "tweets", df)
}
