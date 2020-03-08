#' Connect to the sqlite tracking DB
#'
#' @param db_path path to the db
#' @param clean logical, should the db be unlinked and reinitialized
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
#' @param con the database to use
#' @param proc_dir directory to place the processed files
#'
#' @return `TRUE` silently, if successful
#' @export
queue <- function(photo, con, proc_dir = "processed") {
  df <- process_many(photo, proc_dir = proc_dir, con = con)

  if (is.null(df)) {
    return(NULL)
  }

  # TODO: what to do if the photo is already there?

  # add to DB
  return(DBI::dbWriteTable(con, "tweets", ugh_dates(df), append = TRUE))
}

process_many <- function(photo, proc_dir, con) {
  photos_in_db <- DBI::dbGetQuery(
    con, "SELECT rowid, orig_file, date_added FROM tweets WHERE tweeted == 0;"
  )

  photos_in_folder <- data.frame(orig_file = photo, stringsAsFactors = FALSE)
  photos_to_process <- merge(photos_in_folder, photos_in_db, all.x = TRUE)

  photos_to_process$date_added <- as.POSIXct(photos_to_process$date_added)

  return(do.call(rbind, by(
    photos_to_process,
    seq_len(nrow(photos_to_process)),
    process_one,
    proc_dir = proc_dir,
    con = con,
    simplify = F
  )))
}

process_one <- function(photo_df, proc_dir, con) {
  if (!is.na(photo_df$rowid)) {
    # there is something in the DB already
    return(maybe_update_one(photo_df, proc_dir, con = con))
  } else {
    return(add_new_one(photo_df, proc_dir))
  }
}

maybe_update_one <- function(photo_df, proc_dir, con) {
  photo <- photo_df$orig_file
  # check if the file version is newer than the DB
  file_mtime <- file.mtime(photo)
  if (file_mtime > photo_df$date_added) {
    # if the file mtime is after the db time, then update the photo
    # TODO: remove from the DB
    DBI::dbExecute(con, glue_sql("DELETE FROM tweets WHERE rowid = {photo_df$rowid};"))

    return(add_new_one(photo_df, proc_dir))
  }

  # otherwise, do nothing.
  return(NULL)
}

add_new_one <- function(photo_df, proc_dir) {
  photo <- photo_df$orig_file

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
#' @param photo_df dataframe with updated photo information (with a single row)
#' @param con a sqlite connection
#'
#' @export
update_one <- function(photo_df, con) {
  if (!"rowid" %in% colnames(photo_df)) {
    stop("The dataframe in `photo_df` must have a rowid column.", call. = FALSE)
  }
  DBI::dbExecute(con, glue_sql("DELETE FROM tweets WHERE rowid = {photo_df$rowid};"))
  DBI::dbAppendTable(con, "tweets", ugh_dates(photo_df))
}

ugh_dates <- function(photo_df) {
  for(col in c("date_added", "date_tweeted")) {
    if (col %in% colnames(photo_df)) {
      photo_df[[col]] <- as.character(photo_df[[col]])
    }
  }
  return(photo_df)
}
