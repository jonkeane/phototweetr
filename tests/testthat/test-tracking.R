# setup
con <- connect(":memory:", clean = TRUE)
dir.create(test_path("processed"))

# teardown
on.exit({
  unlink(test_path("processed"), recursive = TRUE)
  DBI::dbDisconnect(con)
})

test_that("Queueing and processing", {
  expect_true(
    queue(test_path("orig", "IMG_4907.jpg"), con, proc_dir = test_path("processed"))
  )

  # queue 1
  data_out <- DBI::dbGetQuery(con, "SELECT * FROM tweets")
  expect_identical(data_out$orig_file, test_path("orig", "IMG_4907.jpg"))
  expect_identical(data_out$tweet_file, test_path("processed", "IMG_4907.jpg"))
  expect_true(grepl("Fuji from Hakone II", data_out$title))
  expect_true(grepl("Film Make: Ilford\nFilm Type: Pan F+", data_out$comment))
  expect_true(grepl("A photo of mount Fuji. Snow covers th", data_out$alt_text))
  expect_true(grepl("#Fuji-san", data_out$tags))
  expect_identical(data_out$tweeted, 0L)

  # queue 2
  file.copy(test_path("orig", "IMG_4907.jpg"), test_path("orig", "copy-1.jpg"))
  file.copy(test_path("orig", "IMG_4907.jpg"), test_path("orig", "copy-2.jpg"))

  expect_true(
    queue(
      c(test_path("orig", "copy-1.jpg"), test_path("orig", "copy-2.jpg")),
      con,
      proc_dir = test_path("processed")
    )
  )

  data_out <- DBI::dbGetQuery(con, "SELECT * FROM tweets")
  expect_identical(nrow(data_out), 3L)
})

test_that("not in the db already", {
  # not in DB
  photo_df <- data.frame(
    rowid = NA,
    orig_file = test_path("orig", "IMG_4907.jpg"),
    date_added = NA,
    stringsAsFactors = FALSE
  )

  out <- process_one(photo_df, test_path("processed"), con)

  expect_false(out$tweeted)
  expect_true(grepl("processed/IMG_4907.jpg", out$tweet_file))
})

test_that("already in DB, need to update", {
  photo_df <- data.frame(
    rowid = 1L,
    orig_file = test_path("orig", "IMG_4907.jpg"),
    date_added = "2000-01-01 00:00:00",
    stringsAsFactors = FALSE,
    tweeted = 0L
  )

  out <- process_one(photo_df, test_path("processed"), con)

  expect_false(out$tweeted)
  expect_true(grepl("processed/IMG_4907.jpg", out$tweet_file))
})

test_that("already in DB, don't need to update", {
  photo_df <- data.frame(
    rowid = 1L,
    orig_file = test_path("orig", "IMG_4907.jpg"),
    date_added = "2040-12-30 00:00:00",
    stringsAsFactors = FALSE
  )

  out <- process_one(photo_df, test_path("processed"), con)
  expect_null(out)
})

test_that("Update", {
  data <- DBI::dbGetQuery(con, "SELECT rowid, * FROM tweets LIMIT 1")
  expect_identical(data$tweeted, 0L)

  data$tweeted <- 1L
  expect_silent(update_one(data, con))
  data <- DBI::dbGetQuery(con, "SELECT rowid, * FROM tweets LIMIT 1")
  expect_identical(data$tweeted, 1L)
})

test_that("schema_update", {
  # add a column to the schema
  new_schema <- schema
  new_schema$new_col <- character(0)

  # add a few photos
  queue(
    c(test_path("orig", "copy-1.jpg"), test_path("orig", "copy-2.jpg")),
    con,
    proc_dir = test_path("processed")
  )

  # simulate what happens in process_many
  photos_to_process <- get_photos_to_proc(con, test_path(c("orig/IMG_4907.jpg", "orig/copy-1.jpg", "orig/copy-2.jpg")))
  photos_to_process$date_added <- as.POSIXct(photos_to_process$date_added)

  expect_message(
    new_process <- schema_update(con, photos_to_process, new_schema = new_schema),
    "The schema has updated"
  )

  # the date has been updated
  expect_equal(new_process$date_added, rep(as.POSIXct("1969-01-01 00:00:00"), 3))

  # the schema has been updated
  db_schema <- DBI::dbGetQuery(
    con, "SELECT * FROM tweets LIMIT 0;"
  )
  expect_setequal(colnames(new_schema), colnames(db_schema))

  # there are still three rows, but the dates updated are new
  data_out <- DBI::dbGetQuery(con, "SELECT * FROM tweets")
  expect_identical(nrow(data_out), 2L)
  expect_true(all(data_out$date_added > as.POSIXct("1969-01-01 00:00:00")))
})


test_that("schema_update is a no-op if there are no changes", {
  # add a few photos
  queue(
    c(test_path("orig", "copy-1.jpg"), test_path("orig", "copy-2.jpg")),
    con,
    proc_dir = test_path("processed")
  )

  # simulate what happens in process_many
  photos_to_process <- get_photos_to_proc(con, test_path(c("orig/IMG_4907.jpg", "orig/copy-1.jpg", "orig/copy-2.jpg")))
  photos_to_process$date_added <- as.POSIXct(photos_to_process$date_added)

  # schema_update is a no-op if there are no changes
  expect_message(
    new_process <- schema_update(con, photos_to_process, new_schema = schema),
    NA
  )

  expect_identical(photos_to_process, new_process)
})
