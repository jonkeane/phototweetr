con <- connect(":memory:", clean = TRUE)

test_that("Queueing and processing", {
  # setup
  dir.create(test_path("processed"))

  expect_true(
    queue(test_path("orig", "IMG_4907.jpg"), con, proc_dir = test_path("processed")),
  )

  # queue 1
  data_out <- DBI::dbGetQuery(con, "SELECT * FROM tweets")
  expect_identical(data_out$orig_file, test_path("orig", "IMG_4907.jpg"))
  expect_identical(data_out$tweet_file, test_path("processed", "IMG_4907.jpg"))
  expect_true(grepl("Fuji from Hakone II", data_out$tweet_text))
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

  # teardown
  unlink(test_path("processed"), recursive = TRUE)
})

test_that("Update", {
  data <- DBI::dbGetQuery(con, "SELECT rowid, * FROM tweets LIMIT 1")
  expect_identical(data$tweeted, 0L)

  data$tweeted <- 1L
  expect_silent(update_one(data, con))
  data <- DBI::dbGetQuery(con, "SELECT rowid, * FROM tweets LIMIT 1")
  expect_identical(data$tweeted, 1L)

})

DBI::dbDisconnect(con)
