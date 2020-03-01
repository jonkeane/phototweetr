con <- connect(":memory:", clean = TRUE)

test_that("Queueing and processing", {
  # setup
  dir.create(test_path("orig"))
  dir.create(test_path("processed"))
  file.copy(test_path("IMG_4907.jpg"), test_path("copy.jpg"))

  expect_true(
    queue(test_path("copy.jpg"), con, orig_dir = test_path("orig"), proc_dir = test_path("processed")),
  )

  # queue 1
  data_out <- DBI::dbGetQuery(con, "SELECT * FROM tweets")
  expect_identical(data_out$orig_file, file.path(test_path("orig"), "copy.jpg"))
  expect_identical(data_out$tweet_file, file.path(test_path("processed"), "copy.jpg"))
  expect_true(grepl("Fuji from Hakone II", data_out$tweet_text))
  expect_identical(data_out$tweeted, 0L)


  # queue 2
  file.copy(test_path("IMG_4907.jpg"), test_path("copy-1.jpg"))
  file.copy(test_path("IMG_4907.jpg"), test_path("copy-2.jpg"))

  expect_true(
    queue(
      c(test_path("copy-1.jpg"), test_path("copy-2.jpg")),
      con,
      orig_dir = test_path("orig"),
      proc_dir = test_path("processed")
    )
  )

  data_out <- DBI::dbGetQuery(con, "SELECT * FROM tweets")
  expect_identical(nrow(data_out), 3L)

  # teardown
  unlink(
    c(test_path("orig"), test_path("processed"),test_path("copy.jpg")),
    recursive = TRUE
  )
})

DBI::dbDisconnect(con)
