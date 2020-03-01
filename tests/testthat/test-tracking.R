con <- connect(":memory:", clean = TRUE)

test_that("add_row_statement works", {
  out <- add_row_statement(
    data.frame(
      orig_file = "new.jpg",
      tweet_file = "copy/new.jpg",
      tweet_text = "whoa",
      date_added = "2020-01-01",
      date_tweeted = NA,
      tweeted = 0
    ),
    con
  )

  expect_s4_class(out, "SQL")
  expect_identical(
    as.character(out),
    paste0(
      "INSERT INTO tweets (orig_file, tweet_file, tweet_text, date_added, date_tweeted, tweeted)",
      "VALUES('new.jpg', 'copy/new.jpg', 'whoa', '2020-01-01', NULL, 0);"
    )
  )
})

test_that("Queueing", {
  dir.create(test_path("orig"))
  dir.create(test_path("processed"))
  file.copy(test_path("IMG_4907.jpg"), test_path("copy.jpg"))

  expect_identical(
    queue(test_path("copy.jpg"), con, orig_dir = test_path("orig"), proc_dir = test_path("processed")),
    1L
  )

  data_out <- DBI::dbGetQuery(con, "SELECT * FROM tweets")
  expect_identical(data_out$orig_file, file.path(test_path("orig"), "copy.jpg"))
  expect_identical(data_out$tweet_file, file.path(test_path("processed"), "copy.jpg"))
  expect_true(grepl("Fuji from Hakone II", data_out$tweet_text))
  expect_identical(data_out$tweeted, 0L)

  unlink(
    c(test_path("orig"), test_path("processed"),test_path("copy.jpg")),
    recursive = TRUE
  )
})

DBI::dbDisconnect(con)
