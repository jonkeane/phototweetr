con <- connect(":memory:", clean = TRUE)

test_that("add_row_statement works", {
  out <- add_row_statement(
    data.frame(
      id = 0,
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
      "INSERT INTO tweets (id, orig_file, tweet_file, tweet_text, date_added, date_tweeted, tweeted)",
      "VALUES(0, 'new.jpg', 'copy/new.jpg', 'whoa', '2020-01-01', NULL, 0);"
    )
  )
})

DBI::dbDisconnect(con)
