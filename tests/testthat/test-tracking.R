con <- con(":memory:", clean = TRUE)

test_that("add_row_statement works", {
  out <- add_row_statement(
    data.frame(
      orig_file = "new.jpg",
      tweet_file = "copy/new.jpg",
      tweet_text = "whoa",
      date = "2020-01-01",
      tweeted = 0
    ),
    con
  )

  expect_s4_class(out, "SQL")
  expect_identical(
    as.character(out),
    paste0(
      "INSERT INTO tweets (orig_file, tweet_file, tweet_text, date, tweeted)",
      "VALUES('new.jpg', 'copy/new.jpg', 'whoa', '2020-01-01', 0);"
    )
  )
})

DBI::dbDisconnect(con)
