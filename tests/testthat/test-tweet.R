test_that("tweet_photo works", {
  with_mock_api({
    photo_df <- data.frame(
      orig_file = test_path("IMG_4907.jpg"),
      tweet_file = test_path("IMG_4907.jpg"),
      tweet_text = "this is a test tweet with an image",
      date_added = "2020-03-07 07:53:04",
      date_tweeted = NA_character_,
      tweeted = 0L,
      tweet_error = NA_character_,
      stringsAsFactors = FALSE
    )

    out_photo_df <- tweet_photo(photo_df)

    expect_identical(out_photo_df$tweeted, TRUE)
    expect_equal(out_photo_df$date_tweeted, Sys.time(), tolerance = 1)
  })
})
