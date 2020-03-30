test_that("tweet_photo works", {
  skip_if(Sys.info()['sysname'] == "Windows", "Windows and rtweet testing aren't friends")
  with_mock_api({
    auth_rtweet()

    photo_df <- data.frame(
      orig_file = test_path("orig", "IMG_4907.jpg"),
      tweet_file = test_path("orig", "IMG_4907.jpg"),
      caption = "this is a test tweet with an image",
      tags = "#tag #tag2",
      exposure = "ISO etc.",
      tweeted_text =  NA_character_,
      date_added = "2020-03-07 07:53:04",
      date_tweeted = NA_character_,
      tweeted = 0L,
      tweet_error = NA_character_,
      stringsAsFactors = FALSE
    )

    out_photo_df <- tweet_photo(photo_df)

    expect_identical(out_photo_df$tweeted, TRUE)
    expect_identical(
      out_photo_df$tweeted_text,
      glue_collapse(
        c("this is a test tweet with an image", "ISO etc.", "#tag #tag2"),
        sep = "\n"
      ))
    expect_equal(out_photo_df$date_tweeted, Sys.time(), tolerance = 1)
  })
})


test_that("tweet_splitter", {
  # if it fits in one tweet, it collapses
  text <- c(
    caption = "one",
    tags = "#four",
    exposure = "two three"
  )
  expect_identical(
    tweet_splitter(text),
    list(text[c("caption", "exposure", "tags")])
  )

  # if the caption+tags fit, make the exposure be a second tweet
  text <- c(
    caption = paste(rep("1", 255), collapse = ""),
    exposure = "two three",
    tags = "#twenty  #characters"
  )
  expect_equivalent(
    tweet_splitter(text),
    list(
      text[c("caption", "tags")],
      text["exposure"]
    )
  )

  # if the caption+some tags fit, split that way
  text <- c(
    caption = paste(rep("1111 ", 51), collapse = ""),
    exposure = "two three",
    tags = "#twenty  #characters #again #more #characters"
  )
  expect_equivalent(
    tweet_splitter(text),
    list(
      paste0(text["caption"], "#twenty  #characters", collapse = " "),
      c("#again #more #characters", text["exposure"])
    )
  )

  # but if the exposure info would break, that goes separately
  text <- c(
    caption = paste(rep("1111 ", 51), collapse = ""),
    exposure = paste(c("two\n", rep("three ", 45)), collapse = ""),
    tags = "#twenty  #characters #again #more #characters"
  )
  expect_equivalent(
    tweet_splitter(text),
    list(
      paste0(text["caption"], "#twenty  #characters", collapse = " "),
      "#again #more #characters",
      text["exposure"]
    )
  )
})
