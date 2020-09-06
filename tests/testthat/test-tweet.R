test_that("tweet_photo works", {
  skip_if(Sys.info()['sysname'] == "Windows", "Windows and rtweet testing aren't friends")
  with_mock_api({
    token <- auth_rtweet(set_renv = FALSE)

    photo_df <- data.frame(
      orig_file = test_path("orig", "IMG_4907.jpg"),
      tweet_file = test_path("orig", "IMG_4907.jpg"),
      title = "this is a title",
      comment = "this is a test tweet with an image",
      tags = "#tag #tag2",
      exposure = "ISO etc.",
      alt_text = "This is an image description",
      tweeted_text =  NA_character_,
      date_added = "2020-03-07 07:53:04",
      date_tweeted = NA_character_,
      tweeted = 0L,
      tweet_error = NA_character_,
      stringsAsFactors = FALSE
    )

    out_photo_df <- tweet_photo(photo_df, token = token)

    expect_identical(out_photo_df$tweeted, TRUE)
    expect_identical(
      out_photo_df$tweeted_text,
      glue_collapse(
        c("this is a title", "this is a test tweet with an image", "ISO etc.", "#tag #tag2"),
        sep = "\n"
      ))
    expect_equal(out_photo_df$date_tweeted, Sys.time(), tolerance = 1)
  })
})


test_that("tweet_splitter", {
  # if it fits in one tweet, it collapses
  text <- c(
    title = "one",
    comment = NA,
    tags = "#four",
    exposure = "two three"
  )
  expect_identical(
    tweet_splitter(text),
    list(text[c("title", "comment", "exposure", "tags")])
  )

  # if the title+exposure+tags fit, split that way
  text <- c(
    title = "title",
    comment = paste(rep("1111", 50), collapse = " "),
    exposure = "two three",
    tags = "#twenty  #characters #again #more #characters"
  )
  expect_equivalent(
    tweet_splitter(text),
    list(
      text[c("title", "exposure", "tags")],
      text["comment"]
    )
  )

  # if the title+comment+tags fit, split that way
  text <- c(
    title = paste(rep("title ", 40), collapse = ""),
    comment = paste(rep("1111", 50), collapse = " "),
    exposure = "two three",
    tags = "#twenty  #characters #again #more #characters"
  )
  expect_equivalent(
    tweet_splitter(text),
    list(
      paste0(text["title"], "#twenty  #characters #again #more"),
      "#characters",
      text["comment"],
      text["exposure"]
    )
  )
})

test_that("tweet_collapse()", {
  skip_if(Sys.info()['sysname'] == "Windows", "Unicode on windows \U1F643")
  tweet_text <- list(
    c(
      title = "A title",
      comment = "comment",
      tags ="#tag #hash",
      exposure = "📸50mm • 1/45s f/4 800iso\n📷Canon EOS 6D EF24-105mm f/4L IS USM"
    )
  )
  expect_identical(
    tweet_collapse(tweet_text),
    glue::as_glue("A title\ncomment\n#tag #hash\n\U0001f4f850mm • 1/45s f/4 800iso\n\U0001f4f7Canon EOS 6D EF24-105mm f/4L IS USM")
  )

  tweet_text[[1]]["comment"] <- NA
  expect_identical(
    tweet_collapse(tweet_text),
    glue::as_glue("A title\n#tag #hash\n\U0001f4f850mm • 1/45s f/4 800iso\n\U0001f4f7Canon EOS 6D EF24-105mm f/4L IS USM")
  )
})
