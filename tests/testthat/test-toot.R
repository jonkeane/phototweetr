test_that("toot_photo works", {
  skip_if(Sys.info()['sysname'] == "Windows", "Windows and rtoot testing aren't friends")

})


test_that("toot_splitter", {
  # if it fits in one toot, it collapses
  text <- c(
    title = "one",
    comment = NA,
    tags = "#four",
    exposure = "two three"
  )
  expect_identical(
    toot_splitter(text),
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
    toot_splitter(text),
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
    toot_splitter(text),
    list(
      paste0(text["title"], "#twenty  #characters #again #more"),
      "#characters",
      text["comment"],
      text["exposure"]
    )
  )
})

test_that("toot_collapse()", {
  skip_if(Sys.info()['sysname'] == "Windows", "Unicode on windows \U1F643")
  toot_text <- list(
    c(
      title = "A title",
      comment = "comment",
      tags ="#tag #hash",
      exposure = "📸50mm • 1/45s f/4 800iso\n📷Canon EOS 6D EF24-105mm f/4L IS USM"
    )
  )
  expect_identical(
    toot_collapse(toot_text),
    glue::as_glue("A title\ncomment\n#tag #hash\n\U0001f4f850mm • 1/45s f/4 800iso\n\U0001f4f7Canon EOS 6D EF24-105mm f/4L IS USM")
  )

  toot_text[[1]]["comment"] <- NA
  expect_identical(
    toot_collapse(toot_text),
    glue::as_glue("A title\n#tag #hash\n\U0001f4f850mm • 1/45s f/4 800iso\n\U0001f4f7Canon EOS 6D EF24-105mm f/4L IS USM")
  )
})

