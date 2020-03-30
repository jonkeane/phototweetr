exif_data <- exifr::read_exif(test_path("orig", "IMG_4907.jpg"))

test_that("formatting", {
  skip_if(Sys.info()['sysname'] == "Windows", "Unicode on windows \U1F643")
  expect_equal(
    format_exif(exif_data),
    paste0(
      "Fuji from Hakone II • A photo of mount Fuji. Snow covers the top ",
      "1/3 of the mountain with trails visible zig-zagging up.\n\U0001f4f8",
      "105mm • 1/500s f/9.5 100iso\n\U0001f4f7Canon EOS 6D EF24-105mm f/4L IS ",
      "USM\n#Fuji-san #Hakone #Japan #MountFuji #Mountain #富士山 #日本 ",
      "#箱根"
    )
  )
})

test_that("separators", {
  exif_data <- data.frame(
    Title = "Title",
    Description = "Description",
    stringsAsFactors = FALSE
  )

  expect_equal(
    text(exif_data),
    "Title \u2022 Description"
  )

  exif_data$Title <- NULL
  expect_equal(
    text(exif_data),
    "Description"
  )
})

test_that("tweet_splitter", {
  # if it fits in one tweet, it collapses
  text <- c(
    caption = "one",
    exposure = "two",
    camera = "three",
    tags = "#four"
  )
  expect_identical(tweet_splitter(text), tweet_collapse(text))

  # if the caption+tags fit, make the exposure be a second tweet
  text <- c(
    caption = paste(rep("1", 255), collapse = ""),
    exposure = "two",
    camera = "three",
    tags = "#twenty  #characters"
  )
  expect_identical(
    tweet_splitter(text),
    glue_collapse(c(
      tweet_collapse(text[c("caption", "tags")]),
      tweet_collapse(text[c("exposure", "camera")])
    ),
    sep = "{{tweet break}}"
    )
  )

  # if the caption+some tags fit, split that way
  text <- c(
    caption = paste(rep("1111 ", 51), collapse = ""),
    exposure = "two",
    camera = "three",
    tags = "#twenty  #characters #again #more #characters"
  )
  expect_identical(
    tweet_splitter(text),
    glue_collapse(c(
      tweet_collapse(c(text["caption"], "#twenty  #characters")),
      tweet_collapse(c("#again #more #characters", text[c("exposure", "camera")]))
    ),
    sep = "{{tweet break}}"
    )
  )

  # but if the exposure info would break, that goes separately
  text <- c(
    caption = paste(rep("1111 ", 51), collapse = ""),
    exposure = "two",
    camera = paste(rep("three ", 45), collapse = ""),
    tags = "#twenty  #characters #again #more #characters"
  )
  expect_identical(
    tweet_splitter(text),
    glue_collapse(c(
      tweet_collapse(c(text["caption"], "#twenty  #characters")),
      tweet_collapse("#again #more #characters"),
      tweet_collapse(text[c("exposure", "camera")])
    ),
    sep = "{{tweet break}}"
    )
  )
})
