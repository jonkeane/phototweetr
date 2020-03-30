exif_data <- exifr::read_exif(test_path("orig", "IMG_4907.jpg"))

test_that("formatting", {
  skip_if(Sys.info()['sysname'] == "Windows", "Unicode on windows \U1F643")
  expect_equal(
    grab_exif_list(exif_data),
    list(
      caption = glue_collapse(c(
        "Fuji from Hakone II • A photo of mount Fuji. Snow covers the top ",
        "1/3 of the mountain with trails visible zig-zagging up."
      )),
      exposure_camera = "\U0001f4f8105mm • 1/500s f/9.5 100iso\n\U0001f4f7Canon EOS 6D EF24-105mm f/4L IS USM",
      tags = glue_collapse("#Fuji-san #Hakone #Japan #MountFuji #Mountain #富士山 #日本 #箱根")
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
