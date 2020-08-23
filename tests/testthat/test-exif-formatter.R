exif_data <- exifr::read_exif(test_path("orig", "IMG_4907.jpg"))

test_that("formatting", {
  skip_if(Sys.info()['sysname'] == "Windows", "Unicode on windows \U1F643")
  expect_equal(
    grab_exif_list(exif_data),
    list(
      title = "Fuji from Hakone II",
      comment = "Film Make: Ilford\nFilm Type: Pan F+",
      image_description = paste0(
        "A photo of mount Fuji. Snow covers the top ",
        "1/3 of the mountain with trails visible zig-zagging up."
      ),
      exposure_camera = "\U0001f4f8105mm • 1/500s f/9.5 100iso\n\U0001f4f7Canon EOS 6D EF24-105mm f/4L IS USM",
      tags = glue_collapse("#Fuji-san #Hakone #Japan #MountFuji #Mountain #富士山 #日本 #箱根")
    )
  )
})

test_that("comment parsing", {
  exif_data <- data.frame(
    UserComment = "This is a caption\n-Make=fdasf\n-Model=fdas\n-ISO=50\nFilm Make: Ilford\nFilm Type: Pan F+\nLensTaggerVer:1.7.6",
    stringsAsFactors = FALSE
  )

  expect_equal(
    clean_comment(exif_data),
    "This is a caption\nFilm Make: Ilford\nFilm Type: Pan F+"
  )

  expect_equal(
    clean_comment(exif_data, exclude_vers = FALSE),
    "This is a caption\nFilm Make: Ilford\nFilm Type: Pan F+\nLensTaggerVer:1.7.6"
  )

  expect_null(clean_comment(data.frame()))
})
