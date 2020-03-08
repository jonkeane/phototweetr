exif_data <- exifr::read_exif(test_path("orig", "IMG_4907.jpg"))

test_that("formatting", {
  skip_if(Sys.info()['sysname'] == "Windows", "Unicode on windows \U1F643")
  expect_identical(
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
