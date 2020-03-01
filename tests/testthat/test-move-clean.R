new_image <- test_path("cleaned.jpg")
file.copy(test_path("IMG_4907.jpg"), new_image, overwrite = TRUE)

test_that("cleaning works", {
    expect_silent(sanitize_exif(new_image))
    new_exif <- exifr::read_exif(new_image)
    # check that a few of the more sensitive or extraneous EXIF tags are gone
    expect_false(
        any(mapply(
            grepl,
            c(
                "SerialNumber",
                "GPSLatitude",
                "GPSLongitude",
                "Tint",
                "Saturation",
                "Sharpness"
            ),
            colnames(new_exif),
            MoreArgs = list(ignore.case = TRUE)
        ))
    )

    # and we keep all the tags we expect
    expect_true(all(tags_to_keep %in%  colnames(new_exif)))
})

unlink(new_image)
