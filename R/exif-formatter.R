#' Format EXIF for tweet
#'
#' Takes the exif data (from `exifr::read_exif()`) and formats it into a tweet.
#'
#' @param exif_data EXIF data from `exifr::read_exif()`
#'
#' @return text for a tweet
#' @export
format_exif <- function(exif_data) {
    text <- c(
        text(exif_data),
        paste0("\U1f4f8", exposure(exif_data)),
        paste0("\U1f4f7", camera(exif_data)),
        tags(exif_data)
    )

    # TODO: What to do if over twitter_max?

    return(paste(text, collapse = "\n"))
}

twitter_max <- 280

text <- function(exif_data) {
    # Title, Description
    return(paste(exif_data$Title, exif_data$Description, sep = " \u2022 "))
}

exposure <- function(exif_data) {
    # shutter, aperture, iso, focal length
    shutter <- paste0(MASS::fractions(exif_data$ShutterSpeed,),"s")
    aperture <- paste0("f/", exif_data$Aperture)
    iso <- paste0(exif_data$ISO, "iso")
    focal_length <- paste0(exif_data$FocalLength, "mm")

    return(paste(focal_length, paste(shutter, aperture, iso), sep = " \u2022 "))
}

camera <- function(exif_data) {
    # Lens or Lensmodel?
    return(paste(exif_data$Model, exif_data$Lens))
}

tags <- function(exif_data) {
    # what about subject?
    tags <- gsub(" ", "", exif_data$Keywords[[1]])
    return(paste(paste0("#", tags), collapse = " "))
}

# for geo info:
# https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/post-statuses-update
