grab_exif_list <- function(exif_data) {
  # convert to a data.frame to avoid tibble's unknown or uninitialized columns
  # warnings
  exif_data <- as.data.frame(exif_data)

  text <- list(
    title = exif_data$Title,
    comment = clean_comment(exif_data),
    image_description = exif_data$ImageDescription,
    exposure_camera = paste0(
      "\U1f4f8", exposure(exif_data), "\n",
      "\U1f4f7", camera(exif_data)
    ),
    tags = tags(exif_data)
  )

  return(text)
}

exposure <- function(exif_data) {
  # shutter, aperture, iso, focal length
  shutter <- paste0(MASS::fractions(exif_data$ShutterSpeed,),"s")
  aperture <- paste0("f/", exif_data$Aperture)
  iso <- paste0(exif_data$ISO, "iso")
  focal_length <- paste0(exif_data$FocalLength, "mm")

  return(glue_collapse(c(focal_length, paste(shutter, aperture, iso)), sep = " \u2022 "))
}

camera <- function(exif_data) {
  # Lens or Lensmodel?
  return(paste(exif_data$Model, exif_data$Lens))
}

tags <- function(exif_data) {
  # what about subject?
  tags <- gsub(" ", "", exif_data$Keywords[[1]])
  return(glue_collapse(paste0("#", tags), sep = " "))
}

clean_comment <- function(exif_data, exclude_vers = TRUE) {
  # Necessary for tags created when using the lens tagger plugin for lightroom.
  # It cleans out extra arguments that are added to comment by LensTagger as
  # well as the errant version information — https://www.lenstagger.com
  if (is.null(exif_data$UserComment)) {
    return(NULL)
  }
  comments <- strsplit(exif_data$UserComment, "\n")[[1]]
  clean <- function(string, version = exclude_vers) {
    errant_tag <- grepl("^-Make=|^-Model=|^-ISO=", string, ignore.case = TRUE)
    errant_version <- grepl("^LensTaggerVer:", string, ignore.case = TRUE)
    if (version) {
      return(!(errant_tag | errant_version))
    } else {
      return(!errant_tag)
    }
  }
  return(paste0(Filter(clean, comments), collapse = "\n"))
}

# for geo info:
# https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/post-statuses-update
