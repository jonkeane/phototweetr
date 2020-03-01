sanitize_exif <- function(path, exif_data) {
  if (missing(exif_data)) {
    exif_data <- exifr::read_exif(path)
  }

  args <- c(
    "-overwrite_original",
    "-charset iptc=UTF8",
    "-all=",
    "-tagsFromFile @ "
  )
  args <- c(args, paste0("-", tags_to_keep))

  return(exifr::exiftool_call(args = args, fnames = path, quiet = TRUE))
}

tags_to_keep <- list(
  "ImageDescription",
  "Make",
  "Model",
  "Software",
  "ModifyDate",
  "ExposureTime",
  "FNumber",
  "ExposureProgram",
  "ISO",
  "RecommendedExposureIndex",
  "ExifVersion",
  "DateTimeOriginal",
  "CreateDate",
  "TimeCreated",
  "ApertureValue",
  "FocalLength",
  "ExposureTime",
  "LensModel",
  "Lens",
  "ObjectName",
  "Keywords",
  "By-line",
  "Caption-Abstract",
  "Description",
  "Subject",
  "Artist",
  "OwnerName",
  "Creator",
  "Title",
  "Copyright",
  "CopyrightNotice",
  "CopyrightFlag",
  "URL",
  "UsageTerms"
)
