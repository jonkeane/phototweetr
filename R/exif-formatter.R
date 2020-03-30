#' Format EXIF for tweet
#'
#' Takes the EXIF data (from `exifr::read_exif()`) and formats it into a tweet.
#'
#' @param exif_data EXIF data from `exifr::read_exif()`
#'
#' @return text for a tweet
#' @export
format_exif <- function(exif_data) {
  text <- grab_exif_list(exif_data)

  return(tweet_splitter(text))
}

twitter_max <- 280
tweet_breaker <- "{{tweet break}}"

grab_exif_list <- function(exif_data) {
  # convert to a data.frame to avoid tibble's unknown or uninitialized columns
  # warnings
  exif_data <- as.data.frame(exif_data)

  text <- c(
    caption = text(exif_data),
    exposure = paste0("\U1f4f8", exposure(exif_data)),
    camera = paste0("\U1f4f7", camera(exif_data)),
    tags = tags(exif_data)
  )

  return(text)
}

tweet_splitter <- function(text) {
  # TODO: how to count emojis as 2 charactrs, CJK as 2 characters, etc.
  # https://developer.twitter.com/en/docs/basics/counting-characters
  # currently this uses utf8::utf8_width, which is close, but overcounts
  # combining emojis

  # if the text all together is within one tweet, we're done
  if (utf8::utf8_width(tweet_collapse(text)) <= twitter_max) {
    return(tweet_collapse(text))
  }

  # first, try to put the photo settings in the next tweet
  if (utf8::utf8_width(tweet_collapse(text[c("caption", "tags")])) <= twitter_max) {
    return(tweet_collapse(text[c("caption", "tags")], text[c("exposure", "camera")]))
  }

  # finally, start with the caption and tags, and split them in to chunks that
  # are under the limit
  chunks <- tweet_collapse(text[c("caption", "tags")])
  chunks <- strsplit(chunks, " ")[[1]]
  chunk_lengths <- vapply(chunks, utf8::utf8_width, integer(1)) + 1
  start <- 1
  chunked_chunks <- list()
  for (i in seq_along(chunk_lengths)) {
    end <- i
    if (sum(chunk_lengths[start:end], chunk_lengths[end+1], na.rm = TRUE) > twitter_max) {

      chunked_chunks[length(chunked_chunks) + 1] <- list(chunks[start:end])
      start <- end + 1
    }

    # clean up at the end
    if (end == length(chunk_lengths)) {
      chunked_chunks[length(chunked_chunks) + 1] <- list(chunks[start:end])
    }
  }

  chunks <- lapply(chunked_chunks, paste, collapse = " ")

  # now add on the last chunk IFF it can fit, if not just put it in another tweet
  last_chunk <- text[c("exposure", "camera")]

  if (utf8::utf8_width(tweet_collapse(chunks[-1], last_chunk)) < twitter_max) {
    chunks[[-1]] <- c(chunks[[-1]], last_chunk)
  } else {
    chunks <- c(chunks, tweet_collapse(last_chunk))
  }

  return(do.call(tweet_collapse, chunks))
}

tweet_collapse <- function(..., sep = tweet_breaker) {
  text <- list(...)

  # collapse each element
  text_out <- lapply(text, glue::glue_collapse, sep = "\n")

  # now collapse with the seperator
  text_out <- glue::glue_collapse(text_out,  sep = sep)

  return(text_out)
}

text <- function(exif_data) {
  # Title, Description
  return(glue_collapse(c(exif_data$Title, exif_data$Description), sep = " \u2022 "))
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

# for geo info:
# https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/post-statuses-update
