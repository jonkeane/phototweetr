#' Toot a photo out
#'
#' @param photo_df a dataframe with information about the photo to be tooted
#' @param path the path to the `phototweetr` directory (the one above "orig"
#' typically)
#' @param ... arguments passed to `rtoot::post_toot`
#'
#' @return the `photo_df`, updated with toot time, etc.
#' @export
toot_photo <- function(photo_df, path = NULL, ...) {
  message(glue("Attempting to toot photo {photo_df$tweet_file}"))
  if (!is.null(path)) {
    image_path <- file.path(path, photo_df$tweet_file)
  } else {
    image_path <- photo_df$tweet_file
  }

  toot_info <- unlist(photo_df[,c("title", "comment", "tags", "exposure")])

  toot_text <- toot_splitter(toot_info)
  toot_text <- toot_text[!is.na(toot_text)]
  image_alt <- photo_df$alt_text

  if (length(toot_text) == 1) {
    tooted_text <- toot_collapse(toot_text)
    toot_response <- rtoot::post_toot(status = tooted_text, media = image_path, ..., alt_text = image_alt)
  } else {
    text <- toot_collapse(toot_text[[1]])
    toot_response <- rtoot::post_toot(status = text, media = image_path, ..., alt_text = image_alt)
    reply_id <- httr::content(toot_response)$id

    # Iterate through each reply after the first
    for (toot in toot_text[-1]) {
      toot_response <- rtoot::post_toot(
        status = toot_collapse(toot),
        in_reply_to_id = reply_id,
        ...
      )
      reply_id <- httr::content(toot_response)$id
    }
    tooted_text <- paste0(lapply(toot_text, toot_collapse), collapse = "\\\\")
  }

  if (httr::status_code(toot_response) == 200L) {
    photo_df$tweeted <- TRUE
    photo_df$tweeted_text <- tooted_text
    photo_df$date_tweeted <- Sys.time()
  } else {
    photo_df$tweet_error <- httr::content(toot_response)
  }

  return(photo_df)
}

mastodon_max <- 500
toot_breaker <- "\\"

toot_splitter <- function(text, max = mastodon_max) {
  # TODO: how to count emojis as 2 characters, CJK as 2 characters, etc.
  # https://developer.twitter.com/en/docs/basics/counting-characters
  # currently this uses utf8::utf8_width, which is close, but over counts
  # combining emojis

  # if the text all together is within one toot, we're done. But ensure that
  # the order is title comment exposure tags
  if (check_length(text)) {
    return(list(text[c("title", "comment", "exposure", "tags")]))
  }

  # first, try to put the photo settings in the next toot
  # the settings should always be under one toot, so no need to check those
  if (check_length(text[c("title", "exposure", "tags")]) &
      check_length(text["comment"])) {
    return(list(text[c("title", "exposure", "tags")], text["comment"]))
  }

  # finally, start with the title tags and then comment, and split them in to
  # chunks that are under the limit the tags are second here so the first toot
  # has at least some tags
  chunks <- as.list(c(
    chunker(text[c("title", "tags")], max),
    chunker(text[c("comment")], max),
    text["exposure"]
  ))

  return(chunks)
}

#' Collapse toot text
#'
#' @param text text to flatten
#'
#' @return the text flattened with new lines
#' @export
toot_collapse <- function(text) {
  text <- unlist(text)
  text <- Filter(Negate(is.na), text)

  return(glue::glue_collapse(text, sep = "\n"))
}

check_length <- function(input, max = mastodon_max) {
  input <- toot_collapse(input)

  n_char <- sum(utf8::utf8_width(as.character(input)), na.rm = TRUE)

  return(n_char <= mastodon_max)
}
