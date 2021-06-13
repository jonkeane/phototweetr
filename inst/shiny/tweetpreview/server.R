library(shiny)
library(DT)
library(phototweetr)
library(RSQLite)

process_photos_df <- function(photo_df) {
  # mark titles as untitled
  photo_df[is.na(photo_df$title),]$title <- "untitled"
  photo_df[is.na(photo_df$comment),]$comment <- ""

  text_preview <- paste(
    photo_df$title,
    photo_df$comment,
    photo_df$exposure,
    photo_df$tags
  )

  photo_df$tweet_text_preview <- text_preview
  photo_df[photo_df$tweeted == 1, ]$tweet_text_preview <- photo_df[photo_df$tweeted == 1, ]$tweeted_text
  photo_df$tweet_text_preview <- paste(substring(photo_df$tweet_text_preview, 0, 25), "...")

  photo_df$orig_file <- gsub("orig/", "", photo_df$orig_file)
  photo_df[photo_df$tweeted == 1, ]$tweeted <- "\U002714"
  photo_df[photo_df$tweeted == 0, ]$tweeted <- ""
  photo_df$alt <- ""
  photo_df[is.na(photo_df$alt_text), ]$alt <- "\U002718"
  photo_df[!is.na(photo_df$alt_text), ]$alt <- "\U002714"

  return(photo_df[,c("orig_file", "tweet_text_preview", "tweeted", "alt")])
}

shinyServer(function(input, output, session) {
  options(DT.options = list(pageLength = 15))
  # TODO: make reactive?
  con <- connect(file.path(calling_dir, "phototweetr.sql"))
  photos_full <- dbGetQuery(con, "SELECT rowid, * FROM tweets;")
  dbDisconnect(con)

  output$photo_df <- DT::renderDataTable(
    process_photos_df(photos_full),
    selection = list(mode = "single", selected = 1),
    rownames = FALSE
  )
  output$photo_selected <- renderText(input$photo_df_rows_selected)

  # TODO: edit some?
  # TODO: width altering doesn't help too much here, because it's already wider
  # than the screen
  output$photo_df_full <- DT::renderDataTable(photos_full, rownames = FALSE)

  text_format_func <- function(i) {
    if (photos_full[i, "tweeted"] == 1) {
      return(photos_full[i, "tweeted_text"])
    } else {
      title <- photos_full[i, "title"]
      if (is.na(title)) title <- "untitled"
      comment <- photos_full[i, "comment"]
      exposure <- photos_full[i, "exposure"]
      tags <- photos_full[i, "tags"]

      # don't display any that are NA
      contents <- c(title, comment, exposure, tags)
      preview <- paste0(contents[!is.na(contents)], collapse = "<br />")
      return(preview)
    }
  }

  output$tweet_text <- renderText(text_format_func(input$photo_df_rows_selected))
  output$photo <- renderImage({
    photo_loc <- file.path(calling_dir, photos_full[input$photo_df_rows_selected, "orig_file"])

    # if the selection hasn't been made yet, setup an empty character
    if (length(photo_loc) == 0) {
      photo_loc <- ""
    }

    return(list(
      src = photo_loc,
      width = 500,
      title = photos_full[input$photo_df_rows_selected, "alt_text"] %||% "no text",
      alt = photos_full[input$photo_df_rows_selected, "alt_text"] %||% "no text"
    ))
  }, deleteFile = FALSE)
  output$alt_text <- renderText(paste0("<b>Alt text:</b> ", photos_full[input$photo_df_rows_selected, "alt_text"]))


  observeEvent(input$tweet_button, {
    tweet_modal(input$photo_df_rows_selected, photos_full)
  })
  # this might be better inside the modal, but for now is here
  observeEvent(input$tweet_now, {
    tweet_immediately(input$photo_df_rows_selected, photos_full)
  })

  observeEvent(input$show_image, {
    photo_modal(input$photo_df_rows_selected, photos_full)
  })

})

tweet_modal <- function(id, photo_df) {
  photo_file <- file.path(calling_dir, photo_df[id, "orig_file"])
  showModal(
    div(modalDialog(
      title = "Are you sure you want to tweet this?",
      renderImage({
        list(
          src = photo_file,
          "width" = "auto",
          "height" = "auto"
        )
      }, deleteFile = FALSE), easyClose = TRUE,
      footer = tagList(
        modalButton("no"),
        actionButton("tweet_now", "yes")
      )), id = "photo_modal")
  )
}

tweet_immediately <- function(id, photo_df) {
  photo_to_tweet <- photo_df[id, ]

  if (photo_to_tweet$tweeted == 1) {
    showModal(modalDialog(
      h2("\U00274C This photo was already tweeted \U00274C"),
      easyClose = TRUE
    ))
    return(NULL)
  }

  message("Authenticating with Twitter")
  token <- auth_rtweet()

  message(glue::glue("Tweeting out photo {photo_to_tweet$orig_file}"))
  if (tolower(Sys.getenv("phototweetrtweet", "")) == "true") {
    # if the env var is set, then actually tweet
    message(glue::glue("Tweeting out photo {photo_to_tweet$orig_file}"))
    photo_to_tweet <- tweet_photo(photo_to_tweet, path = calling_dir, token = token)
  } else {
    message(glue::glue("Pretending to tweet out photo {photo_to_tweet$orig_file}"))
    photo_to_tweet$tweeted <- 1L
    photo_to_tweet$date_tweeted <- Sys.time()
  }

  message("Updating the database")
  con <- connect(file.path(calling_dir, "phototweetr.sql"))
  update_one(photo_to_tweet, con)
  dbDisconnect(con)

  showModal(modalDialog(h2("\U002714 The photo was tweeted \U002714"), easyClose = TRUE))
}

photo_modal <- function(id, photo_df) {
  photo_file <- file.path(calling_dir, photo_df[id, "orig_file"])
  showModal(div(modalDialog(renderImage({
    list(
      src = photo_file,
      "width" = "auto",
      "height" = "auto"
    )
  }, deleteFile = FALSE), easyClose = TRUE), id = "photo_modal"))
}
