library(shiny)
library(DT)
library(phototweetr)
library(RSQLite)

render_tweet <-

shinyServer(function(input, output, session) {
  con <- connect("../../phototweetr.sql")
  photos_full <- dbGetQuery(con, "SELECT rowid, * FROM tweets;")
  dbDisconnect(con)
  options(DT.options = list(pageLength = 15))

  photos <- photos_full[,c("orig_file", "tweet_text")]
  photos$tweet_text <- paste(substring(photos$tweet_text, 0, 25), "...")

  output$photo_df <- DT::renderDataTable(
    photos,
    selection = list(mode = "single", selected = c(1)),
    rownames = FALSE
    )
  output$photo_selected <- renderText(input$photo_df_rows_selected)

  # TODO: edit some?
  # TODO: width altering doesn't help too much here, because it's already wider
  # than the screen
  output$photo_df_full <- DT::renderDataTable(photos_full, rownames = FALSE)

  output$tweet_text <- renderText(gsub("\n", "<br />", photos_full[input$photo_df_rows_selected, "tweet_text"]))
  output$photo <- renderImage({
    photo_loc <- file.path("../..", photos_full[input$photo_df_rows_selected, "orig_file"])

    # if the selection hasn't been made yet, setup an empty character
    if (length(photo_loc) == 0) {
      photo_loc <- ""
    }

    list(
      src = photo_loc,
      width = 500,
      contentType = 'image/jpg'
    )
  }, deleteFile = FALSE)

  observeEvent(input$tweet_now, {
    tweet_immediately(input$photo_df_rows_selected, photos_full)
  })
})

tweet_immediately <- function(id, photo_df) {
  photo_to_tweet <- photo_df[id, ]

    message("Authenticating with Twitter")
  token <- auth_rtweet(set_renv = FALSE)

  message(glue::glue("Tweeting out photo {photo_to_tweet$orig_file}"))
  photo_to_tweet <- tweet_photo(photo_to_tweet, token = token)

  message("Updating the database")
  con <- connect("../../phototweetr.sql")
  update_one(photo_to_tweet, con)
  dbDisconnect(con)
}
