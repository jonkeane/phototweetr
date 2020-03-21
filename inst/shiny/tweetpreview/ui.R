library(shiny)

navbarPage(
  title = 'phototweetr',

  tabPanel(
    'Tweet preview',
    tags$head(tags$style(
      type="text/css",
      "#photo { position: relative; overflow: hidden; border-radius: 15px;",
      "maring-top: 10px; margin-bottom: 10px;}",
      "#photo img {position: absolute; left: 50%; top: 50%;",
      "-webkit-transform: translate(-50%,-50%);",
      "-ms-transform: translate(-50%,-50%);",
      "transform: translate(-50%,-50%);}",
      "#photo_modal img {max-width: 100%; max-height: 100%;}"
    )),
    fluidRow(
      column(
        6, hr(),
        htmlOutput('tweet_text'),
        imageOutput('photo', height = "285px", width = "500px"),
        actionButton("tweet_button", "tweet immediately", width = "500px"),
        br(),
        br(),
        actionButton("show_image", "show full image", width = "500px")
      ),
      column(
        6,
        DT::dataTableOutput('photo_df')
      )
    )
  ),

  tabPanel(
    'Photos',
    fluidRow(
      column(
        12, h2('Available photos'), hr(),
        DT::dataTableOutput('photo_df_full')
      )
    )
  )
)

