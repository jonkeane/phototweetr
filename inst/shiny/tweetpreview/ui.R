library(shiny)

navbarPage(
  title = 'phototweetr',

  tabPanel(
    'Tweet preview',
    tags$head(tags$style(
      type="text/css",
      "#photo {overflow: hidden}",
    )),
    fluidRow(
      column(
        6, hr(),
        htmlOutput('tweet_text'),
        imageOutput('photo', height = "500px"),
        fluidRow(
          align="bottom",
          actionButton("tweet_now", "tweet immediately", width = "100%")
        )
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

