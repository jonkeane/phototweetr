library(shiny)

navbarPage(
  title = 'phototweetr',

  tabPanel(
    'Tweet preview',
    fluidRow(
      column(
        6, hr(),
        htmlOutput('tweet_text'),
        imageOutput('photo')
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

