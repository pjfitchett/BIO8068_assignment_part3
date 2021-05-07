# Shiny app

library(shiny)

# Define UI ----

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textOutput("panel")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Overview" 
                 ),
        tabPanel("Adder"
                 ),
        tabPanel("Common Lizard"
                 ),
        tabPanel("Grass Snake"
                 ),
        tabPanel("Slow Worm")
      )
    )
  )
)

# Define server logic ----

server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
}

# Run the app ----

shinyApp(ui = ui, server = server)
