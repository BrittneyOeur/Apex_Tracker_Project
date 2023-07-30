library(shiny)
library(stringr)
library(ggplot2)
library(plotly)
library(shinyWidgets)

source("apex_tracking.r")

# WORKS ON THE UI OF THE PAGE
intro_page <- tabPanel("Active",
  fluidPage(
    tags$head(
      includeCSS("www/apexTrackerDesign.css")
    ),
    
    setBackgroundImage(
      src = "wraith_background_op20.png"
    ),

    div(
      id = "title",
      h1("Current Active Legend Stats")
    ),

    div(
      id = "panel",
      selectInput(
        inputId = "platforms", 
        label = "Choose platform:",
        choices = c("Origin", "XBOX", "Playstation"),
        selected = NULL
      ),
      
      textInput(
        inputId = "playerName", 
        label = "Enter username:"
      ),
    ),
    
    tags$div(
      class = "text-box",
      htmlOutput(outputId = "pc_playerOutput")
    ),
  )
)

lifetime_page <- tabPanel("Lifetime",
  fluidPage(
    tags$head(
      includeCSS("www/apexTrackerDesign.css")
    ),
    
    div(
      class = "life_title",
      h1("Lifetime Stats")
    ),
    
    #div(
    #  id = "panel_2",
    #  selectInput(
    #    inputId = "platforms_2", 
    #    label = "Choose platform:",
    #    choices = c("Origin", "XBOX", "Playstation"),
    #    selected = NULL
    #  ),
    #  textInput(
    #    inputId = "playerName_2", 
    #    label = "Enter username:"
    #  ),
    #),
    
    fluidRow(align = "center",
        div(id = "main_2",
            plotlyOutput(
              outputId = "pc_playerOutput_2",
              width = 550, 
              height = 550))
    ),
  )
)

ui <- navbarPage(
  theme = "custom-theme",
  title = "Apex Legends Tracker",
  intro_page,
  lifetime_page
)

server <- function(input, output) {
  # RENDERS STATS FOR ORIGIN PLAYERS:
  output$pc_playerOutput <- renderUI({
    text <- input$playerName
    option <- input$platforms
    
    if (is.null(text) || text == "" || is.na(text)) {
      return("")
    } else {
      result <- NULL
      if (option == "Origin") {
        result <- get_user("PC", input$playerName)
      } else if (option == "XBOX") {
        result <- get_user("X1", input$playerName)
      } else if (option == "Playstation") {
        result <- get_user("PS4", input$playerName)
      } else {
        result <- NULL
      }
    }
    return(result)
  })
  
  # RENDERS GRAPH
  output$pc_playerOutput_2 <- renderPlotly({
    placeholder_df <- data.frame(
      legends = c("Horizon", "Ballisitic", "Wraith"),
      kills = c(5, 10, 15)
    )
    
    lifetime_graph <- ggplot(data = placeholder_df, 
                             aes(x = reorder(legends, kills), 
                                 y = kills,
                                 fill = legends)) +
      geom_bar(stat = "identity") +
      labs(x = "Legends",
           y = "Kills") +
      theme_minimal() +
      theme(panel.background = element_rect("#f2ede1", 
                                            "#b8b8b8"),
            plot.background = element_rect("transparent"),
            text = element_text(family = "segoe ui", size = 15),
            axis.title.x = element_text(face = "italic", vjust = -0.75),
            axis.title.y = element_text(face = "italic", vjust = +3),
            legend.position = "none") +
      scale_fill_manual(values = c("#993636", 
                                   "#9c5a5a", 
                                   "#961414"))

    return(lifetime_graph)
  })
}

shinyApp(ui, server)