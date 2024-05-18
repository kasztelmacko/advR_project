library(shiny)
library(igraph)
library(R6)
source("GameClass.R")

# init the game instance
game_instance <- reactiveVal(NULL)
game_vectors <- reactiveVal(NULL)

#############################################
#                                           #
#                 FRONTEND                  #
#                                           #
#############################################
ui <- fluidPage(
  titlePanel("Strategies in Game Theory"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("playerInputs"),
      uiOutput("startGameButton"),
      uiOutput("addNodeInputs")
    ),
    mainPanel(
      uiOutput("graph_and_heading")
    )
  ),
  fluidRow(
    column(6,
           align = "left",
           uiOutput("newGameButton")
    )
  )
)


#############################################
#                                           #
#                 BACKEND                   #
#                                           #
#############################################
server <- function(input, output, session) {
  
  game_instance <- reactiveVal(NULL)
  
  output$playerInputs <- renderUI({
    if (is.null(game_instance()) || !game_instance()$start_game_clicked) {
      tagList(
        textInput("Player1_Name", "Player 1 Name:", value = ""),
        textInput("Player2_Name", "Player 2 Name:", value = "")
      )
    }
  })
  
  output$startGameButton <- renderUI({
    if (is.null(game_instance()) || !game_instance()$start_game_clicked) {
      actionButton("startGame", "Create Game")
    }
  })
  
  observeEvent(input$startGame, {
    if (!is.null(input$Player1_Name) && !is.null(input$Player2_Name)) {
      game_instance(Game$new(input$Player1_Name, input$Player2_Name, start_game_clicked = TRUE))
      game_vectors(GameVectors$new(input$Player1_Name, input$Player2_Name))
    }
  })
  
  
  observeEvent(input$startGame, {
    if (game_instance()$start_game_clicked) {
      output$addNodeInputs <- renderUI({
        tagList(
          actionButton("addNode", "Add Node"),
          checkboxInput("lastNodeToggle", "Last Node", value = FALSE),
          textInput("rootNodeIndex", "Root Node Index:", value = ""),
          textInput("decision", "Decision:", value = ""),
          conditionalPanel(
            condition = "input.lastNodeToggle == true",
            numericInput("points_Player1", paste("Points for", game_instance()$player1_name, ":"), value = 0),
            numericInput("points_Player2", paste("Points for", game_instance()$player2_name, ":"), value = 0)
          )
        )
      })
    } else {
      output$addNodeInputs <- NULL
    }
  })
  
  observeEvent(input$addNode, {
    game_instance()$addDecisionAndEdge(
      input$rootNodeIndex, 
      input$decision, 
      input$lastNodeToggle, 
      input$points_Player1, 
      input$points_Player2
    )
    
    game_vectors()$addDecisionAndEdge_Vector( 
      input$rootNodeIndex, 
      input$decision, 
      input$lastNodeToggle, 
      input$points_Player1, 
      input$points_Player2
    )
    
    updateTextInput(session, "rootNodeIndex", value = "")
    updateTextInput(session, "decision", value = "")
    updateCheckboxInput(session, "lastNodeToggle", value = FALSE)
    if (input$lastNodeToggle) {
      updateNumericInput(session, "points_Player1", value = 0)
      updateNumericInput(session, "points_Player2", value = 0)
    }
    
    # render plot
    output$network_plot <- renderPlot({
      game_instance()$plotTree() 
    })
  })
  
  observeEvent(input$newGame, {
    game_instance(NULL)
    game_vectors(NULL)
    updateTextInput(session, "Player1_Name", value = "")
    updateTextInput(session, "Player2_Name", value = "")
    output$addNodeInputs <- NULL  # Hide Add Node inputs
  })
  
  output$newGameButton <- renderUI({
    if (!is.null(game_instance()) && game_instance()$start_game_clicked) {
      actionButton("newGame", "New Game")
    } else {
      NULL
    }
  })
  
  output$graph_and_heading <- renderUI({
    tagList(
      plotOutput("network_plot")
    )
  })
}

shinyApp(ui = ui, server = server)
