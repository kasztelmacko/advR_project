library(shiny)
library(igraph)
library(R6)

# Source the GameClass.R file
source("GameClass.R")

# Initialize the game instance
game_instance <- reactiveVal(NULL)
game_vectors <- reactiveVal(NULL)
error_message <- reactiveVal("")

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
      uiOutput("addNodeInputs"),
      uiOutput("addNodeButton"),
      uiOutput("performBackwardInductionButton")
    ),
    mainPanel(
      plotOutput("treePlot"),
      verbatimTextOutput("errorMessage"),
      verbatimTextOutput("optimalPaths")
    )
  )
)

#############################################
#                                           #
#               SERVER LOGIC                #
#                                           #
#############################################
server <- function(input, output, session) {
  output$playerInputs <- renderUI({
    tagList(
      textInput("player1", "Player 1 Name:", value = "Player 1"),
      textInput("player2", "Player 2 Name:", value = "Player 2")
    )
  })
  
  output$startGameButton <- renderUI({
    actionButton("startGameBtn", "Start Game")
  })
  
  observeEvent(input$startGameBtn, {
    game_instance(Game$new(input$player1, input$player2))
    game_vectors(GameVectors$new(input$player1, input$player2))
  })
  
  output$addNodeInputs <- renderUI({
    req(game_instance())
    tagList(
      textInput("rootNode", "Root Node:", value = "A"),
      textInput("decision", "Decision:", value = ""),
      checkboxInput("isLastNode", "Is Last Node?", value = FALSE),
      numericInput("pointsPlayer1", "Points for Player 1:", value = 0),
      numericInput("pointsPlayer2", "Points for Player 2:", value = 0)
    )
  })
  
  output$addNodeButton <- renderUI({
    req(game_instance())
    actionButton("addNodeBtn", "Add Node")
  })
  
  observeEvent(input$addNodeBtn, {
    req(game_instance())
    root_node <- input$rootNode
    decision <- input$decision
    is_last_node <- input$isLastNode
    points_player1 <- input$pointsPlayer1
    points_player2 <- input$pointsPlayer2
    
    game_instance()$addDecisionAndEdge(root_node, decision, is_last_node, points_player1, points_player2)
    game_vectors()$addDecisionAndEdge_Vector(root_node, decision, is_last_node, points_player1, points_player2)
    
    output$treePlot <- renderPlot({
      game_instance()$plotTree()
    })
  })
  
  output$performBackwardInductionButton <- renderUI({
    req(game_instance())
    actionButton("performBackwardsInductionBtn", "Perform Backward Induction")
  })
  
  observeEvent(input$performBackwardsInductionBtn, {
    req(game_instance())
    optimal_paths <- game_instance()$performBackwardInduction()
    
    output$optimalPaths <- renderText({
      paste("Best path for", game_instance()$player1_name, ":", paste(optimal_paths$best_path_player1, collapse = " -> "), "\n",
            "Max payoff for", game_instance()$player1_name, ":", optimal_paths$max_payoff_player1, "\n",
            "Best path for", game_instance()$player2_name, ":", paste(optimal_paths$best_path_player2, collapse = " -> "), "\n",
            "Max payoff for", game_instance()$player2_name, ":", optimal_paths$max_payoff_player2)
    })
  })
  
  output$errorMessage <- renderText({
    req(error_message())
    error_message()
  })
}

shinyApp(ui = ui, server = server)
