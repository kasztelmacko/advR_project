library(shiny)
library(igraph)
library(R6)

# Source the GameClass.R file
source("GameClass.R")

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
      uiOutput("graph_and_heading"),
      verbatimTextOutput("optimalPaths")
    )
  ),
  fluidRow(
    column(12,
           align = "center",
           style = "margin-top: 20px;",
           div(style = "background-color: #f8f9fa; padding: 10px;",
               div(style = "display: inline-block; margin-right: 10px;", uiOutput("newGameButton")),
               div(style = "display: inline-block; margin-right: 10px;", uiOutput("performBackwardInductionButton")),
               div(style = "display: inline-block; margin-right: 10px;", actionButton("aboutButton", "About")),
               div(style = "display: inline-block;", actionButton("createdByButton", "Created By"))
           )
    )
  )
)

#############################################
#                                           #
#                 BACKEND                   #
#                                           #
#############################################
server <- function(input, output, session) {
  
  # init the class instances
  game_instance <- reactiveVal(NULL)
  game_vectors <- reactiveVal(NULL)
  error_message <- reactiveVal("")
  
  # render initial screen
  output$playerInputs <- renderUI({
    if (is.null(game_instance()) || !game_instance()$start_game_clicked) {
      tagList(
        textInput("Player1_Name", "Player 1 Name:", value = ""),
        textInput("Player2_Name", "Player 2 Name:", value = ""),
        uiOutput("errorMessage")
      )
    }
  })
  
  # render start game button
  output$startGameButton <- renderUI({
    if (is.null(game_instance()) || !game_instance()$start_game_clicked) {
      actionButton("startGame", "Create Game", style = "background-color: #f45b69; color: white;")
    }
  })
  
  # player names check for game start
  observeEvent(input$startGame, {
    if (is.null(input$Player1_Name) || input$Player1_Name == "" || 
        is.null(input$Player2_Name) || input$Player2_Name == "") {
      error_message("Player names cannot be empty. Please enter names for both players.")
    } else if (nchar(input$Player1_Name) > 16 || nchar(input$Player2_Name) > 16) {
      error_message("Player names cannot be longer than 16 characters. Please enter shorter names.")
    } else if (tolower(input$Player1_Name) == tolower(input$Player2_Name)) {
      error_message("Player names cannot be the same. Please enter different names for both players.")
    } else if (!grepl("^[a-zA-Z0-9]+$", input$Player1_Name) || !grepl("^[a-zA-Z0-9]+$", input$Player2_Name)) {
      error_message("Player names can only consist of letters and numbers. Please enter valid names for both players.")
    } else {
      error_message("")
      game_instance(Game$new(input$Player1_Name, input$Player2_Name, start_game_clicked = TRUE))
      game_vectors(GameVectors$new(input$Player1_Name, input$Player2_Name))
    }
  })
  
  # error message creator
  output$errorMessage <- renderUI({
    if (!is.null(error_message()) && error_message() != "") {
      div(style = "color: red;", error_message())
    }
  })
  
  # render Add Node UI elements
  observe({
    if (!is.null(game_instance()) && game_instance()$start_game_clicked) {
      output$addNodeInputs <- renderUI({
        tagList(
          actionButton("addNode", "Add Node", style = "background-color: #456990; color: white;"),
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
  
  # adding Nodes to the game
  observeEvent(input$addNode, {
    req(game_instance())
    existing_edges <- game_instance()$getEdges()
    if (length(existing_edges) > 0 && !input$rootNodeIndex %in% unlist(existing_edges)) {
      error_message("Please provide an existing Node Index")
    } else {
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
        req(game_instance())
        game_instance()$plotTree(color_type = 1)
      })
    }
  })
  
  # reset game after newGame is clicked
  observeEvent(input$newGame, {
    game_instance(NULL)
    game_vectors(NULL)
    updateTextInput(session, "Player1_Name", value = "")
    updateTextInput(session, "Player2_Name", value = "")
    output$addNodeInputs <- NULL  # Hide Add Node inputs
    output$network_plot <- NULL   # Hide network plot
    
    # Clear the error message when starting a new game
    error_message("")
  })
  
  # render newGame button
  output$newGameButton <- renderUI({
    if (!is.null(game_instance()) && game_instance()$start_game_clicked) {
      actionButton("newGame", "Restart Game", style = "background-color: #f45b69; color: white;")
    } else {
      NULL
    }
  })
  
  # render plot 
  output$graph_and_heading <- renderUI({
    tagList(
      plotOutput("network_plot")
    )
  })
  
  # render About button
  observeEvent(input$aboutButton, {
    showModal(modalDialog(
      title = "About",
      tags$iframe(src = "about.html", width = "100%", height = "600px", frameborder = 0),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # render CreatedBy button
  observeEvent(input$createdByButton, {
    showModal(modalDialog(
      title = "Created By",
      tags$iframe(src = "createdby.html", width = "100%", height = "600px", frameborder = 0),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # render calculate best strategy
  output$performBackwardInductionButton <- renderUI({
    req(game_instance())
    if (!is.null(game_instance()) && game_instance()$start_game_clicked) {
      actionButton("performBackwardsInductionBtn", "Perform Backward Induction", style = "background-color: #456990; color: white;")
    } else {
      NULL
    }
  })
  
  # show best strategy results
  observeEvent(input$performBackwardsInductionBtn, {
    req(game_instance())
    if (game_instance()$hasTerminalNode()) {
      optimal_paths <- game_instance()$performBackwardInduction()
      
      showModal(modalDialog(
        title = "Best Strategy Results",
        tabsetPanel(
          tabPanel(
            paste(game_instance()$player1_name, "strategy"),
            tags$div(
              class = "container-fluid",
              tags$div(
                class = "row",
                tags$div(
                  class = "col-sm-6 text-center",
                  style = "background-color: #f8f9fa;",
                  tags$h3("Best path:")
                ),
                tags$div(
                  class = "col-sm-6 text-center",
                  style = "background-color: #f8f9fa;",
                  tags$h3("Max payoff:")
                )
              ),
              tags$div(
                class = "row",
                tags$div(
                  class = "col-sm-6 text-center fs-1",
                  style = "background-color: #f8f9fa; ; color: #f45b69;",
                  tags$h2(paste(optimal_paths$best_path_player1, collapse = " -> "))
                ),
                tags$div(
                  class = "col-sm-6 text-center fs-1",
                  style = "background-color: #f8f9fa; color: #f45b69;",
                  tags$h2(optimal_paths$max_payoff_player1)
                )
              ),
              plotOutput("player1_plot")
            )
          ),
          
          tabPanel(
            paste(game_instance()$player2_name, "strategy"),
            tags$div(
              class = "container-fluid",
              tags$div(
                class = "row",
                tags$div(
                  class = "col-sm-6 text-center",
                  style = "background-color: #f8f9fa;",
                  tags$h3("Best path:")
                ),
                tags$div(
                  class = "col-sm-6 text-center",
                  style = "background-color: #f8f9fa;",
                  tags$h3("Max payoff:")
                )
              ),
              tags$div(
                class = "row",
                tags$div(
                  class = "col-sm-6 text-center fs-1",
                  style = "background-color: #f8f9fa; ; color: #f45b69;",
                  tags$h2(paste(optimal_paths$best_path_player2, collapse = " -> "))
                ),
                tags$div(
                  class = "col-sm-6 text-center fs-1",
                  style = "background-color: #f8f9fa; ; color: #f45b69;",
                  tags$h2(optimal_paths$max_payoff_player2)
                )
              ),
              plotOutput("player2_plot")
            )
          )
        ),
        easyClose = TRUE,
        footer = NULL
      ))
      
      output$player1_plot <- renderPlot({
        req(game_instance())
        game_instance()$plotTree(color_type = 2, best_nodes_player = optimal_paths$best_nodes_player1)
      })
      
      output$player2_plot <- renderPlot({
        req(game_instance())
        game_instance()$plotTree(color_type = 3, best_nodes_player = optimal_paths$best_nodes_player2)
      })
    } else {
      showModal(modalDialog(
        title = "Error",
        "Add a tree with last nodes to perform this operation.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
}

shinyApp(ui = ui, server = server)
