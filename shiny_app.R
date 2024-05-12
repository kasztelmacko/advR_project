if (!require("pacman")) install.packages("pacman")
pacman::p_load(rattle, R6, rpart.plot, rpart, shiny)

# Define the VisualizeGame R6 class
VisualizeGame <- R6Class("VisualizeGame",
                         public = list(
                           player_1 = NA_character_,
                           player_2 = NA_character_,
                           decision_player_1 = numeric(),
                           decision_player_2 = numeric(),
                           earn_player_1 = numeric(),
                           earn_player_2 = numeric(),
                           initialize = function(player_1, player_2, decision_player_1, decision_player_2, earn_player_1, earn_player_2) {
                             self$player_1 <- player_1
                             self$player_2 <- player_2
                             self$decision_player_1 <- decision_player_1
                             self$decision_player_2 <- decision_player_2
                             self$earn_player_1 <- earn_player_1
                             self$earn_player_2 <- earn_player_2
                           },
                           Create_DataFrame = function() {
                             data.frame(
                               Player_1 = self$player_1,
                               Player_2 = self$player_2,
                               Decision_Player_1 = self$decision_player_1,
                               Decision_Player_2 = self$decision_player_2,
                               Earn_Player_1 = self$earn_player_1,
                               Earn_Player_2 = self$earn_player_2
                             )
                           },
                           Plot_Game_Tree = function(formula, data) {
                             mytree <- rpart(
                               formula,
                               data = data,
                               method = "class",
                               minsplit = 2,
                               minbucket = 1,
                               cp = -1
                             )
                             fancyRpartPlot(mytree, caption = NULL)
                           },
                           Create_Game_Table = function(player_1_name, player_2_name) {
                             setNames(
                               data.frame(
                                 self$decision_player_1,
                                 self$decision_player_2,
                                 self$earn_player_1,
                                 self$earn_player_2
                               ),
                               c(paste("Decyzja", player_1_name), paste("Decyzja", player_2_name), paste("Zysk", player_1_name), paste("Zysk", player_2_name))
                             )
                           }
                         )
)

# Define UI
ui <- fluidPage(
  titlePanel("Game Visualization"),
  sidebarLayout(
    sidebarPanel(
      textInput("player1", "Player 1:", ""),
      textAreaInput("player1_earn", "Zysk Player 1:", ""),
      textInput("player2", "Player 2:", ""),
      textAreaInput("player2_earn", "Zysk Player 2:", ""),
      actionButton("visualize", "Visualize")
    ),
    mainPanel(
      conditionalPanel(
        condition = "output.tree || output.datatable",
        h1("Extensive Representation")
      ),
      plotOutput("tree"),
      conditionalPanel(
        condition = "output.tree || output.datatable",
        h1("Strategic Representation")
      ),
      tableOutput("datatable")
    )
  )
)


# Define server logic
server <- function(input, output) {
  observeEvent(input$visualize, {
    # Convert text area inputs to lists
    earn_player_1 <- as.numeric(unlist(strsplit(input$player1_earn, "\n")))
    earn_player_2 <- as.numeric(unlist(strsplit(input$player2_earn, "\n")))
    
    # Check if any input list is empty
    if (length(earn_player_1) == 0 || length(earn_player_2) == 0) {
      stop("All input fields must be provided.")
    }
    
    # Check if lengths of earnings vectors match
    if (length(earn_player_1) %% 2 != 0 || length(earn_player_1) != length(earn_player_2)) {
      stop("Lengths of earnings vectors must be even and match.")
    }
    
    # Create named list for data frame columns
    column_names <- c(
      paste("Decyzja_", input$player1, sep = ""),
      paste("Decyzja_", input$player2, sep = ""),
      paste("Zysk_", input$player1, sep = ""),
      paste("Zysk_", input$player2, sep = "")
    )
    
    # Create decisions vectors
    decision_player_1 <- c(rep("A", length(earn_player_1) / 2), rep("B", length(earn_player_1) / 2))
    decision_player_2 <- rep(c("Yes", "No"), length(earn_player_2) / 2)
    
    # Create decyzje data frame based on user inputs
    decyzje <- data.frame(
      setNames(
        list(
          decision_player_1,
          decision_player_2,
          earn_player_1,
          earn_player_2
        ),
        column_names
      )
    )
    
    
    # Create a VisualizeGame object
    game <- VisualizeGame$new(
      player_1 = input$player1,
      player_2 = input$player2,
      decision_player_1 = decyzje[[paste("Decyzja_", input$player1, sep = "")]],
      decision_player_2 = decyzje[[paste("Decyzja_", input$player2, sep = "")]],
      earn_player_1 = decyzje[[paste("Zysk_", input$player1, sep = "")]],
      earn_player_2 = decyzje[[paste("Zysk_", input$player2, sep = "")]]
    )
    
    # Plot the game tree
    output$tree <- renderPlot({
      player1_col <- paste("Decyzja_", input$player1, sep = "")
      player2_col <- paste("Decyzja_", input$player2, sep = "")
      earn_col <- paste("Zysk_", input$player1, sep = "")
      formula <- as.formula(paste0(earn_col, " ~ ", player1_col, " + ", player2_col))
      
      game$Plot_Game_Tree(
        formula = formula,
        data = decyzje
      )
    })
    
    # Display the game table
    output$datatable <- renderTable({
      game$Create_Game_Table(player_1_name = input$player1, player_2_name = input$player2)
    })
  })
}


# Run the application
shinyApp(ui = ui, server = server)

