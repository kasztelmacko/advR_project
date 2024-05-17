library(shiny)
library(igraph)

# Define the UI
ui <- fluidPage(
  titlePanel("Shiny App with Add Node Button"),
  sidebarLayout(
    sidebarPanel(
      actionButton("addNode", "Add Node"),
      checkboxInput("lastNodeToggle", "Last Node", value = FALSE), # Toggle for Last Node
      textInput("rootNodeIndex", "Root Node Index:", value = ""), # Input for Root Node Index
      textInput("nodeIndex", "Node Index:", value = ""), # Input for Node Index
      textInput("decision", "Decision:", value = ""), # Input for Decision
      uiOutput("pointsInput_Player1"), # Placeholder for Points input for Player 1
      uiOutput("pointsInput_Player2") # Placeholder for Points input for Player 2
    ),
    mainPanel(
      plotOutput("network_plot") # Plot of the graph
    )
  )
)

server <- function(input, output, session) {
  # Variable to store the edges and edge labels
  edges <- c()
  edge_labels <- c()
  
  # Variable to store the count of nodes
  node_count <- reactiveVal(0)
  
  observeEvent(input$addNode, {
    node_count(node_count() + 1) # Increment node count
    
    # Print statement after adding a new node
    print("Node added:")
    print(paste("Root Node Index:", input$rootNodeIndex))
    print(paste("Node Index:", input$nodeIndex))
    print(paste("Decision:", input$decision))
  
    
    if (input$lastNodeToggle) {
      print(paste("Points for Player 1:", input$points_Player1))
      print(paste("Points for Player 2:", input$points_Player2))
      
      # Format the last node
      last_node <- paste(input$nodeIndex, ". (", input$points_Player1, " | ", input$points_Player2, ")", sep = "")
      edges <<- c(edges, input$rootNodeIndex, last_node)
      edge_labels <<- c(edge_labels, input$decision)
    } else {
      edges <<- c(edges, input$rootNodeIndex, input$nodeIndex)
      edge_labels <<- c(edge_labels, input$decision)
    }
    print(edges)
    
    # Create the graph object
    g <- graph(edges, directed = TRUE)
    
    # Generate the layout
    tree_layout <- layout_as_tree(g, root = "A")
    
    # Plot the graph
    output$network_plot <- renderPlot({
      plot(g, layout = tree_layout,
           vertex.color = "lightblue",
           vertex.size = 50,  # Adjust the size of the nodes here (default is 30)
           vertex.label.color = "black",
           edge.arrow.size = 0.5,
           edge.color = "gray",
           edge.label = edge_labels,
           edge.label.color = "black")
    }, height = 600, width = 800)
    
    # Clear the input fields after processing
    updateTextInput(session, "rootNodeIndex", value = "")
    updateTextInput(session, "nodeIndex", value = "")
    updateTextInput(session, "decision", value = "")
    updateCheckboxInput(session, "lastNodeToggle", value = FALSE)
    
    # Observer for lastNodeToggle inside addNode
    observeEvent(input$lastNodeToggle, {
      if (input$lastNodeToggle) {
        output$pointsInput_Player1 <- renderUI({
          numericInput("points_Player1", "Points for Player 1:", value = 0) # Input for Points for Player 1
        })
        output$pointsInput_Player2 <- renderUI({
          numericInput("points_Player2", "Points for Player 2:", value = 0) # Input for Points for Player 2
        })
      } else {
        output$pointsInput_Player1 <- renderUI({})
        output$pointsInput_Player2 <- renderUI({})
      }
    })
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
