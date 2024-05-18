library(igraph)
library(R6)

Game <- R6Class(
  "Game",
  
  public = list(
    player1_name = NULL,
    player2_name = NULL,
    edges = NULL,
    edge_labels = NULL,
    start_game_clicked = FALSE,
    node_count = 0,
    
    initialize = function(player1_name, player2_name, edges = c(), edge_labels = c(), start_game_clicked = FALSE, node_count = 0) {
      self$player1_name <- player1_name
      self$player2_name <- player2_name
      self$edges <- edges
      self$edge_labels <- edge_labels
      self$start_game_clicked <- start_game_clicked
      self$node_count <- node_count
    },
    
    addDecisionAndEdge = function(root_node_index, decision, is_last_node = FALSE, points_Player1 = 0, points_Player2 = 0) {
      if (is_last_node) {
        next_letter <- if (length(self$edges) == 0) "B" else LETTERS[max(as.numeric(factor(substr(self$edges, 1, 1), levels = LETTERS))) + 1]
        last_node <- paste(next_letter, ". (", points_Player1, " | ", points_Player2, ")", sep = "")
        self$edges <- c(self$edges, root_node_index, last_node)
      } else {
        next_letter <- if (length(self$edges) == 0) "B" else LETTERS[max(as.numeric(factor(substr(self$edges, 1, 1), levels = LETTERS))) + 1]
        new_node <- next_letter
        self$edges <- c(self$edges, root_node_index, new_node)
      }
      self$edge_labels <- c(self$edge_labels, decision)
    },
    
    resetGame = function() {
      self$edges <- c()
      self$edge_labels <- c()
      self$start_game_clicked <- FALSE
      self$node_count <- 0
    },
    
    plotTree = function() {
      if (!is.null(self$edges) && length(self$edges) > 0) {
        g <- graph(self$edges, directed = TRUE)
        
        distances_from_root <- distances(g, v = "A", to = V(g), mode = "out")
        tree_depth <- max(distances_from_root[distances_from_root != Inf]) + 1
        
        if (!is.null(tree_depth)) {
          tree_layout <- layout_as_tree(g, root = "A")
          
          y_axis_labels <- if (tree_depth %% 2 == 0) {
            rep(c(self$player2_name, self$player1_name), length.out = tree_depth)
          } else {
            rep(c(self$player1_name, self$player2_name), length.out = tree_depth)
          }
          
          plot.new()
          padding <- 0.5 # Adjust the padding value as needed
          plot.window(xlim = range(tree_layout[, 1]), ylim = c(-padding, tree_depth - 1 + padding))
          axis(2, at = seq(0, tree_depth - 1, by = 1), labels = y_axis_labels)
          
          par(new = TRUE)
          plot(g, layout = tree_layout,
               vertex.color = "lightblue",
               vertex.size = 40,
               vertex.label.color = "black",
               edge.arrow.size = 0.5,
               edge.color = "gray",
               edge.label = self$edge_labels,
               edge.label.color = "black",
               rescale = FALSE, 
               xlim = range(tree_layout[, 1]), 
               ylim = c(-padding, tree_depth - 1 + padding))
          
          mtext("Decision", side = 2, line = 3)
        }
      }
    }
  )
)

GameVectors <- R6Class(
  "GameVectors",
  inherit = Game,
  
  public = list(
    dec1 = NULL,
    dec2 = NULL,
    payouts1 = NULL,
    payouts2 = NULL,
    i = 1,
    
    initialize = function(player1_name, player2_name, edges = c(), edge_labels = c(), start_game_clicked = FALSE, node_count = 0, dec1 = c(), dec2 = c(), payouts1 = c(), payouts2 = c()) {
      super$initialize(player1_name, player2_name, edges, edge_labels, start_game_clicked, node_count)
      self$dec1 <- dec1
      self$dec2 <- dec2
      self$payouts1 <- payouts1
      self$payouts2 <- payouts2
    },
    
    addDecisionAndEdge_Vector = function(root_node_index, decision, is_last_node = FALSE, points_Player1 = 0, points_Player2 = 0) {
      if (self$i %% 2 == 1) {
        self$dec1 <- c(self$dec1, decision)
      } else {
        self$dec2 <- c(self$dec2, decision)
      }
      self$i <- self$i + 1
      
      if (is_last_node) {
        self$payouts1 <- c(self$payouts1, points_Player1)
        print(self$payouts1)
        self$payouts2 <- c(self$payouts2, points_Player2)
        print(self$payouts2)
      }
      
      node_index <- paste0(root_node_index, "_", length(self$edges) + 1)
    }
  )
)

