library(R6)
library(igraph)

Game <- R6Class(
  "Game",
  
  public = list(
    player1_name = NULL,
    player2_name = NULL,
    edges = NULL,
    edge_labels = NULL,
    node_data = NULL,
    start_game_clicked = FALSE,
    
    initialize = function(player1_name, player2_name, edges = c(), edge_labels = c(), node_data = list(), start_game_clicked = FALSE) {
      self$player1_name <- player1_name
      self$player2_name <- player2_name
      self$edges <- edges
      self$edge_labels <- edge_labels
      self$node_data <- node_data
      self$start_game_clicked <- start_game_clicked
    },
    
    addDecisionAndEdge = function(root_node_index, decision, is_last_node = FALSE, points_Player1 = 0, points_Player2 = 0) {
      next_letter <- if (length(self$edges) == 0) "B" else LETTERS[max(as.numeric(factor(substr(self$edges, 1, 1), levels = LETTERS))) + 1]
      if (is_last_node) {
        last_node <- paste(next_letter, ". (", points_Player1, " | ", points_Player2, ")", sep = "")
        self$edges <- c(self$edges, root_node_index, last_node)
        self$node_data[[last_node]] <- list(points_Player1 = points_Player1, points_Player2 = points_Player2, decision_path = c())
      } else {
        new_node <- next_letter
        self$edges <- c(self$edges, root_node_index, new_node)
        self$node_data[[new_node]] <- list(points_Player1 = NULL, points_Player2 = NULL, decision_path = c())
      }
      self$edge_labels <- c(self$edge_labels, decision)
      print(self$edges)
      print(self$edge_labels)
    },
    
    getPathsToPayoffs = function() {
      leaf_nodes <- names(self$node_data)[sapply(self$node_data, function(x) !is.null(x$points_Player1) && !is.null(x$points_Player2))]
      paths_to_payoffs <- list()
      
      for (leaf in leaf_nodes) {
        path <- self$backtrackPath(leaf)
        payoff_key <- paste("(", self$node_data[[leaf]]$points_Player1, ", ", self$node_data[[leaf]]$points_Player2, ")", sep = "")
        if (!payoff_key %in% names(paths_to_payoffs)) {
          paths_to_payoffs[[payoff_key]] <- list()
        }
        paths_to_payoffs[[payoff_key]] <- c(paths_to_payoffs[[payoff_key]], list(path))
      }
      return(paths_to_payoffs)
    },
    
    backtrackPath = function(leaf_node) {
      path <- c()
      current_node <- leaf_node
      
      while (current_node != "A") {
        edge_index <- which(self$edges == current_node)
        if (length(edge_index) == 0) break
        edge_index <- edge_index[1] - 1
        path <- c(self$edge_labels[ceiling(edge_index / 2)], path)
        current_node <- self$edges[edge_index]
      }
      
      return(path)
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
          padding <- 0.5
          plot.window(xlim = range(tree_layout[, 1]), ylim = c(-padding, tree_depth - 1 + padding))
          axis(2, at = seq(0, tree_depth - 1, by = 1), labels = y_axis_labels)
          
          par(new = TRUE)
          plot(g, layout = tree_layout,
               vertex.color = "#e4fde1",
               vertex.size = 40,
               vertex.label.color = "black",
               edge.arrow.size = 1,
               edge.color = "black",
               edge.label = self$edge_labels,
               edge.label.color = "black",
               edge.label.bg = "#e4fde1",
               rescale = FALSE, 
               xlim = range(tree_layout[, 1]), 
               ylim = c(-padding, tree_depth - 1 + padding))
          
          mtext("Decision", side = 2, line = 3)
        }
      }
    },
    
    getEdges = function() {
      return(self$edges)
    },
    
    hasTerminalNode = function() {
      return(any(grepl("\\(", self$edges)))
    },
    
    performBackwardInduction = function() {
      paths_to_payoffs <- self$getPathsToPayoffs()
      
      # Extract all paths and corresponding payoffs
      all_paths <- list()
      for (payoff_key in names(paths_to_payoffs)) {
        paths <- paths_to_payoffs[[payoff_key]]
        payoff_values <- as.numeric(strsplit(gsub("[()]", "", payoff_key), ", ")[[1]])
        for (path in paths) {
          all_paths <- c(all_paths, list(list(path = path, payoffs = payoff_values)))
        }
      }
      
      # Identify the path with the highest payoff for Player 1
      best_path_player1 <- NULL
      max_payoff_player1 <- -Inf
      for (path_info in all_paths) {
        if (path_info$payoffs[1] > max_payoff_player1) {
          max_payoff_player1 <- path_info$payoffs[1]
          best_path_player1 <- path_info$path
        }
      }
      
      # Identify the path with the highest payoff for Player 2
      best_path_player2 <- NULL
      max_payoff_player2 <- -Inf
      for (path_info in all_paths) {
        if (path_info$payoffs[2] > max_payoff_player2) {
          max_payoff_player2 <- path_info$payoffs[2]
          best_path_player2 <- path_info$path
        }
      }
      
      # Return the best paths for both players
      return(list(
        best_path_player1 = best_path_player1,
        max_payoff_player1 = max_payoff_player1,
        best_path_player2 = best_path_player2,
        max_payoff_player2 = max_payoff_player2
      ))
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
    
    initialize = function(player1_name, player2_name, edges = c(), edge_labels = c(), node_data = list(), start_game_clicked = FALSE, dec1 = c(), dec2 = c(), payouts1 = c(), payouts2 = c()) {
      super$initialize(player1_name, player2_name, edges, edge_labels, node_data, start_game_clicked)
      self$dec1 <- dec1
      self$dec2 <- dec2
      self$payouts1 <- payouts1
      self$payouts2 <- payouts2
    },
    
    addDecisionAndEdge_Vector = function(root_node_index, decision, is_last_node = FALSE, points_Player1 = 0, points_Player2 = 0) {
      super$addDecisionAndEdge(root_node_index, decision, is_last_node, points_Player1, points_Player2)
      
      if (self$i %% 2 == 1) {
        self$dec1 <- c(self$dec1, decision)
      } else {
        self$dec2 <- c(self$dec2, decision)
      }
      self$i <- self$i + 1
      
      if (is_last_node) {
        self$payouts1 <- c(self$payouts1, points_Player1)
        self$payouts2 <- c(self$payouts2, points_Player2)
      }
    },
    
    correlatePayoffsWithPaths = function() {
      return(self$getPathsToPayoffs())
    }
  )
)