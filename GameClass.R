library(R6)
library(igraph)
library(data.table)

Game <- R6Class(
  "Game",
  
  # Game class which holds all of the logic of game calculations
  # player_name - names of the players
  # edges - nodes of the game tree
  # edge_labels - decisions of the game tree
  # node_data - data of the nodes combined
  # start_game_clicked - boolean to check if the game has started
  public = list(
    player1_name = NULL,
    player2_name = NULL,
    edges = NULL,
    edge_labels = NULL,
    node_data = NULL,
    start_game_clicked = FALSE,
    
    # initialize the class
    initialize = function(player1_name, player2_name, edges = c(), edge_labels = c(), node_data = list(), start_game_clicked = FALSE) {
      self$player1_name <- player1_name
      self$player2_name <- player2_name
      self$edges <- edges
      self$edge_labels <- edge_labels
      self$node_data <- node_data
      self$start_game_clicked <- start_game_clicked
    },
    
    # add a decision and edge to the game tree
    addDecisionAndEdge = function(root_node_index, decision, is_last_node = FALSE, points_Player1 = 0, points_Player2 = 0) {
      # always add next letter in the alphabet as a new node
      next_letter <- if (length(self$edges) == 0) "B" else LETTERS[max(as.numeric(factor(substr(self$edges, 1, 1), levels = LETTERS))) + 1]
      
      # change last node index to represent also payoffs (points_Player1 | points_Player2)
      if (is_last_node) {
        last_node <- paste(next_letter, "\n(", points_Player1, " | ", points_Player2, ")", sep = "")
        self$edges <- c(self$edges, root_node_index, last_node)
        self$node_data[[last_node]] <- list(points_Player1 = points_Player1, points_Player2 = points_Player2, decision_path = c())
      # else keep node index as next letter
      } else {
        new_node <- next_letter
        self$edges <- c(self$edges, root_node_index, new_node)
        self$node_data[[new_node]] <- list(points_Player1 = NULL, points_Player2 = NULL, decision_path = c())
      }
      
      self$edge_labels <- c(self$edge_labels, decision)
    },
    
    # add a decision to the decision path of a node
    getPathsToPayoffs = function() {
      # find all last nodes
      leaf_nodes <- names(self$node_data)[sapply(self$node_data, function(x) !is.null(x$points_Player1) && !is.null(x$points_Player2))]
      paths_to_payoffs <- list()
      # find all possible finds to all possible last nodes
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
    
    # get the decision path and nodes from a last node
    backtrackPath = function(leaf_node) {
      path <- c()
      nodes <- c()
      current_node <- leaf_node
      # go back to the root node and collect the decision path
      while (current_node != "A") {
        edge_index <- which(self$edges == current_node)
        if (length(edge_index) == 0) break
        edge_index <- edge_index[1] - 1
        path <- c(self$edge_labels[ceiling(edge_index / 2)], path)
        nodes <- c(current_node, nodes)
        current_node <- self$edges[edge_index]
      }
      nodes <- c("A", nodes)
      
      return(list(decisions = path, nodes = nodes))
    },
    
    # plot the game tree (extensive representation)
    plotTree = function(color_type = 1, best_nodes_player = c()) {
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
          
          vertex_colors <- rep("#dde5b6", length(V(g)))
          if (color_type == 2 || color_type == 3) {
            for (node in best_nodes_player) {
              if (node %in% V(g)$name) {
                vertex_colors[V(g)$name == node] <- "#adc178"
              }
            }
          }
          
          plot(g, layout = tree_layout,
               vertex.color = vertex_colors,
               vertex.size = 60,
               vertex.label.color = "black",
               vertex.label.cex = 1.5,
               edge.arrow.size = 1,
               edge.color = "#6b2737",
               edge.label = self$edge_labels,
               edge.label.color = "black",
               edge.label.bg = "#dde5b6",
               edge.label.cex = 1.2,
               rescale = FALSE, 
               xlim = range(tree_layout[, 1]), 
               ylim = c(-padding, tree_depth - 1 + padding))
          
          mtext("Decision", side = 2, line = 3)
          
        }
      }
    },
    
    # show the game table (strategic representation)
    showTable = function() {
      paths_to_payoffs <- self$getPathsToPayoffs()
      num_paths <- sum(lengths(paths_to_payoffs))
      player1_name_payoff <- paste(self$player1_name, "Payoff")
      player2_name_payoff <- paste(self$player2_name, "Payoff")
      
      table_data <- data.frame(
        "Decision Path" = character(num_paths),
        player1_name_payoff = numeric(num_paths),
        player2_name_payoff = numeric(num_paths),
        stringsAsFactors = FALSE
      )
      
      colnames(table_data) <- c("Decision Path", player1_name_payoff, player2_name_payoff)
      row_idx <- 1
      # fill the table with the decision paths and payoffs
      for (payoff_key in names(paths_to_payoffs)) {
        paths <- paths_to_payoffs[[payoff_key]]
        payoff_values <- as.numeric(strsplit(gsub("[()]", "", payoff_key), ", ")[[1]])
        for (path in paths) {
          path_str <- paste(path$decisions, collapse = " -> ")
          table_data[row_idx, "Decision Path"] <- path_str
          table_data[row_idx, player1_name_payoff] <- payoff_values[1]
          table_data[row_idx, player2_name_payoff] <- payoff_values[2]
          row_idx <- row_idx + 1
        }
      }
      return(table_data)
    },
    
    # get the edges of the game tree
    getEdges = function() {
      return(self$edges)
    },
    
    # check if the game tree has last nodes
    hasTerminalNode = function() {
      return(any(sapply(self$node_data, function(x) !is.null(x$points_Player1) && !is.null(x$points_Player2))))
    },
    
    # perform backward induction to find the best decision path
    performBackwardInduction = function() {
      paths_to_payoffs <- self$getPathsToPayoffs()
      # go through all paths and collect the heighest payoffs for each player
      all_paths <- list()
      for (payoff_key in names(paths_to_payoffs)) {
        paths <- paths_to_payoffs[[payoff_key]]
        payoff_values <- as.numeric(strsplit(gsub("[()]", "", payoff_key), ", ")[[1]])
        for (path in paths) {
          all_paths <- c(all_paths, list(list(path = path, payoffs = payoff_values)))
        }
      }
      
      # best path for player 1
      best_path_player1 <- NULL
      best_nodes_player1 <- NULL
      max_payoff_player1 <- -Inf
      for (path_info in all_paths) {
        if (path_info$payoffs[1] > max_payoff_player1) {
          max_payoff_player1 <- path_info$payoffs[1]
          best_path_player1 <- path_info$path$decisions
          best_nodes_player1 <- path_info$path$nodes
        }
      }
      
      # best path for player 2
      best_path_player2 <- NULL
      best_nodes_player2 <- NULL
      max_payoff_player2 <- -Inf
      for (path_info in all_paths) {
        if (path_info$payoffs[2] > max_payoff_player2) {
          max_payoff_player2 <- path_info$payoffs[2]
          best_path_player2 <- path_info$path$decisions
          best_nodes_player2 <- path_info$path$nodes
        }
      }
      
      # return best results
      return(list(
        best_path_player1 = best_path_player1,
        best_nodes_player1 = best_nodes_player1,
        max_payoff_player1 = max_payoff_player1,
        best_path_player2 = best_path_player2,
        best_nodes_player2 = best_nodes_player2,
        max_payoff_player2 = max_payoff_player2
      ))
    }
  )
)
