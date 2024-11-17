# Initialize the grid
initialize_grid <- function(width, height, empty_ratio) {
  grid <- sample(c(-1, 1), width * height, replace = TRUE)
  grid <- matrix(grid, nrow = width, ncol = height)
  empty_count <- floor(width * height * empty_ratio)
  empty_indices <- sample(seq_len(width * height), empty_count)
  grid[empty_indices] <- 0
  return(grid)
}

# Get neighbor indices
get_neighbours_index <- function(x, y, width, height) {
  neighbors <- expand.grid(dx = -1:1, dy = -1:1)
  neighbors <- neighbors[!(neighbors$dx == 0 & neighbors$dy == 0), ]
  
  valid_neighbors <- neighbors %>%
    dplyr::mutate(nx = x + dx, ny = y + dy) %>%
    dplyr::filter(nx >= 1 & nx <= width & ny >= 1 & ny <= height)
  
  indices <- mapply(function(nx, ny) (ny - 1) * width + nx, 
                    valid_neighbors$nx, valid_neighbors$ny)
  return(indices)
}

# Check if agent is unsatisfied
is_unsatisfied <- function(grid, x, y, similarity_threshold) {
  current_agent <- grid[x, y]
  if (current_agent == 0) return(FALSE)
  
  neighbors <- get_neighbours_index(x, y, nrow(grid), ncol(grid))
  neighbor_values <- grid[neighbors]
  
  similar_count <- sum(neighbor_values == current_agent, na.rm = TRUE)
  different_count <- sum(neighbor_values == -current_agent, na.rm = TRUE)
  
  if (similar_count + different_count == 0) {
    return(TRUE)
  }
  
  ratio <- similar_count / (similar_count + different_count)
  return(ratio < similarity_threshold)
}

# Move agent to an empty spot
move_to_empty <- function(grid, x, y) {
  empty_indices <- which(grid == 0, arr.ind = TRUE)
  if (nrow(empty_indices) == 0) return(grid)
  
  random_index <- sample(seq_len(nrow(empty_indices)), 1)
  empty_x <- empty_indices[random_index, 1]
  empty_y <- empty_indices[random_index, 2]
  
  grid[empty_x, empty_y] <- grid[x, y]
  grid[x, y] <- 0
  
  return(grid)
}

# Update the grid
update_grid <- function(grid, similarity_threshold) {
  changes <- 0
  for (i in seq_len(nrow(grid) * ncol(grid))) {
    coord <- arrayInd(i, .dim = dim(grid))
    x <- coord[1]
    y <- coord[2]
    
    if (grid[x, y] == 0) next
    
    if (is_unsatisfied(grid, x, y, similarity_threshold)) {
      grid <- move_to_empty(grid, x, y)
      changes <- changes + 1
    }
  }
  return(list(grid = grid, changes = changes))
}



plot_grid <- function(grid, iteration) {
  # Set up the plotting area
  par( mar = c(4, 1, 2, 1), xpd=TRUE ) # Adjust margins
  plot(
    1:ncol(grid), 1:nrow(grid),
    type = "n", xlab = "", ylab = "", axes = FALSE, asp = 1,
    main = paste("Schelling Segregation Simulation - Iteration", iteration)
  )
  
  # Draw each cell
  for (x in 1:nrow(grid)) {
    for (y in 1:ncol(grid)) {
      if (grid[x, y] == 1) {
        col <- "red" # Type 1
      } else if (grid[x, y] == -1) {
        col <- "blue" # Type -1
      } else {
        col <- "white" # Empty
      }
      
      # Draw the rectangle
      rect(
        x - 0.5, y - 0.5, x + 0.5, y + 0.5,
        col = col, border = "gray"
      )
    }
  }
  
  # Add a legend
  legend(
    "bottom", inset=c(0,-0.1),
    legend = c("Type 1", "Type -1", "Empty"),
    fill = c("red", "blue", "white"), horiz = TRUE, bty = "n"
  )
}



# Run the simulation
run_simulation <- function(width, height, empty_ratio, similarity_threshold, n_iterations) {

  resL <- list()
  grid <- initialize_grid(width, height, empty_ratio)

  for (iteration in seq_len(n_iterations)) {

    resL[[ iteration ]] <- grid

    result <- update_grid(grid, similarity_threshold)
    grid <- result$grid
    changes <- result$changes
    
    cat(sprintf("Iteration %d: %d changes\n", iteration, changes))
    plot_grid(grid, iteration)
    
    if (changes == 0) {
      cat("System stabilized\n")
      break
    }
  }

  return(resL)
}

