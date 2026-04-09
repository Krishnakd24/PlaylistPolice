#This file contains code for optimal Sequencing of songs in a playlist 


# Purpose : build a ordering using a greedy nearest-neighbour
#           heuristic.

#' @keywords internal
nearest_neighbor_sequence <- function(playlist,
                                      features,
                                      weights = NULL,
                                      start_idx = 1L,
                                      harm_matrix = NULL,
                                      harm_weight = harmonic_weight) {
  # ── Edge cases ───────────────────────────────────────────────────────────────
  if (nrow(playlist) == 0) {
    stop("playlist has no rows")
  }

  # Validate start_idx: must be a single integer in [1, N]
  start_idx <- as.integer(start_idx)
  if (length(start_idx) != 1 || is.na(start_idx) ||
    start_idx < 1L || start_idx > nrow(playlist)) {
    stop(sprintf("start_idx must be a single integer in [1, %d].", nrow(playlist)))
  }

  # Single-song playlist: nothing to sequence
  if (nrow(playlist) == 1) {
    return(list(
      sequenced_playlist = playlist,
      total_cost = 0,
      order_index = 1L
    ))
  }

  # ── Setup ────────────────────────────────────────────────────────────────────
  prepared <- prepare_scaled_features(playlist, features, weights = weights)
  scaled_matrix <- prepared$matrix
  n <- nrow(scaled_matrix)
  nc <- ncol(scaled_matrix)

  order_idx <- integer(n)
  visited <- rep(FALSE, n)
  order_idx[1] <- start_idx # pin start_idx to position 1 always
  visited[start_idx] <- TRUE

  # ── Greedy NN loop ───────────────────────────────────────────────────────────
  for (pos in 2:n) {
    current <- order_idx[pos - 1]
    unvisited <- which(!visited)

    # Broadcast current song's feature vector to match the number of unvisited rows
    current_mat <- matrix(scaled_matrix[current, ],
      nrow = length(unvisited), ncol = nc, byrow = TRUE
    )

    # Squared Euclidean distances from current song to all unvisited songs
    dist2 <- rowSums((scaled_matrix[unvisited, , drop = FALSE] - current_mat)^2)

    # Optionally add squared harmonic penalty so key compatibility affects NN too
    if (!is.null(harm_matrix) && harm_weight > 0) {
      dist2 <- dist2 + (harm_matrix[current, unvisited] * harm_weight)
    }

    # Move to the closest unvisited song
    order_idx[pos] <- unvisited[which.min(dist2)]
    visited[order_idx[pos]] <- TRUE
  }

  # ── Build output ─────────────────────────────────────────────────────────────
  sequenced <- playlist[order_idx, , drop = FALSE]
  rownames(sequenced) <- NULL # clear row names so downstream code isn't confused

  list(
    sequenced_playlist = sequenced,
    total_cost = compute_order_cost(
      scaled_matrix, order_idx,
      harm_matrix, harm_weight
    ),
    order_index = order_idx
  )
}

# Other methods can be used so that final sequnece is not in local minima
# One method is simulated annealing can be used to find the optimal sequence
