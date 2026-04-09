#' Get Optimal Sequence
#'
#' Determines an optimal song sequence for a playlist by minimizing the mean squared distance between consecutive tracks based on selected audio features.
#'
#'
#' @param playlist data.frame containing song metadata and audio features
#' @param features feature names to use (default: default_features)
#' @param weights per-feature weights (default: default_weights)
#' @param start_idx integer (1-based row index) of the song to pin as the first track (default: 1)
#' @param harmonic_weight Weight for Camelot wheel penalty. Recommend 0 to disable, 0.5 for moderate penalty, or 1.0 for strict. (default=0.5)
#' @param output_csv_path character path to save the optimized playlist (default: NULL)
#' @param verbose logical; whether to print progress to console (default: FALSE)
#'
#' @details
#' This function constructs an optimized ordering of tracks such that consecutive
#' songs are as similar as possible in terms of audio features.
#'
#' \strong{Step 1 — Feature preparation:}
#' Selected audio features are standardized and weighted according to
#' \code{default_weights}. See \link{feature_weighting} for details.
#'
#' \strong{Step 2 — Distance metric:}
#' The dissimilarity between two tracks is computed using weighted squared
#' Euclidean distance:
#'
#' \deqn{d(i, j) = \|x_i - x_j\|^2}
#'
#' If harmonic information is available, an additional penalty is applied:
#'
#' \deqn{d_{total}(i,j) = d_{features}(i,j) + \lambda \cdot d_{harmonic}(i,j)}
#'
#' where \eqn{\lambda} is \code{harmonic_weight}. See \link{harmonic_concepts}.
#'
#' \strong{Step 3 — Sequence construction (Nearest-Neighbour Heuristic):}
#' Starting from a user-specified track (\code{start_idx}), the algorithm
#' iteratively selects the next track that is closest (minimum distance) to
#' the current track among the remaining unvisited tracks.
#'
#' \deqn{
#' next = \arg\min_{j \in \text{unvisited}} d(current, j)
#' }
#'
#' This produces a greedy approximation to the optimal ordering.
#'
#' \strong{Step 4 — Cost evaluation:}
#' The total sequence cost is computed as the sum of distances between
#' consecutive tracks:
#'
#' \deqn{
#' C = \sum_{i=1}^{n-1} d(\pi_i, \pi_{i+1})
#' }
#'
#' where \eqn{\pi} is the ordering.
#'
#' \strong{Interpretation:}
#' Lower cost indicates smoother transitions between tracks and a more cohesive
#' listening experience.
#'
#' \strong{Note:}
#' The nearest-neighbour approach is computationally efficient but does not
#' guarantee a globally optimal sequence (similar to heuristic solutions of the
#' Traveling Salesman Problem).
#'
#' @return
#' A list (returned invisibly) containing:
#' \itemize{
#'   \item \code{features_used} — Features used in the distance computation
#'   \item \code{initial_order_index} — Original ordering indices
#'   \item \code{initial_cost} — Cost of the original playlist ordering
#'   \item \code{original_playlist} — Input playlist
#'   \item \code{nearest_neighbor} — List with:
#'     \itemize{
#'       \item \code{sequenced_playlist} — Optimized playlist
#'       \item \code{total_cost} — Cost of optimized sequence
#'       \item \code{order_index} — New ordering indices
#'     }
#' }
#' @examples
#' songs <- read.csv(system.file("extdata", "songs.csv", package = "PlaylistPolice"))
#' result <- get_optimal_sequence(songs)
#'
#' @export

get_optimal_sequence <- function(
  playlist,
  features = default_features,
  weights = default_weights,
  start_idx = 1L,
  harmonic_weight = 0.5,
  output_csv_path = NULL,
  verbose = FALSE
) {
  # ── Validate & load ──────────────────────────────────────────────────────────

  if (nrow(playlist) == 0) stop("playlist has no rows")
  if (verbose) cat(sprintf("Loaded %d songs from playlist\n\n", nrow(playlist)))

  start_idx <- as.integer(start_idx)
  if (is.na(start_idx) || start_idx < 1L || start_idx > nrow(playlist)) {
    stop(sprintf("start_idx must be in [1, %d]. Got: %d", nrow(playlist), start_idx))
  }

  # ── Build harmonic distance matrix ──────────────────────────────────────────
  # Returns NULL if CSV has no "key"/"mode" column.
  harm_mat <- harmonic_distance_matrix(playlist)
  hw <- if (!is.null(harm_mat)) harmonic_weight else 0

  # Baseline: cost of the input playlist in its original CSV order
  prepared_features <- prepare_scaled_features(playlist, features, weights = weights)
  initial_order_idx <- seq_len(nrow(playlist))
  initial_cost <- compute_order_cost(
    prepared_features$matrix,
    initial_order_idx,
    harm_mat,
    hw
  )

  if (verbose) {
    if (!is.null(harm_mat)) {
      cat("Harmonic (Camelot wheel) penalty enabled.\n\n")
    } else if (harmonic_weight > 0) {
      cat("Note: 'key'/'mode' columns not found — harmonic penalty disabled.\n\n")
    }
  }

  # ── Step 1: Nearest-neighbour greedy seed ────────────────────────────────────
  if (verbose) {
    cat(sprintf(
      "Nearest-neighbour sequence (start song: row %d — '%s')...\n",
      start_idx,
      if ("name" %in% tolower(names(playlist))) {
        playlist[[names(playlist)[tolower(names(playlist)) == "name"][1]]][start_idx]
      } else {
        "unknown"
      }
    ))
  }

  nn_result <- nearest_neighbor_sequence(
    playlist,
    features,
    weights     = weights,
    start_idx   = start_idx, # user's chosen starting song, pinned to position 1
    harm_matrix = harm_mat,
    harm_weight = hw
  )
  if (verbose) cat(sprintf("      NN cost: %.6f\n\n", nn_result$total_cost))

  # ── Optional: save optimised playlist ────────────────────────────────────────
  if (!is.null(output_csv_path)) {
    write.csv(nn_result$sequenced_playlist, output_csv_path, row.names = FALSE)
    if (verbose) cat(sprintf("\nOptimised playlist saved to: %s\n", output_csv_path))
  }

  # ── Summary ──────────────────────────────────────────────────────────────────
  if (verbose) {
    cat("\n===== Pipeline Cost Summary =====\n")
    cat(sprintf("Initial playlist cost:    %.6f\n", initial_cost))
    cat(sprintf("Nearest-neighbour cost:   %.6f\n", nn_result$total_cost))
    cat(sprintf(
      "Improvement (Initial → NN): %.2f%%\n",
      (initial_cost - nn_result$total_cost) /
        initial_cost * 100
    ))
  }

  invisible(list(
    features_used       = resolve_feature_names(playlist, features),
    initial_order_index = initial_order_idx,
    initial_cost        = initial_cost,
    original_playlist   = playlist,
    nearest_neighbor    = nn_result
  ))
}
