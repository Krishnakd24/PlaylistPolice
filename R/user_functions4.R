#' Check Playlist Compatibility
#'
#' @description
#' Measures how compatible two playlists are by comparing the average distance
#' between songs across playlists vs within each playlist. If songs from
#' Playlist A are just as close to songs in Playlist B as they are to each
#' other, the playlists are highly compatible.
#'
#' @param playlist_A data.frame containing song metadata and audio features
#' @param playlist_B data.frame containing song metadata and audio features
#' @param features character vector of feature names (default: default_features)
#' @param weights named numeric vector of per-feature weights (default: default_weights)
#' @param harmonic_weight Weight for Camelot wheel penalty. Recommend 0 to disable, 0.5 for moderate penalty, or 1.0 for strict.(default:0.5)
#' @param verbose logical; whether to print summary to console (default: FALSE)
#' @details
#' This function evaluates how well two playlists fit together by comparing
#' distances between songs across playlists and within each playlist.
#'
#' \strong{Step 1 — Feature preparation:}
#' Both playlists are combined and scaled using shared statistics to ensure
#' comparability. See \link{feature_weighting}.
#'
#' \strong{Step 2 — Distance computation:}
#' Pairwise squared Euclidean distances are computed between songs.
#' Harmonic penalties may be added if key/mode data is available.
#'
#' \strong{Step 3 — Cross-playlist distance:}
#' For each song in one playlist, the average distance to all songs in the
#' other playlist is computed.
#'
#' \strong{Step 4 — Within-playlist baseline:}
#' The average internal distance within each playlist is computed to establish
#' a baseline for comparison.
#'
#' \strong{Step 5 — Compatibility score:}
#' The score is defined as:
#'
#' \deqn{
#' score = \frac{\text{within-distance}}{\text{cross-distance}} \times 100
#' }
#'
#' Higher scores indicate greater compatibility.
#'
#' \strong{Interpretation:}
#' \itemize{
#'   \item ~100 → highly compatible playlists
#'   \item ~50 → moderate compatibility
#'   \item ~0 → very different playlists
#' }
#' @return
#' A list containing:
#' \itemize{
#'   \item \code{compatibility_score} — Numeric score (0–100)
#'   \item \code{mean_cross_distance} — Average distance between playlists
#'   \item \code{mean_within_distance} — Average internal playlist distance
#' }
#'
#' @seealso \link{feature_weighting}, \link{harmonic_concepts}
#'
#' @examples
#' songs <- read.csv(system.file("extdata", "songs.csv", package = "PlaylistPolice"))
#' half <- floor(nrow(songs) / 2)
#' playlist_A <- songs[1:half, ]
#' playlist_B <- songs[(half + 1):nrow(songs), ]
#' result <- check_playlist_compatibility(playlist_A, playlist_B)
#'
#' @export
check_playlist_compatibility <- function(playlist_A,
                                         playlist_B,
                                         features = default_features,
                                         weights = default_weights,
                                         harmonic_weight = 0.5,
                                         verbose = FALSE) {
  # ── Input validation ───────────────────────────────────────────────────────
  if (!is.data.frame(playlist_A) || !is.data.frame(playlist_B)) {
    stop("playlist_A and playlist_B must both be data.frames")
  }
  if (nrow(playlist_A) < 2) stop("playlist_A needs at least 2 songs")
  if (nrow(playlist_B) < 2) stop("playlist_B needs at least 2 songs")

  # ── Scale both playlists on COMBINED statistics ────────────────────────────
  # Combining first ensures both playlists live in the same scaled space
  combined <- rbind(playlist_A, playlist_B)
  prepared <- prepare_scaled_features(combined, features, weights = weights)

  n_A <- nrow(playlist_A)
  n_B <- nrow(playlist_B)
  n_feat <- ncol(prepared$matrix)
  mat_A <- prepared$matrix[seq_len(n_A), , drop = FALSE]
  mat_B <- prepared$matrix[seq(n_A + 1, n_A + n_B), , drop = FALSE]

  harm_mat <- harmonic_distance_matrix(combined)

  # ── Cross-playlist distances ───────────────────────────────────────────────
  # For each song in A, mean squared distance to every song in B (and vice versa)
  cross_dists_A <- numeric(n_A)
  for (i in seq_len(n_A)) {
    diffs <- mat_B - matrix(mat_A[i, ], nrow = n_B, ncol = n_feat, byrow = TRUE)
    sq_dist <- rowSums(diffs^2)
    if (!is.null(harm_mat) && harmonic_weight > 0) sq_dist <- sq_dist + (harm_mat[i, seq(n_A + 1, n_A + n_B)] * harmonic_weight)
    cross_dists_A[i] <- mean(sq_dist)
  }

  cross_dists_B <- numeric(n_B)
  for (i in seq_len(n_B)) {
    diffs <- mat_A - matrix(mat_B[i, ], nrow = n_A, ncol = n_feat, byrow = TRUE)
    sq_dist <- rowSums(diffs^2)
    if (!is.null(harm_mat) && harmonic_weight > 0) sq_dist <- sq_dist + (harm_mat[n_A + i, seq_len(n_A)] * harmonic_weight)
    cross_dists_B[i] <- mean(sq_dist)
  }

  mean_cross <- mean(c(cross_dists_A, cross_dists_B))

  # ── Within-playlist distances (baseline) ───────────────────────────────────
  # How spread out each playlist is internally
  within_A <- numeric(n_A)
  for (i in seq_len(n_A)) {
    diffs <- mat_A[-i, , drop = FALSE] -
      matrix(mat_A[i, ], nrow = n_A - 1, ncol = n_feat, byrow = TRUE)
    sq_dist <- rowSums(diffs^2)
    if (!is.null(harm_mat) && harmonic_weight > 0) sq_dist <- sq_dist + (harm_mat[i, seq_len(n_A)][-i] * harmonic_weight)
    within_A[i] <- mean(sq_dist)
  }

  within_B <- numeric(n_B)
  for (i in seq_len(n_B)) {
    diffs <- mat_B[-i, , drop = FALSE] -
      matrix(mat_B[i, ], nrow = n_B - 1, ncol = n_feat, byrow = TRUE)
    sq_dist <- rowSums(diffs^2)
    if (!is.null(harm_mat) && harmonic_weight > 0) sq_dist <- sq_dist + (harm_mat[n_A + i, seq(n_A + 1, n_A + n_B)][-i] * harmonic_weight)
    within_B[i] <- mean(sq_dist)
  }

  baseline <- mean(c(within_A, within_B))

  # ── Compatibility score ────────────────────────────────────────────────────
  # If cross ≈ baseline → songs fit just as well across playlists → 100%
  # If cross >> baseline → playlists are far apart → score drops toward 0%
  score <- min(100, max(0, (baseline / max(mean_cross, 1e-10)) * 100))

  # ── Print summary ──────────────────────────────────────────────────────────
  if (verbose) {
    cat(sprintf("\n===== Playlist Compatibility =====\n"))
    cat(sprintf("Playlist A: %d songs\n", n_A))
    cat(sprintf("Playlist B: %d songs\n", n_B))
    cat(sprintf("Avg within-playlist distance:  %.4f\n", baseline))
    cat(sprintf("Avg cross-playlist distance:   %.4f\n", mean_cross))
    cat(sprintf("──────────────────────────────────\n"))
    cat(sprintf("COMPATIBILITY SCORE:  %.1f / 100\n", score))
  }

  invisible(list(
    compatibility_score  = round(score, 1),
    mean_cross_distance  = round(mean_cross, 4),
    mean_within_distance = round(baseline, 4)
  ))
}
