#' Identify Outlier Songs
#'
#' @description
#' Detects songs that deviate from the overall "mood" of a playlist using a
#' weighted distance metric over audio features.
#'
#' @param playlist data.frame containing song metadata and audio features
#' @param features feature names to use (default: default_features)
#' @param weights per-feature weights (default: default_weights)
#' @param harmonic_weight Numeric scalar controlling the contribution of harmonic
#'   (mode-based) distance to the overall similarity metric. Set to 0 to disable,
#'   0.5 for moderate influence, and 1.0 for strong penalization of harmonic mismatch. (default=0.5)
#' @param threshold_multiplier A sensitivity scalar; higher values flag only the most extreme outliers,
#'  while lower values are more aggressive.(default=0.5)
#' @param verbose logical; whether to print progress to console (default: FALSE)
#' @details
#' This function identifies songs that deviate from the overall "vibe" of a playlist
#' by measuring how different each track is from all others in a weighted feature space.
#'
#' \strong{Step 1 — Feature preparation:}
#' Selected audio features are standardized and weighted according to
#' \code{default_weights}. See \link{feature_weighting} for details.
#'
#' \strong{Step 2 — Distance computation:}
#' For each track, the function computes the mean squared Euclidean distance to all
#' other tracks in the playlist.
#'
#' \deqn{D(i) = \frac{1}{n-1} \sum_{j \neq i} \|x_i - x_j\|^2}
#'
#' \strong{Step 3 — Harmonic penalty (optional):}
#' If \code{harmonic_weight > 0}, an additional penalty is added based on harmonic
#' incompatibility (e.g., mode differences). This term captures tonal mismatch that is
#' not reflected in standard audio features.
#' See \link{harmonic_concepts} for justification of this penalty.
#'
#' \deqn{D_{total}(i,j) = D_{features}(i,j) + \lambda \cdot D_{harmonic}(i,j)}
#'
#' where \eqn{\lambda} is \code{harmonic_weight}.
#'
#' \strong{Step 4 — Outlier detection:}
#' A track is flagged as an outlier if its average distance exceeds:
#'
#' \deqn{\mu + \alpha \cdot \sigma}
#'
#' where \eqn{\mu} and \eqn{\sigma} are the mean and standard deviation of all
#' average distances, and \eqn{\alpha} is \code{threshold_multiplier}.
#'
#' Higher values of \code{threshold_multiplier} result in fewer, more extreme outliers,
#' while lower values increase sensitivity.
#'
#' \strong{Interpretation:}
#' Outlier tracks are those that are consistently dissimilar to the rest of the playlist
#' and may disrupt its cohesion in terms of mood, energy, or style.
#'
#' @return The original playlist data.frame (invisibly) with two new columns appended:
#'   outlier_distance — Numeric vector of the calculated mean squared distances.
#'   is_outlier — Logical vector indicating if the song exceeded the outlier threshold.
#'
#' @examples
#' songs <- read.csv(system.file("extdata", "songs.csv", package = "PlaylistPolice"))
#' result <- get_outlier_songs(songs)
#' print(result)
#'
#' @export
get_outlier_songs <- function(playlist,
                              features = default_features,
                              weights = default_weights,
                              harmonic_weight = 0.5,
                              threshold_multiplier = 0.5,
                              verbose = FALSE) {
  prep <- prepare_scaled_features(playlist,
    features = features,
    weights = weights
  )
  X <- prep$matrix
  n <- nrow(X)

  # For each song, compute mean squared distance to ALL other songs
  avg_dist <- numeric(n)

  harm_mat <- harmonic_distance_matrix(playlist)

  for (i in seq_len(n)) {
    diffs <- X - matrix(X[i, ], nrow = n, ncol = ncol(X), byrow = TRUE)
    sq_dists <- rowSums(diffs^2)

    if (!is.null(harm_mat) && harmonic_weight > 0) {
      sq_dists <- sq_dists + (harm_mat[i, ] * harmonic_weight)
    }

    avg_dist[i] <- mean(sq_dists[-i]) # exclude self
  }

  # Flag anything above mean + multiplier * sd
  threshold <- mean(avg_dist) + threshold_multiplier * sd(avg_dist)
  flagged <- avg_dist > threshold

  # Safe song name extraction for printing
  song_names <- NULL
  if ("name" %in% tolower(names(playlist))) {
    song_names <- playlist[[names(playlist)[tolower(names(playlist)) == "name"][1]]]
  } else if ("track.name" %in% tolower(names(playlist))) {
    song_names <- playlist[[names(playlist)[tolower(names(playlist)) == "track.name"][1]]]
  } else {
    song_names <- paste("Track", seq_len(nrow(playlist)))
  }

  if (verbose) {
    cat("\n===== Outlier Songs =====\n")
    cat(sprintf("Threshold: mean + %.1fx sd = %.4f\n", threshold_multiplier, threshold))

    if (sum(flagged) > 0) {
      idx <- which(flagged)
      idx <- idx[order(avg_dist[idx], decreasing = TRUE)] # worst first
      for (i in idx) {
        cat(sprintf(
          "  [row %2d] %-40s  avg_dist = %.4f\n",
          i, song_names[i], avg_dist[i]
        ))
      }
    } else {
      cat("No outliers found — playlist is cohesive.\n")
    }
  }

  # Enrich the original dataframe
  playlist$outlier_distance <- round(avg_dist, 4)
  playlist$is_outlier <- flagged

  invisible(playlist)
}
