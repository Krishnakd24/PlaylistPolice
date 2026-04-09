# This file contains Documenation for FEature selection and Harmonic Concepts and helper functions


#' Feature Selection and Weighting Strategy
#'
#' @description
#' The package uses default features and weights based on research in music
#' emotion recognition and human perception of Spotify audio features.
#'
#' \strong{Key principles:}
#' \itemize{
#'   \item Energy is strongly weighted due to its high correlation with perceived intensity.
#'   \item Valence is moderately weighted as it reflects emotional positivity.
#'   \item Danceability is down-weighted due to weaker reliability.
#'   \item Acousticness is emphasized based on its relevance in emotion classification.
#' }
#'
#' @details
#' Default feature weights:
#' \preformatted{
#' energy       = 2
#' valence      = 1.5
#' tempo        = 1
#' loudness     = 0.8
#' danceability = 0.6
#' speechiness  = 0.5
#' acousticness = 2
#' }
#'
#' These weights are informed by:
#' \itemize{
#'   \item Validation of Spotify features against human perception
#'   \item Feature importance studies in music emotion recognition
#' }
#'
#' @references
#' Panda et al. (2021) — Music Emotion Recognition using Spotify Features
#' Additional validation studies on Spotify feature reliability
#'
#' @name feature_weighting
NULL

#' @keywords internal
default_features <- c(
  "energy", "valence", "tempo", "loudness",
  "danceability", "speechiness", "acousticness"
)


#' @keywords internal
default_weights <- c(
  energy       = 2,
  valence      = 1.5,
  tempo        = 1,
  loudness     = 0.8,
  danceability = 0.6,
  speechiness  = 0.5,
  acousticness = 2
)


#' @keywords internal
default_k <- 7




# Purpose : map user-supplied feature names to the actual column names in the
#           playlist data frame, ignoring case differences.
#' @keywords internal
resolve_feature_names <- function(playlist, features) {
  if (!is.data.frame(playlist)) {
    stop("playlist must be a data.frame")
  }
  if (length(features) == 0) {
    stop("features must contain at least one column name")
  }

  playlist_names_lower <- tolower(names(playlist))
  resolved <- character(length(features))

  for (i in seq_along(features)) {
    feature_lower <- tolower(features[i])
    idx <- which(playlist_names_lower == feature_lower)
    if (length(idx) == 0) {
      stop(sprintf("Feature '%s' not found in playlist.", features[i]))
    }
    resolved[i] <- names(playlist)[idx[1]]
  }
  resolved
}


# Purpose : turn raw feature columns into a weighted, z-score-scaled numeric
#           matrix that is ready for distance calculations.
#' @keywords internal
prepare_scaled_features <- function(playlist, features, weights = NULL) {
  resolved_features <- resolve_feature_names(playlist, features)

  if (nrow(playlist) == 0) {
    stop("playlist has no rows")
  }

  feature_df <- playlist[, resolved_features, drop = FALSE]

  # Step 0 — coerce to numeric and mean-impute NAs
  for (col_name in names(feature_df)) {
    if (!is.numeric(feature_df[[col_name]])) {
      feature_df[[col_name]] <- as.numeric(feature_df[[col_name]])
    }

    if (all(is.na(feature_df[[col_name]]))) {
      stop(sprintf("Feature '%s' is entirely NA — cannot use it.", col_name))
    }

    if (anyNA(feature_df[[col_name]])) {
      col_mean <- mean(feature_df[[col_name]], na.rm = TRUE)
      feature_df[[col_name]][is.na(feature_df[[col_name]])] <- col_mean
    }
  }
  # Step 1 — z-score scale: each feature → mean 0, sd 1
  # scale() returns a matrix; replace any non-finite values (e.g. sd = 0) with 0
  scaled <- scale(as.matrix(feature_df))
  scaled[!is.finite(scaled)] <- 0

  # Step 2 — apply weights AFTER scaling
  # weight_vec picks the right weight for each column; defaults to 1.0 if absent
  if (!is.null(weights)) {
    feature_names_lower <- tolower(colnames(scaled))
    names_weights_lower <- tolower(names(weights))
    weight_vec <- rep(1.0, ncol(scaled))

    for (i in seq_along(feature_names_lower)) {
      m <- which(names_weights_lower == feature_names_lower[i])
      if (length(m) > 0) weight_vec[i] <- weights[m[1]]
    }

    # Broadcast weight_vec across all rows
    scaled <- scaled * matrix(weight_vec,
      nrow = nrow(scaled),
      ncol = ncol(scaled), byrow = TRUE
    )
  }

  list(matrix = scaled, resolved_features = resolved_features)
}

#' Harmonic Similarity and Circular Key Distance
#'
#' @description
#' This page describes how harmonic relationships between tracks are modeled
#' using key and mode information.
#'
#' @details
#' \strong{Harmonic Similarity:}
#' Musical harmony is determined by relationships between keys. Tracks with
#' compatible keys tend to sound more cohesive when played together.
#'
#' \strong{Circle of Fifths and Camelot System:}
#' Musical keys can be arranged in a circular structure (e.g., the circle of fifths
#' or the Camelot Wheel), where neighboring positions represent harmonically
#' compatible keys.
#'
#' In the Camelot system, keys are mapped onto a 12-position circular scale,
#' allowing harmonic relationships to be treated as distances on a circle.
#'
#' \strong{Distance Computation:}
#' Harmonic distance between two tracks is computed as the shortest circular
#' distance between their positions on this 12-point scale:
#'
#' \deqn{d(a, b) = \min(|a - b|, 12 - |a - b|)}
#'
#' This ensures that wrap-around relationships (e.g., 1 and 12) are treated as close.
#'
#' The distance is then normalized:
#'
#' \deqn{d_{norm} = \frac{d}{6}}
#'
#' yielding values in the range [0, 1].
#'
#' \strong{Harmonic Weight:}
#' The harmonic weight acts as a regularization parameter balancing feature similarity and musical key compatibility.
#' Low harmonic weight (≈0.5) prioritizes feature similarity
#' High harmonic weight (2–3) emphasizes key compatibility, producing smoother, DJ-style transitions at the cost of reduced variety.
#'
#'
#' \strong{Usage in the model:}
#' This harmonic distance is used as a penalty term in similarity computations,
#' complementing feature-based distances such as energy and valence.
#'
#' @references
#' Concepts derived from the circle of fifths and harmonic mixing practices in DJing.
#' 
#' @name harmonic_concepts
NULL
#' @keywords internal
camelot_number <- function(key, mode) {
  # Camelot numbers indexed by pitch class (position 1 = C, 2 = C#, …, 12 = B)
  major_camelot <- c(8, 3, 10, 5, 12, 7, 2, 9, 4, 11, 6, 1)
  minor_camelot <- c(5, 12, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10)

  key_idx <- (as.integer(key) %% 12L) + 1L # ensure 1-based index
  ifelse(mode == 1, major_camelot[key_idx], minor_camelot[key_idx])
}


# Purpose : build an N×N matrix of pairwise harmonic distances based on
#           Camelot wheel positions, normalised to [0, 1].
#' @keywords internal
harmonic_distance_matrix <- function(playlist) {
  col_names_lower <- tolower(names(playlist))
  has_key <- "key" %in% col_names_lower
  has_mode <- "mode" %in% col_names_lower
  if (!has_key || !has_mode) {
    return(NULL)
  }

  key_col <- names(playlist)[col_names_lower == "key"][1]
  mode_col <- names(playlist)[col_names_lower == "mode"][1]

  keys <- playlist[[key_col]]
  modes <- playlist[[mode_col]]

  # Default missing values to C major
  keys[is.na(keys)] <- 0L
  modes[is.na(modes)] <- 1L

  camelot <- camelot_number(keys, modes)

  # outer() computes all pairs; pmin picks the shorter arc on the 12-position wheel

  mat <- outer(camelot, camelot, FUN = function(a, b) {
    d <- abs(a - b)
    pmin(d, 12L - d)
  })

  mat / 6 # normalise: max distance (6) → 1.0
}


# Purpose : calculate the total transition cost for a given song ordering.
#' @keywords internal
compute_order_cost <- function(scaled_matrix, order_idx,
                               harm_matrix = NULL, harm_weight = 0) {
  n <- length(order_idx)
  if (n <= 1) {
    return(0)
  }

  # Vectorised: compute all consecutive-pair differences at once (no loop)
  diffs <- scaled_matrix[order_idx[-1], , drop = FALSE] -
    scaled_matrix[order_idx[-n], , drop = FALSE]

  # SQUARED Euclidean: sum of squared diffs per row, then sum all rows
  sq_euclidean_cost <- sum(rowSums(diffs^2))

  # Add optional Camelot wheel penalty if enabled
  if (!is.null(harm_matrix) && harm_weight > 0) {
    pairs <- cbind(order_idx[-n], order_idx[-1]) # N-1 consecutive pairs
    harm_cost <- sum(harm_matrix[pairs]) * harm_weight
    return(sq_euclidean_cost + harm_cost)
  }

  sq_euclidean_cost
}
