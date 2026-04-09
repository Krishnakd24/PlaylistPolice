#' Vibe Classification
#'
#' @description
#' Classifies each track in a playlist into "Mellow", "Groove", or "Intense"
#' using a pre-trained k-nearest neighbors (k-NN) model and summarizes the distribution.
#'
#' @param playlist data.frame containing song metadata and audio features
#' @param k integer; number of nearest neighbors to use in k-NN classification (default:7)
#' @param harmonic_weight Weight for Camelot wheel penalty. Recommend 0 to disable, 0.5 for moderate penalty, or 1.0 for strict.(default:0.5)
#' @param verbose logical; whether to print progress to console (default: FALSE)
#' @details
#' This function applies a pre-trained k-nearest neighbors (k-NN) classifier
#' to assign each track to one of three categories based on audio features.
#'
#' \strong{Step 1 — Feature alignment:}
#' Input features are matched to the feature set used during model training.
#'
#' \strong{Step 2 — Scaling and weighting:}
#' Features are standardized using training-set statistics (mean and standard deviation),
#' then scaled using pre-defined feature weights stored in the model.
#'
#' \strong{Step 3 — Distance computation:}
#' Euclidean distance is computed between the input track and all training samples:
#'
#' \deqn{d(i,j) = \|x_i - x_j\|}
#'
#' If harmonic information is available, an additional penalty is applied:
#'
#' \deqn{d_{total} = d + \lambda \cdot d_{harmonic}}
#'
#' See \link{harmonic_concepts}.
#'
#' \strong{Step 4 — k-NN classification:}
#' The k nearest neighbors are selected, and the most frequent category is assigned.
#' In case of ties, the category with the lowest average distance is chosen.
#'
#' \strong{Step 5 — Aggregation:}
#' The function returns both per-track classifications and a summary of category distribution.
#' @seealso \link{feature_weighting}, \link{harmonic_concepts}
#' @return
#' A list containing:
#' \itemize{
#'   \item \code{summary} — Data frame with category counts and percentages
#'   \item \code{classified_playlist} — Input playlist with an added \code{category} column
#' }
#' @examples
#' songs <- read.csv(system.file("extdata", "songs.csv", package = "PlaylistPolice"))
#' result <- summarize_playlist(songs)
#' print(result$summary)
#'
#' @export
summarize_playlist <- function(playlist,
                               k = default_k,
                               harmonic_weight = 0.5,
                               verbose = FALSE) {
  if (nrow(playlist) == 0) stop("playlist has no rows")

  # Use bundled model by default
  model_path <- system.file("extdata", "knn_model.rds", package = "PlaylistPolice")

  # If bundled model doesn't exist (package not installed), try working directory
  if (!nzchar(model_path) || !file.exists(model_path)) {
    model_path <- "knn_model.rds"
  }

  model <- load_playlist_model(model_path)

  if (verbose) message(sprintf("Classifying %d songs (k = %d)...", nrow(playlist), k))

  categories <- vapply(seq_len(nrow(playlist)), function(i) {
    sv <- playlist[i, , drop = FALSE]
    classify_song_knn_model(sv, model, k = k, harmonic_weight = harmonic_weight)
  }, character(1))

  classified <- playlist
  classified$category <- categories
  rownames(classified) <- NULL

  counts <- table(factor(categories, levels = c("Mellow", "Groove", "Intense")))
  total <- nrow(playlist)

  list(
    summary = data.frame(
      category = c("Mellow", "Groove", "Intense"),
      count = as.integer(counts),
      percentage = round(100 * as.integer(counts) / total, 1),
      stringsAsFactors = FALSE
    ),
    classified_playlist = classified
  )
}
