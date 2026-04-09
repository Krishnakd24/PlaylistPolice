#The following code is used to train the model and classify songs into categories



# ── build_training_set ────────────────────────────────────────────────────────
#
# Purpose : load and combine the three labeled category CSV files into a single
#           training data frame with a "category" column.
#' @keywords internal
build_training_set <- function(mellow_file = NULL,
                               groove_file = NULL,
                               intense_file = NULL) {
  # Use bundled package data by default
  if (is.null(mellow_file)) {
    mellow_file <- system.file("extdata", "Mellow.csv", package = "PlaylistPolice")
  }
  if (is.null(groove_file)) {
    groove_file <- system.file("extdata", "Groove.csv", package = "PlaylistPolice")
  }
  if (is.null(intense_file)) {
    intense_file <- system.file("extdata", "Intense.csv", package = "PlaylistPolice")
  }

  # ── File existence check ───────────────────────────────────────────────────
  if (!nzchar(mellow_file) || !file.exists(mellow_file)) {
    stop(sprintf("Mellow file not found: %s", mellow_file))
  }
  if (!nzchar(groove_file) || !file.exists(groove_file)) {
    stop(sprintf("Groove file not found: %s", groove_file))
  }
  if (!nzchar(intense_file) || !file.exists(intense_file)) {
    stop(sprintf("Intense file not found: %s", intense_file))
  }

  mellow <- read.csv(mellow_file, stringsAsFactors = FALSE)
  groove <- read.csv(groove_file, stringsAsFactors = FALSE)
  intense <- read.csv(intense_file, stringsAsFactors = FALSE)

  # ── Non-empty check ────────────────────────────────────────────────────────
  if (nrow(mellow) == 0) stop("Mellow.csv has no rows")
  if (nrow(groove) == 0) stop("Groove.csv has no rows")
  if (nrow(intense) == 0) stop("Intense.csv has no rows")

  # ── Attach category labels and merge ───────────────────────────────────────
  mellow$category <- "Mellow"
  groove$category <- "Groove"
  intense$category <- "Intense"

  common_cols <- Reduce(intersect, list(names(mellow), names(groove), names(intense)))

  if (length(common_cols) == 0) {
    stop("Training files have no columns in common.")
  }

  training <- rbind(
    mellow[, common_cols],
    groove[, common_cols],
    intense[, common_cols]
  )

  # ── Verify required audio features are present ─────────────────────────────
  tryCatch(
    resolve_feature_names(training, default_features),
    error = function(e) {
      stop(sprintf("Training data missing required features: %s", e$message))
    }
  )

  rownames(training) <- NULL
  training
}


# ── train_and_save_model ─────────────────────────────────────────────────────
#' @keywords internal
train_and_save_model <- function(mellow_file = NULL,
                                 groove_file = NULL,
                                 intense_file = NULL,
                                 features = default_features,
                                 weights = default_weights,
                                 model_path = "knn_model.rds") {
  training_data <- build_training_set(mellow_file, groove_file, intense_file)

  # Pre-compute the scaled matrix and the training statistics once
  prepared <- prepare_scaled_features(training_data, features, weights = weights)
  feature_df <- training_data[, prepared$resolved_features, drop = FALSE]

  # Coerce & impute so stats are clean
  for (col in names(feature_df)) {
    feature_df[[col]] <- as.numeric(feature_df[[col]])
    mu <- mean(feature_df[[col]], na.rm = TRUE)
    feature_df[[col]][is.na(feature_df[[col]])] <- mu
  }

  model <- list(
    scaled_matrix     = prepared$matrix, # [n_train x n_features], weighted
    labels            = training_data$category,
    resolved_features = prepared$resolved_features,
    train_means       = colMeans(feature_df),
    train_sds         = apply(feature_df, 2, sd),
    weights           = weights,
    trained_at        = Sys.time()
  )

  # Support harmonic testing if keys exist
  col_names_lower <- tolower(names(training_data))
  if ("key" %in% col_names_lower && "mode" %in% col_names_lower) {
    k_col <- names(training_data)[col_names_lower == "key"][1]
    m_col <- names(training_data)[col_names_lower == "mode"][1]
    model$training_camelot <- camelot_number(training_data[[k_col]], training_data[[m_col]])
  }

  saveRDS(model, model_path)
  message(sprintf(
    "Model saved to '%s'  (%d songs, %d features)",
    model_path, nrow(training_data), length(prepared$resolved_features)
  ))
  invisible(model)
}


# ── 2. Load the saved model ───────────────────────────────────────────────────
#' @keywords internal
load_playlist_model <- function(model_path = "knn_model.rds") {
  if (!file.exists(model_path)) {
    stop(sprintf("Model file not found: %s\nRun train_and_save_model() first.", model_path))
  }
  readRDS(model_path)
}


# ── 3. Classify a single song against the pre-built model ────────────────────
#' @keywords internal
classify_song_knn_model <- function(test_song, model, k = default_k, harmonic_weight = 0.5) {
  rf <- model$resolved_features
  n <- length(rf)

  # Extract test features (name-matched, case-insensitive)
  test_vec <- numeric(n)
  names_lower <- if (!is.null(names(test_song))) tolower(names(test_song)) else character(0)

  for (i in seq_len(n)) {
    fn <- rf[i]
    if (fn %in% names(test_song)) {
      test_vec[i] <- as.numeric(test_song[[fn]])
    } else {
      m <- which(names_lower == tolower(fn))
      if (length(m)) {
        test_vec[i] <- as.numeric(test_song[[m[1]]])
      } else {
        test_vec[i] <- NA_real_
      }
    }
  }

  # Mean-impute using stored training means
  na_idx <- which(is.na(test_vec))
  if (length(na_idx)) test_vec[na_idx] <- model$train_means[na_idx]

  # Z-score using stored training stats, then weight
  test_scaled <- (test_vec - model$train_means) /
    pmax(model$train_sds, 1e-10)
  test_scaled[!is.finite(test_scaled)] <- 0

  if (!is.null(model$weights)) {
    wn <- tolower(names(model$weights))
    fn <- tolower(names(model$train_means))
    wv <- rep(1.0, n)
    for (i in seq_len(n)) {
      m <- which(wn == fn[i])
      if (length(m)) wv[i] <- model$weights[m[1]]
    }
    test_scaled <- test_scaled * wv
  }

  # Euclidean distances against the pre-scaled training matrix
  diffs <- model$scaled_matrix -
    matrix(test_scaled,
      nrow = nrow(model$scaled_matrix),
      ncol = n, byrow = TRUE
    )
  distances <- rowSums(diffs^2)

  if (!is.null(model$training_camelot) && harmonic_weight > 0) {
    col_names_lower <- tolower(names(test_song))

    if ("key" %in% col_names_lower && "mode" %in% col_names_lower) {
      tkey <- test_song[[names(test_song)[col_names_lower == "key"][1]]]
      tmode <- test_song[[names(test_song)[col_names_lower == "mode"][1]]]

      if (!is.null(tkey) && !is.null(tmode) && !is.na(tkey) && !is.na(tmode)) {
        tcam <- camelot_number(tkey, tmode)
        d <- abs(model$training_camelot - tcam)
        harm_dist <- pmin(d, 12L - d) / 6.0
        distances <- distances + (harm_dist * harmonic_weight)
      }
    }
  }

  # k-NN vote
  k_eff <- min(as.integer(k), length(distances))
  idx <- order(distances)[seq_len(k_eff)]
  nb_cats <- model$labels[idx]
  nb_dists <- distances[idx]

  counts <- table(factor(nb_cats, levels = c("Mellow", "Groove", "Intense")))
  max_v <- max(counts)
  tied <- names(counts)[counts == max_v]

  if (length(tied) == 1) {
    return(tied)
  }

  # Tie-break by mean distance
  avg_d <- sapply(tied, function(cat) mean(nb_dists[nb_cats == cat], na.rm = TRUE))
  names(avg_d)[which.min(avg_d)][1]
}
