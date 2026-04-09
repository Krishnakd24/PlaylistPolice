# PlaylistPolice 

## Introduction
Creating the perfect playlist is more than just throwing your favorite songs into a list. **PlaylistPolice** is an R package designed to mathematically analyze playlists, detect audio outliers that break the "vibe", optimize track sequencing for smooth transitions, and evaluate music compatibility across multiple playlists using audio features (e.g., energy, valence, tempo, danceability, etc).

### The Problem
Whether you are a DJ preparing a set, a fitness instructor planning a workout class, or just someone curating vibes for a road trip, a poorly put together playlist can ruin the mood. Jumps in energy, tempo, or musical key can cause jarring transitions. Curating large playlists manually requires deep music theory knowledge (like harmonic mixing) and hours of rearranging tracks to find a decent flow.

### Our Solution
**PlaylistPolice** automates the heavy lifting of professional playlist curation. By utilizing nearest-neighbor heuristics, k-Nearest Neighbors classification, and harmonic distance penalties, it mathematically enforces musical continuity. 

With this package, you can:
- **Cleanse Playlists**: Automatically find and remove songs that don't match the overall mood.
- **Sequence Seamlessly**: Order tracks so that energy levels and musical keys flow naturally from one song to the next.
- **Understand the Vibe**: Classify playlists into broader moods (Mellow, Groove, Intense).
- **Match Playlists**: Calculate how compatible two separate playlists are for mixing.

---

## User Functions
### 1. Identify Outlier Songs (`get_outlier_songs`)
**Location:** [`R/user_functions.R`](R/user_functions.R)

Identifies tracks that mathematically distant from the overall "vibe" of a curated playlist. It operates by calculating the mean squared distance of each song to every other song in the playlist. Songs whose average distance exceeds the threshold (`mean + threshold_multiplier * sd`) are flagged.
- Calculates an initial Euclidean mean-squared distance against playlist peers.
- Injects a continuous **Harmonic Distance penalty** (using a `harmonic_weight` coefficient) if the songs differ melodically on the Camelot wheel.
- **Returns:** The original dataframe with `outlier_distance` and `is_outlier` columns appended.

### 2. Sequence Playlists (`get_optimal_sequence`)
**Location:** [`R/user_functions2.R`](R/user_functions2.R)

Orders a playlist so that consecutive songs flow smoothly into one another. It finds an optimal track progression using a **nearest-neighbour greedy heuristic**, mirroring Traveling Salesperson Problem (TSP) pathfinding methodologies.
- Incorporates a **Harmonic penalty** utilizing the **Camelot Wheel** configuration. If `key` and `mode` data columns exist, it restricts tracks from placing side-by-side if they are musically dissonant via continuous weighting.
- **Returns:** A sequenced playlist object detailing the initial structure and the sorted result.

### 3. Vibe Classification (`summarize_playlist`)
**Location:** [`R/user_functions3.R`](R/user_functions3.R)

Classifies each song within the playlist into three broad categories: **Mellow**, **Groove**, or **Intense**. It provides a holistic perspective on the playlist's thematic makeup.
- Employs a pre-trained **k-Nearest Neighbors (k-NN)** machine learning model.
- Includes backward-compatible on-the-fly harmonic variance comparisons so the k-NN algorithm factors in how musically similar the test tracks are to the labeled database.
- **Returns:** A summary dataframe with mood category distributions and percentages, along with the categorized playlist.

### 4. Compatibility Score (`check_playlist_compatibility`)
**Location:** [`R/user_functions4.R`](R/user_functions4.R)

Given two separated playlists, calculates an integration score determining how musically compatible they are for intertwining.
- Extracts `cross_playlist_distance` and evaluates it relative against the `within_playlist_distance` (the baseline cluster variation), alongside identical harmonic distance mapping.
- **Returns:** A `compatibility_score` (0-100 index rating), `mean_cross_distance`, and `mean_within_distance`.

---

## Deep Dive: Internal Package Logic & Helper Modules

Below is extensive documentation detailing literally every low-level computing script operating under the hood inside the workspace.

### A. The Helper Infrastructure (`R/helper.R`)
This module has the core math and feature pipelines.

1. **`resolve_feature_names(playlist, features)`** 
   - A safety tool that ensures user-supplied feature strings are mapped to actual dataframe columns while automatically ignoring case typing differences.

2. **`prepare_scaled_features(playlist, features, weights = NULL)`**
   - The foundation of the package. It puts `NA` missing values against column means, and transforms raw arrays out into **z-scores** (`scale()` setting `mean=0`, `sd=1`). After standardized scaling, it multiplies the values by the appropriate `weights` argument to return weighted z-scores. 

3. **`camelot_number(key, mode)`**
  -Musical tool to map mode and keys into the Camelot wheel (the DJ mixing standard format). 



4. **`harmonic_distance_matrix(playlist)`**
   - Builds an $N \times N$ matrix. Determines precisely how many clockwise/anti-clockwise steps away two keys are on the Camelot ring `pmin(d, 12L - d)`. It normalizes all outputs to `[0, 1]` metric bounding to stack linearly with Euclidean variances!
  
5. **`compute_order_cost(scaled_matrix, order_idx, harm_matrix, harm_weight)`**
   - A perfectly vectorized operation querying transition costs sequentially across mapped arrays utilizing Euclidean array variance differences (`rowSums(diffs^2)`) padded with linear harmonic adjustments.

### B. Machine Learning Engine Output (`R/Summ_Model.R`)
Controls the training arrays underlying all classification (`summarize_playlist`).

1. **`build_training_set(mellow_file, groove_file, intense_file)`**
   - Merges categorical datasets (`inst/extdata`), tagging datasets automatically into categorical boundaries and restricting bounds directly into uniform intersecting column frameworks.
2. **`train_and_save_model(...)`**
   - Compiles global standard-deviations (`train_sds`) and averages (`train_means`) from the training datasets statically! It generates and stores a packaged memory framework including base key elements and writes them robustly into `knn_model.rds`.
3. **`load_playlist_model(model_path)`**
   - A simple deployment script to catch missing `.rds` installations and pull active classification memory sets safely upon request.
4. **`classify_song_knn_model(test_song, model, k, harmonic_weight)`**
   - Evaluates a single track. It maps `test_song` parameters statically against the **pre-trained framework sets**. It dynamically imputes missing `test_song` properties utilizing `model$train_means`, scales specifically over `model$train_sds`, calculates test Camelot distances, processes distances across the total model `scaled_matrix`, and returns majority KNN votes.

### C. Advanced Algorithm Implementations (`R/Sequencer.R`)
Provides TSP structural tracking for the `get_optimal_sequence` orchestrator to execute against.

1. **`nearest_neighbor_sequence(playlist, features, weights, start_idx, harm_matrix, harm_weight)`**
   - Manages track sorting mathematically. Taking `start_idx` as an absolute point, it pulls active Euclidean node structures and aggressively evaluates the shortest available array vector out of the remaining `unvisited` pools, sequentially looping logic until arrays close! 

---

## Package Defaults
- **Features Analyzed:** `energy`, `valence`, `tempo`, `loudness`, `danceability`, `speechiness`, `acousticness`
- **Default Parameter Configuration:**
  - `harmonic_weight = 0.5`: Applies moderate harmonic penalties to metric algorithms dynamically across all package endpoints.
  - `k = 7`: Neighborhood volume mapping.

---
## 📚 Research Underpinnings

The statistical architectures and default parameters defined within the models are scientifically supported by major workflows in Music Emotion Recognition (MER):

1. **Feature Weighting (Panda et al. 2021)**: The `default_weights` mapping prioritizes core dimensions of human auditory perception metrics:
   - **Energy (2.0)** & **Acousticness (2.0)**: Strongly weighted due to their high correlation with perceived psychological intensity and broad emotion classification.
   - **Valence (1.5)**: Moderately weighted as it reflects emotional positivity.
   - **Danceability (0.6)**: Down-weighted due to weaker mathematical reliability when tracking global playlist vibes.

2. **Harmonic Distance & The Camelot System**: Harmonic compatibility explicitly defines structural continuity using the Circle of Fifths. Utilizing the 12-point Camelot Scale, it derives circular similarity via `d = min(|a - b|, 12 - |a - b|) / 6`, perfectly simulating tracking formats used in DJ automation procedures.

3. **Array Sequencing Routes**: Nearest-neighbor pathfinding fundamentally mirrors standard heuristic simplifications to the Traveling Salesperson Problem (TSP), forcing tracks to generate seamless structural feature gradients without encountering polynomial run-time boundaries.
