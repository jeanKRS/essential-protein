# Improved Fuzzy Matching for Food Names (Vectorized)
# =====================================================
# Addresses issues like "potato" matching "tomato" better than "tomato"
# Properly handles vector inputs from fuzzy_join

library(dplyr)
library(fuzzyjoin)
library(stringdist)
library(stringr)

# Helper function: Calculate match score for single pair
# -------------------------------------------------------

calculate_match_score_single <- function(x, y) {
  # Handle NA or empty values
  if (is.na(x) || is.na(y) || x == "" || y == "") {
    return(0)
  }
  
  # Normalize strings
  x_clean <- tolower(gsub("\\s+", "", x))
  y_clean <- tolower(gsub("\\s+", "", y))
  
  # Handle empty strings after cleaning
  if (nchar(x_clean) == 0 || nchar(y_clean) == 0) {
    return(0)
  }
  
  # 1. Jaro-Winkler similarity (0-1, higher is better)
  jw_sim <- 1 - stringdist(x_clean, y_clean, method = "jw")
  
  # 2. Longest common substring similarity
  lcs_len <- stringdist(x_clean, y_clean, method = "lcs")
  max_len <- max(nchar(x_clean), nchar(y_clean))
  lcs_sim <- 1 - (lcs_len / max_len)
  
  # 3. Length similarity (penalize large differences)
  len_sim <- 1 - (abs(nchar(x_clean) - nchar(y_clean)) / max_len)
  
  # 4. Character overlap (Jaccard similarity)
  x_chars <- unique(strsplit(x_clean, "")[[1]])
  y_chars <- unique(strsplit(y_clean, "")[[1]])
  jaccard_sim <- length(intersect(x_chars, y_chars)) / length(union(x_chars, y_chars))
  
  # 5. Check for substring match (bonus)
  has_substring <- grepl(x_clean, y_clean, fixed = TRUE) | 
    grepl(y_clean, x_clean, fixed = TRUE)
  substring_bonus <- if (has_substring) 0.2 else 0
  
  # Weighted combination
  score <- (jw_sim * 0.35) + 
    (lcs_sim * 0.25) + 
    (len_sim * 0.20) + 
    (jaccard_sim * 0.20) + 
    substring_bonus
  
  return(score)
}

# Vectorized match score calculation
# -----------------------------------

calculate_match_score <- function(x, y) {
  # Ensure equal length vectors
  max_len <- max(length(x), length(y))
  x <- rep_len(x, max_len)
  y <- rep_len(y, max_len)
  
  # Apply to each pair
  scores <- numeric(max_len)
  for (i in seq_along(x)) {
    scores[i] <- calculate_match_score_single(x[i], y[i])
  }
  
  return(scores)
}


# Helper function: Word-level matching for single pair
# -----------------------------------------------------

word_level_match_single <- function(x, y, min_word_overlap = 0.6) {
  # Handle NA or empty
  if (is.na(x) || is.na(y) || x == "" || y == "") {
    return(FALSE)
  }
  
  # Split into words
  x_words <- tolower(unlist(strsplit(x, "\\s+")))
  y_words <- tolower(unlist(strsplit(y, "\\s+")))
  
  # Remove very short words
  x_words <- x_words[nchar(x_words) > 2]
  y_words <- y_words[nchar(y_words) > 2]
  
  if (length(x_words) == 0 || length(y_words) == 0) {
    return(FALSE)
  }
  
  # Calculate word overlap
  matching_words <- 0
  for (x_word in x_words) {
    for (y_word in y_words) {
      if (stringdist(x_word, y_word, method = "jw") <= 0.15) {
        matching_words <- matching_words + 1
        break
      }
    }
  }
  
  # Calculate overlap ratio
  overlap_ratio <- matching_words / max(length(x_words), length(y_words))
  
  return(overlap_ratio >= min_word_overlap)
}


# Main matching function for single pair
# ---------------------------------------

best_fuzzy_match_single <- function(x, y, 
                                    jw_threshold = 0.23,
                                    min_score = 0.70,
                                    min_word_overlap = 0.5) {
  
  # Handle NA or empty values
  if (is.na(x) || is.na(y) || x == "" || y == "") {
    return(FALSE)
  }
  
  # Normalize
  x_clean <- tolower(gsub("\\s+", "", x))
  y_clean <- tolower(gsub("\\s+", "", y))
  
  # Handle empty after cleaning
  if (nchar(x_clean) == 0 || nchar(y_clean) == 0) {
    return(FALSE)
  }
  
  # Rule 1: Exact match (case-insensitive)
  if (x_clean == y_clean) {
    return(TRUE)
  }
  
  # Rule 2: Exact substring match
  has_substring <- grepl(x_clean, y_clean, fixed = TRUE) | 
    grepl(y_clean, x_clean, fixed = TRUE)
  if (has_substring) {
    return(TRUE)
  }
  
  # Rule 3: Length difference too large (reject immediately)
  len_diff <- abs(nchar(x_clean) - nchar(y_clean))
  max_len <- max(nchar(x_clean), nchar(y_clean))
  if (len_diff / max_len > 0.5) {  # More than 50% length difference
    return(FALSE)
  }
  
  # Rule 4: Try word-level matching first (for multi-word strings)
  x_words <- unlist(strsplit(x, "\\s+"))
  y_words <- unlist(strsplit(y, "\\s+"))
  
  if (length(x_words) > 1 || length(y_words) > 1) {
    if (word_level_match_single(x, y, min_word_overlap)) {
      return(TRUE)
    }
  }
  
  # Rule 5: Calculate comprehensive match score
  score <- calculate_match_score_single(x, y)
  if (score >= min_score) {
    return(TRUE)
  }
  
  # Rule 6: Fall back to adjusted Jaro-Winkler with penalties
  base_dist <- stringdist(x_clean, y_clean, method = "jw")
  length_penalty <- (len_diff / max_len) * 0.15
  adjusted_dist <- base_dist + length_penalty
  
  return(adjusted_dist <= jw_threshold)
}


# MAIN VECTORIZED MATCHING FUNCTION
# ==================================
# This is what gets called by fuzzy_join

best_fuzzy_match <- function(x, y, 
                             jw_threshold = 0.23,
                             min_score = 0.70,
                             min_word_overlap = 0.5) {
  
  # Ensure equal length vectors
  max_len <- max(length(x), length(y))
  x <- rep_len(x, max_len)
  y <- rep_len(y, max_len)
  
  # Apply function to each pair
  results <- logical(max_len)
  for (i in seq_along(x)) {
    results[i] <- best_fuzzy_match_single(
      x[i], y[i], 
      jw_threshold, 
      min_score, 
      min_word_overlap
    )
  }
  
  return(results)
}


# Alternative simpler versions
# =============================

# Version 1: Length-penalized Jaro-Winkler
improved_string_distance <- function(x, y, max_dist = 0.23) {
  # Ensure equal length vectors
  max_len <- max(length(x), length(y))
  x <- rep_len(x, max_len)
  y <- rep_len(y, max_len)
  
  results <- logical(max_len)
  
  for (i in seq_along(x)) {
    if (is.na(x[i]) || is.na(y[i]) || x[i] == "" || y[i] == "") {
      results[i] <- FALSE
      next
    }
    
    # Remove spaces for comparison
    x_clean <- gsub("\\s+", "", x[i])
    y_clean <- gsub("\\s+", "", y[i])
    
    # Calculate base Jaro-Winkler distance
    base_dist <- stringdist(x_clean, y_clean, method = "jw")
    
    # Calculate length penalty
    len_x <- nchar(x_clean)
    len_y <- nchar(y_clean)
    len_diff <- abs(len_x - len_y)
    max_len_str <- max(len_x, len_y)
    
    # Penalize if length difference is substantial
    length_penalty <- (len_diff / max_len_str) * 0.15
    
    # Adjusted distance
    adjusted_dist <- base_dist + length_penalty
    
    results[i] <- adjusted_dist <= max_dist
  }
  
  return(results)
}


# Version 2: Substring priority matching
improved_string_distance_v3 <- function(x, y, max_dist = 0.23) {
  # Ensure equal length vectors
  max_len <- max(length(x), length(y))
  x <- rep_len(x, max_len)
  y <- rep_len(y, max_len)
  
  results <- logical(max_len)
  
  for (i in seq_along(x)) {
    if (is.na(x[i]) || is.na(y[i]) || x[i] == "" || y[i] == "") {
      results[i] <- FALSE
      next
    }
    
    # Normalize strings
    x_clean <- tolower(gsub("\\s+", "", x[i]))
    y_clean <- tolower(gsub("\\s+", "", y[i]))
    
    # Check for exact substring match (high priority)
    has_substring <- grepl(x_clean, y_clean, fixed = TRUE) | 
      grepl(y_clean, x_clean, fixed = TRUE)
    if (has_substring) {
      results[i] <- TRUE
      next
    }
    
    # Calculate base Jaro-Winkler distance
    base_dist <- stringdist(x_clean, y_clean, method = "jw")
    
    # Calculate length penalty
    len_diff <- abs(nchar(x_clean) - nchar(y_clean))
    max_len_str <- max(nchar(x_clean), nchar(y_clean))
    length_penalty <- (len_diff / max_len_str) * 0.15
    
    # Adjusted distance
    adjusted_dist <- base_dist + length_penalty
    
    results[i] <- adjusted_dist <= max_dist
  }
  
  return(results)
}


