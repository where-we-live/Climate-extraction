model_climate_point_data_pca_clustering <- function(question, k = 3, var_target = 0.80) {
  # pkgs
  library(dplyr)
  library(ggplot2)
  library(grid)       # for unit()
  library(patchwork)  # for side-by-side layout
  
  set.seed(123)
  
  # --- load & assemble data ---
  dir_path <- "./data/delta"
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  dat_list <- lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE))
  df <- do.call(rbind, dat_list)
  
  # choose delta column from question input (e.g., "que_6" -> "que_6_delta")
  delta_col <- paste0(question, "_delta")
  needed <- c("Education", "Occupation", "Duration", "age", delta_col)
  stopifnot(all(needed %in% names(df)))
  
  Data1 <- df %>% dplyr::select(dplyr::all_of(needed))
  Data1[] <- lapply(Data1, function(z) suppressWarnings(as.numeric(z)))
  
  # keep rows with complete numeric data; align Location
  mask <- complete.cases(Data1)
  X <- Data1[mask, , drop = FALSE]
  city <- if ("Location" %in% names(df)) factor(df$Location[mask]) else NULL
  if (nrow(X) < 3) stop("Not enough complete rows after filtering.")
  
  # --- PCA ---
  pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
  ve <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  M <- suppressWarnings(max(2, which(cumsum(ve) >= var_target)[1]))
  if (!is.finite(M)) M <- min(2, ncol(X))
  
  # --- clustering on first M PCs ---
  S <- scale(pca_result$x[, 1:M, drop = FALSE])
  set.seed(123)
  km <- kmeans(S, centers = k, nstart = 50)
  clusters <- factor(km$cluster)
  
  # --- scores for plotting ---
  scores <- as.data.frame(pca_result$x[, 1:2, drop = FALSE])
  names(scores) <- c("PC1", "PC2")
  scores$cluster <- clusters
  if (!is.null(city)) scores$city <- city
  
  # --- loadings scaled to score space for arrows ---
  L <- as.data.frame(pca_result$rotation[, 1:2, drop = FALSE])
  names(L) <- c("PC1", "PC2")
  L$var <- rownames(L)
  
  rng <- function(z) diff(range(z))
  arrow_scale <- 0.7 * min(rng(scores$PC1) / rng(L$PC1),
                           rng(scores$PC2) / rng(L$PC2))
  Lx <- transform(L, PC1 = PC1 * arrow_scale, PC2 = PC2 * arrow_scale)
  
  lab_x <- sprintf("PC1 (%.1f%%)", 100 * ve[1])
  lab_y <- sprintf("PC2 (%.1f%%)", 100 * ve[2])
  ttl  <- paste0("PCA biplot: ", question, "_delta")
  
  # --- p: colored by cluster ---
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = 3) +
    geom_vline(xintercept = 0, linetype = 3) +
    geom_point(data = scores, aes(PC1, PC2, color = cluster), alpha = 0.75) +
    stat_ellipse(data = scores, aes(PC1, PC2, color = cluster),
                 level = 0.68, linewidth = 0.6, linetype = 2, show.legend = FALSE) +
    geom_segment(data = Lx, aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.02, "npc"))) +
    geom_text(data = Lx, aes(PC1, PC2, label = var), vjust = -0.4, size = 3) +
    labs(x = lab_x, y = lab_y, title = paste0(ttl, " — by Cluster"), color = "Cluster") +
    theme_minimal()
  
  # --- p2: colored by city (falls back to gray if Location missing) ---
  if (!is.null(city)) {
    p2 <- ggplot() +
      geom_hline(yintercept = 0, linetype = 3) +
      geom_vline(xintercept = 0, linetype = 3) +
      geom_point(data = scores, aes(PC1, PC2, color = city), alpha = 0.75) +
      stat_ellipse(data = scores, aes(PC1, PC2, color = city),
                   level = 0.68, linewidth = 0.6, linetype = 2, show.legend = FALSE) +
      geom_segment(data = Lx, aes(x = 0, y = 0, xend = PC1, yend = PC2),
                   arrow = arrow(length = unit(0.02, "npc"))) +
      geom_text(data = Lx, aes(PC1, PC2, label = var), vjust = -0.4, size = 3) +
      labs(x = lab_x, y = lab_y, title = paste0(ttl, " — by City"), color = "City") +
      theme_minimal()
  } else {
    p2 <- ggplot(scores, aes(PC1, PC2)) +
      geom_point(alpha = 0.75) +
      labs(x = lab_x, y = lab_y, title = paste0(ttl, " — (no Location column)")) +
      theme_minimal()
  }
  
  combined <- p | p2
  
  list(
    pca = pca_result,
    kmeans = km,
    pcs_used_for_clustering = M,
    var_explained = ve,
    scores_df = scores,
    loadings_df = L,
    plot_cluster = p,
    plot_city = p2,
    plot_both = combined
  )
}



# 
# model_climate_point_data_pca_clustering <- function(question, k = 3, var_target = 0.80) {
#   # pkgs
#   library(dplyr)
#   library(ggplot2)
#   library(grid)       # for unit()
#   library(patchwork)  # for side-by-side layout
#   
#   set.seed(123)
#   
#   # --- load & assemble data ---
#   dir_path <- "./data/delta"
#   files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
#   dat_list <- lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE))
#   df <- do.call(rbind, dat_list)
#   
#   # choose delta column from question input (e.g., "que_6" -> "que_6_delta")
#   delta_col <- paste0(question, "_delta")
#   needed <- c("Education", "Occupation", "Duration", "age", delta_col)
#   stopifnot(all(needed %in% names(df)))
#   
#   Data1 <- df %>% dplyr::select(dplyr::all_of(needed))
#   Data1[] <- lapply(Data1, function(z) suppressWarnings(as.numeric(z)))
#   
#   # keep rows with complete numeric data; align Location
#   mask <- complete.cases(Data1)
#   X <- Data1[mask, , drop = FALSE]
#   city <- if ("Location" %in% names(df)) factor(df$Location[mask]) else NULL
#   if (nrow(X) < 3) stop("Not enough complete rows after filtering.")
#   
#   # --- PCA ---
#   pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
#   ve <- pca_result$sdev^2 / sum(pca_result$sdev^2)
#   M <- suppressWarnings(max(2, which(cumsum(ve) >= var_target)[1]))
#   if (!is.finite(M)) M <- min(2, ncol(X))
#   
#   # --- clustering on first M PCs ---
#   S <- scale(pca_result$x[, 1:M, drop = FALSE])
#   set.seed(123)
#   km <- kmeans(S, centers = k, nstart = 50)
#   clusters <- factor(km$cluster)
#   
#   # --- scores for plotting ---
#   scores <- as.data.frame(pca_result$x[, 1:2, drop = FALSE])
#   names(scores) <- c("PC1", "PC2")
#   scores$cluster <- clusters
#   if (!is.null(city)) scores$city <- city
#   
#   # --- loadings scaled to score space for arrows ---
#   L <- as.data.frame(pca_result$rotation[, 1:2, drop = FALSE])
#   names(L) <- c("PC1", "PC2")
#   L$var <- rownames(L)
#   
#   rng <- function(z) diff(range(z))
#   arrow_scale <- 0.7 * min(rng(scores$PC1) / rng(L$PC1),
#                            rng(scores$PC2) / rng(L$PC2))
#   Lx <- transform(L, PC1 = PC1 * arrow_scale, PC2 = PC2 * arrow_scale)
#   
#   lab_x <- sprintf("PC1 (%.1f%%)", 100 * ve[1])
#   lab_y <- sprintf("PC2 (%.1f%%)", 100 * ve[2])
#   ttl  <- paste0("PCA biplot: ", question, "_delta")
#   
#   # --- p: colored by cluster ---
#   p <- ggplot() +
#     geom_hline(yintercept = 0, linetype = 3) +
#     geom_vline(xintercept = 0, linetype = 3) +
#     geom_point(data = scores, aes(PC1, PC2, color = cluster), alpha = 0.75) +
#     stat_ellipse(data = scores, aes(PC1, PC2, color = cluster),
#                  level = 0.68, linewidth = 0.6, linetype = 2, show.legend = FALSE) +
#     geom_segment(data = Lx, aes(x = 0, y = 0, xend = PC1, yend = PC2),
#                  arrow = arrow(length = unit(0.02, "npc"))) +
#     geom_text(data = Lx, aes(PC1, PC2, label = var), vjust = -0.4, size = 3) +
#     labs(x = lab_x, y = lab_y, title = paste0(ttl, " — by Cluster"), color = "Cluster") +
#     theme_minimal()
#   
#   # --- p2: colored by city (falls back to gray if Location missing) ---
#   if (!is.null(city)) {
#     p2 <- ggplot() +
#       geom_hline(yintercept = 0, linetype = 3) +
#       geom_vline(xintercept = 0, linetype = 3) +
#       geom_point(data = scores, aes(PC1, PC2, color = city), alpha = 0.75) +
#       stat_ellipse(data = scores, aes(PC1, PC2, color = city),
#                    level = 0.68, linewidth = 0.6, linetype = 2, show.legend = FALSE) +
#       geom_segment(data = Lx, aes(x = 0, y = 0, xend = PC1, yend = PC2),
#                    arrow = arrow(length = unit(0.02, "npc"))) +
#       geom_text(data = Lx, aes(PC1, PC2, label = var), vjust = -0.4, size = 3) +
#       labs(x = lab_x, y = lab_y, title = paste0(ttl, " — by City"), color = "City") +
#       theme_minimal()
#   } else {
#     p2 <- ggplot(scores, aes(PC1, PC2)) +
#       geom_point(alpha = 0.75) +
#       labs(x = lab_x, y = lab_y, title = paste0(ttl, " — (no Location column)")) +
#       theme_minimal()
#   }
#   
#   # --- side-by-side output ---
#   combined <- p | p2
#   
#   list(
#     pca = pca_result,
#     kmeans = km,
#     pcs_used_for_clustering = M,
#     var_explained = ve,
#     scores_df = scores,
#     loadings_df = L,
#     plot_cluster = p,
#     plot_city = p2,
#     plot_both = combined
#   )
# }
