
model_climate_point_data_pca <- function(question) {
library(psych)
set.seed(123)

#load the data
dir_path <- "./data/delta"
files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
dat_list <- lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE))
df <- do.call(rbind, dat_list)  

#set which column to use for delta - for a specific question
delta_col <- paste0(question, "_delta")
needed <- c("Education", "Occupation", "Duration", "age", delta_col)
Data1<-df%>%dplyr::select(all_of(needed)) %>%
mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

#perform PCA
pca_result <- prcomp(Data1, center = TRUE, scale = TRUE)
summary(pca_result)
screeplot(pca_result,type="l")
print(pca_result$rotation)
eigenvectors <- pca_result$rotation  
eigenvalues  <- pca_result$sdev^2

# Loadings scaled to score space
L <- as.data.frame(pca_result$rotation[, 1:2])
names(L) <- c("PC1","PC2")
L$var <- rownames(L)

rng <- function(z) diff(range(z))
arrow_scale <- 0.7 * min(rng(scores$PC1)/rng(L$PC1), rng(scores$PC2)/rng(L$PC2))
Lx <- transform(L, PC1 = PC1 * arrow_scale, PC2 = PC2 * arrow_scale)

ve <- pca_result$sdev^2 / sum(pca_result$sdev^2)
lab_x <- sprintf("PC1 (%.1f%%)", 100*ve[1]); lab_y <- sprintf("PC2 (%.1f%%)", 100*ve[2])

scores <- as.data.frame(pca_result$x[, 1:2])
names(scores) <- c("PC1","PC2")
scores$city <- factor(df$Location) 

ggplot() +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_point(data = scores, aes(PC1, PC2, color = city), alpha = 0.7) +
  geom_segment(data = Lx,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text(data = Lx, aes(PC1, PC2, label = var), vjust = -0.4, size = 3) +
  labs(x = lab_x, y = lab_y, title = "PCA biplot (scores + loadings)", color = "City") +
  theme_minimal()

}


#--------------------------new function

model_climate_point_data_pca <- function(question, k = 3, var_target = 0.80) {
  # pkgs
  library(dplyr)
  library(ggplot2)
  library(grid)   # for unit()
  
  set.seed(123)
  
  # --- load & assemble data ---
  dir_path <- "./data/delta"
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  dat_list <- lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE))
  df <- do.call(rbind, dat_list)
  
  # choose delta column from question input (e.g., "que_6" -> "que_6_delta")
  delta_col <- paste0(question, "_delta")
  needed <- c("Education", "Occupation", "Duration", "age", delta_col)
  
  if (!all(needed %in% names(df))) {
    stop("Missing columns: ", paste(setdiff(needed, names(df)), collapse = ", "))
  }
  
  Data1 <- df %>%
    dplyr::select(dplyr::all_of(needed))
  
  # coerce to numeric safely
  Data1[] <- lapply(Data1, function(z) suppressWarnings(as.numeric(z)))
  
  # keep rows with complete numeric data; also align city/Location
  mask <- complete.cases(Data1)
  X <- Data1[mask, , drop = FALSE]
  city <- if ("Location" %in% names(df)) factor(df$Location[mask]) else NULL
  
  if (nrow(X) < 3) stop("Not enough complete rows after filtering.")
  
  # --- PCA ---
  pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
  
  # how many PCs to use for clustering? (>= 2, reach var_target if possible)
  ve <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  M <- suppressWarnings(max(2, which(cumsum(ve) >= var_target)[1]))
  if (!is.finite(M)) M <- min(2, ncol(X))  # fallback
  
  # --- clustering on first M PCs ---
  S <- scale(pca_result$x[, 1:M, drop = FALSE])
  set.seed(123)
  km <- kmeans(S, centers = k, nstart = 50)
  clusters <- factor(km$cluster)
  
  # --- build scores for plotting (PC1 & PC2) ---
  scores <- as.data.frame(pca_result$x[, 1:2, drop = FALSE])
  names(scores) <- c("PC1", "PC2")
  scores$cluster <- clusters
  if (!is.null(city)) scores$city <- city
  
  # --- loadings scaled to score space for biplot arrows ---
  L <- as.data.frame(pca_result$rotation[, 1:2, drop = FALSE])
  names(L) <- c("PC1", "PC2")
  L$var <- rownames(L)
  
  rng <- function(z) diff(range(z))
  arrow_scale <- 0.7 * min(rng(scores$PC1) / rng(L$PC1),
                           rng(scores$PC2) / rng(L$PC2))
  Lx <- transform(L, PC1 = PC1 * arrow_scale, PC2 = PC2 * arrow_scale)
  
  lab_x <- sprintf("PC1 (%.1f%%)", 100 * ve[1])
  lab_y <- sprintf("PC2 (%.1f%%)", 100 * ve[2])
  
  # --- plot: clusters on PC1–PC2 + loadings arrows ---
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = 3) +
    geom_vline(xintercept = 0, linetype = 3) +
    geom_point(data = scores, aes(PC1, PC2, color = cluster), alpha = 0.75) +
    stat_ellipse(data = scores, aes(PC1, PC2, color = cluster),
                 level = 0.68, linewidth = 0.6, linetype = 2, show.legend = FALSE) +
    geom_segment(data = Lx,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.02, "npc"))) +
    geom_text(data = Lx, aes(PC1, PC2, label = var), vjust = -0.4, size = 3) +
    labs(
      x = lab_x, y = lab_y,
      title = paste0("PCA biplot with clusters: ", question, "_delta"),
      color = "Cluster"
    ) +
    theme_minimal()
  
  # optionally, if you want to color by city instead of cluster, swap aes(color = cluster) to city
  
  # return everything useful
  list(
    pca = pca_result,
    kmeans = km,
    scores_plot = p,
    scores_df = scores,
    loadings_df = L,
    pcs_used_for_clustering = M,
    var_explained = ve
  )
}










#using principal 

pca_result <- principal(Data1, nfactors = 2, rotate = "varimax")
pca_result

fit.varimax<-principal(Data1,nfactors=2,rotate="varimax")
fit.varimax
loadings <- fit.varimax$loadings
loadings1<-loadings(fit.varimax)
loadings1
component_scores <- as.matrix(Data1) %*% loadings
normalized_scores <- scale(component_scores)
composite_data <- data.frame(RC1 = normalized_scores[, 1], RC2 = normalized_scores[, 2])
View(composite_data)
standardized_data <- scale(composite_data)

#----

set.seed(42)
fviz_nbclust(pca_result, kmeans, method = "wss") + labs(subtitle = "Elbow method")

set.seed(42)
fviz_nbclust(S, kmeans, method = "silhouette") + labs(subtitle = "Silhouette")

set.seed(42)
gap <- clusGap(S, FUN = kmeans, nstart = 50, K.max = 10, B = 200)
fviz_gap_stat(gap)


#---

set.seed(123)
k <- 3  # <- pick from the diagnostics above
km <- kmeans(S, centers = k, nstart = 100)
clusters <- factor(km$cluster)

# Alternative: hierarchical Ward
# D  <- dist(S)
# hc <- hclust(D, method = "ward.D2")
# clusters <- factor(cutree(hc, k = k))


#---

# Scores on first two PCs (from the same fit)
scores2 <- as.data.frame(fit$scores[, 1:2])
names(scores2) <- c("PC1", "PC2")
scores2$cluster <- clusters

p_scores <- ggplot(scores2, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.75) +
  stat_ellipse(level = 0.68, linewidth = 0.6, linetype = 2, show.legend = FALSE) +
  labs(title = "Clusters visualized on PC1–PC2 scores") +
  theme_minimal()
p_scores









fviz_nbclust(standardized_data,kmeans,method="wss")+labs(subtitle="Elbow method")
fviz_nbclust(standardized_data,kmeans,method="silhouette")+labs(subtitle="Sil method")
set.seed(200)
gap_stat<-clusGap(standardized_data,FUN=kmeans,nstart=25,K.max=10,B=50)
print(gap_stat,method="firstmax")
fviz_gap_stat(gap_stat)

set.seed(20)
data.matrix(standardized_data)
TIPOC3c<-(na.omit(data.matrix(standardized_data)))
Tipo3c<-dist(standardized_data,method="euclidean")
hclust(Tipo3c,method="ward.D")
tipo3C<-hclust(Tipo3c,method="ward.D")

windows()
plot(tipo3C)

windows()
# Plot and save the dendrogram
png("Ddendrogram_plot.png", width = 800, height = 600)  # Open a PNG device
plot(tipo3C, main = "Dendrogram", xlab = "Observations", sub = "", ylab = "Height")  # Plot the dendrogram
rect.hclust(tipo3C, k = 4, border = "red")  # Add rectangles to the plot for 3 clusters





kc<-kmeans(standardized_data,4)
kc
kc$centers
write.csv(kc$centers,file="clusterpca4.csv")



dataset<-cbind(F4,kc$cluster) 
View(dataset)
dataset$cluster<-kc$cluster
View(dataset)

View(dataset)

cluster_summary <- dataset %>%group_by(cluster) %>%summarise(Mean_Variable1 = mean(age),SD_Variable1 = sd(age),Mean_Variable2 = mean(Duration),SD_Variable2 = sd(Duration),Mean_Variable3= mean(Occupation),SD_Variable3 = sd(Occupation),Mean_Variable4 = mean(Education),SD_Variable4 = sd(Education),Count = n())
cluster_summary
print(cluster_summary)
write.csv(cluster_summary,file="clustersummary.csv")


F6<-cbind(dataset,F5)
View(F6)



View(F6)


#using principal 
# 
# pca_result <- principal(Data1, nfactors = 2, rotate = "varimax")
# pca_result
# 
# fit.varimax<-principal(Data1,nfactors=2,rotate="varimax")
# fit.varimax
# loadings <- fit.varimax$loadings
# loadings1<-loadings(fit.varimax)
# loadings1
# component_scores <- as.matrix(Data1) %*% loadings
# normalized_scores <- scale(component_scores)
# composite_data <- data.frame(RC1 = normalized_scores[, 1], RC2 = normalized_scores[, 2])
# View(composite_data)
# standardized_data <- scale(composite_data)
