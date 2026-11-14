}
# 1. Combine the 28 known samples and 1 unknown sample into one full matrix
X_all <- rbind(X_known, X_unknown)
# 2. Run K-Means clustering with k = 2 (representing the two Coffea species)
set.seed(123) # Ensures reproducible cluster assignments every time you run it
km_res <- kmeans(X_all, centers = 2, nstart = 25)
# 3. Attach K-means cluster assignments to your PCA coordinates
kmeans_df <- all_scores %>%
mutate(
Cluster_Num = km_res$cluster[match(spectrum, names(km_res$cluster))],
KMeans_Cluster = paste("Cluster", Cluster_Num)
)
# Print a cross-tabulation table in the console to verify sample groupings
cat("\nK-Means Cluster Assignments vs. External Labels:\n")
print(table(External_Label = kmeans_df$class, Assigned_Cluster = kmeans_df$KMeans_Cluster))
# 4. Calculate cluster centroids in PCA space for clean plotting
centroids_pca <- kmeans_df %>%
group_by(KMeans_Cluster) %>%
summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
# 5. Render the ACS-Compliant K-Means Cluster Plot
cluster_cols <- c("Cluster 1" = "#2a6f97", "Cluster 2" = "#ca6702")
p_kmeans <- ggplot(kmeans_df, aes(PC1, PC2)) +
geom_hline(yintercept = 0, colour = "grey92", linewidth = 0.4) +
geom_vline(xintercept = 0, colour = "grey92", linewidth = 0.4) +
# Plot known reference samples colored by their unsupervised K-means cluster
geom_point(
data = filter(kmeans_df, class != "unknown"),
aes(colour = KMeans_Cluster, shape = class),
size = 2.5,
alpha = 0.85
) +
# Highlight the Kirkland unknown sample as a bold star colored by its assigned cluster
geom_point(
data = filter(kmeans_df, class == "unknown"),
aes(colour = KMeans_Cluster),
shape = 8,
size = 3.8,
stroke = 1.3,
show.legend = FALSE
) +
# Mark the mathematical center (centroid) of each K-means cluster with a cross (+)
geom_point(
data = centroids_pca,
aes(PC1, PC2),
shape = 3,
size = 3.5,
stroke = 1.2,
colour = "black"
) +
scale_colour_manual(values = cluster_cols, name = "K-Means Partition") +
scale_shape_manual(values = c(arabica = 16, robusta = 17), name = "External Label") +
labs(
x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
y = paste0("PC2 (", round(var_exp[2], 1), "%)")
) +
theme_bw(base_size = 8, base_family = "sans") +
theme(
panel.grid.major       = element_blank(),
panel.grid.minor       = element_blank(),
panel.border           = element_rect(colour = "black", linewidth = 0.75),
axis.title             = element_text(size = 9, face = "plain"),
axis.text              = element_text(color = "black"),
legend.title           = element_text(size = 7, face = "bold"),
legend.text            = element_text(size = 7),
legend.position        = "inside",
legend.position.inside = c(0.18, 0.25),
legend.background      = element_rect(fill = "white", color = "black", linewidth = 0.3),
legend.key.size        = unit(0.35, "cm")
)
print(p_kmeans)
ggsave(
filename = file.path(out_dir, "KMeans_Clustering_ACS.png"),
plot = p_kmeans,
width = 3.5,
height = 3.2,
units = "in",
dpi = 600
)
# Load required libraries
library(dplyr)
library(ggplot2)
# =============================================================================
# ---- K-Means Unsupervised Clustering (k = 2) --------------------------------
# =============================================================================
# Ensure output directory exists (defaults to current working directory if unset)
if (!exists("out_dir")) {
out_dir <- "."
}
# 1. Combine the 28 known samples and 1 unknown sample into one full matrix
X_all <- rbind(X_known, X_unknown)
# 2. Run K-Means clustering with k = 2 (representing the two Coffea species)
set.seed(123) # Ensures reproducible cluster assignments every time you run it
km_res <- kmeans(X_all, centers = 2, nstart = 25)
# 3. Attach K-means cluster assignments to your PCA coordinates
kmeans_df <- all_scores %>%
mutate(
Cluster_Num = km_res$cluster[match(spectrum, names(km_res$cluster))],
KMeans_Cluster = paste("Cluster", Cluster_Num)
)
# Print a cross-tabulation table in the console to verify sample groupings
cat("\nK-Means Cluster Assignments vs. External Labels:\n")
print(table(External_Label = kmeans_df$class, Assigned_Cluster = kmeans_df$KMeans_Cluster))
# 4. Calculate cluster centroids in PCA space for clean plotting
centroids_pca <- kmeans_df %>%
group_by(KMeans_Cluster) %>%
summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
# 5. Render the ACS-Compliant K-Means Cluster Plot
cluster_cols <- c("Cluster 1" = "#2a6f97", "Cluster 2" = "#ca6702")
p_kmeans <- ggplot(kmeans_df, aes(PC1, PC2)) +
geom_hline(yintercept = 0, colour = "grey92", linewidth = 0.4) +
geom_vline(xintercept = 0, colour = "grey92", linewidth = 0.4) +
# Plot known reference samples colored by their unsupervised K-means cluster
geom_point(
data = filter(kmeans_df, class != "unknown"),
aes(colour = KMeans_Cluster, shape = class),
size = 2.5,
alpha = 0.85
) +
# Highlight the Kirkland unknown sample as a bold star colored by its assigned cluster
geom_point(
data = filter(kmeans_df, class == "unknown"),
aes(colour = KMeans_Cluster),
shape = 8,
size = 3.8,
stroke = 1.3,
show.legend = FALSE
) +
# Mark the mathematical center (centroid) of each K-means cluster with a cross (+)
geom_point(
data = centroids_pca,
aes(PC1, PC2),
shape = 3,
size = 3.5,
stroke = 1.2,
colour = "black"
) +
scale_colour_manual(values = cluster_cols, name = "K-Means Partition") +
scale_shape_manual(values = c(arabica = 16, robusta = 17), name = "External Label") +
labs(
x = "PC1 (45.8%)",
y = "PC2 (11.5%)"
) +
theme_bw(base_size = 8, base_family = "sans") +
theme(
panel.grid.major       = element_blank(),
panel.grid.minor       = element_blank(),
panel.border           = element_rect(colour = "black", linewidth = 0.75),
axis.title             = element_text(size = 9, face = "plain"),
axis.text              = element_text(color = "black"),
legend.title           = element_text(size = 7, face = "bold"),
legend.text            = element_text(size = 7),
legend.position        = "inside",
legend.position.inside = c(0.18, 0.25),
legend.background      = element_rect(fill = "white", color = "black", linewidth = 0.3),
legend.key.size        = unit(0.35, "cm")
)
print(p_kmeans)
ggsave(
filename = file.path(out_dir, "KMeans_Clustering_ACS.png"),
plot = p_kmeans,
width = 3.5,
height = 3.2,
units = "in",
dpi = 600
)
# Load required libraries
library(dplyr)
library(ggplot2)
# =============================================================================
# ---- K-Means Unsupervised Clustering (k = 2) --------------------------------
# =============================================================================
# Ensure output directory exists (defaults to current working directory if unset)
if (!exists("out_dir")) {
out_dir <- "."
}
# 1. Combine the 28 known samples and 1 unknown sample into one full matrix
X_all <- rbind(X_known, X_unknown)
# 2. Run K-Means clustering with k = 2 (representing the two Coffea species)
set.seed(123) # Ensures reproducible cluster assignments every time you run it
km_res <- kmeans(X_all, centers = 2, nstart = 25)
# 3. Attach K-means cluster assignments to your PCA coordinates
kmeans_df <- all_scores %>%
mutate(
Cluster_Num = km_res$cluster[match(spectrum, names(km_res$cluster))],
KMeans_Cluster = paste("Cluster", Cluster_Num)
)
# Print a cross-tabulation table in the console to verify sample groupings
cat("\nK-Means Cluster Assignments vs. External Labels:\n")
print(table(External_Label = kmeans_df$class, Assigned_Cluster = kmeans_df$KMeans_Cluster))
# 4. Calculate cluster centroids in PCA space for clean plotting
centroids_pca <- kmeans_df %>%
group_by(KMeans_Cluster) %>%
summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
# 5. Render the ACS-Compliant K-Means Cluster Plot
cluster_cols <- c("Cluster 1" = "#2a6f97", "Cluster 2" = "#ca6702")
p_kmeans <- ggplot(kmeans_df, aes(PC1, PC2)) +
geom_hline(yintercept = 0, colour = "grey92", linewidth = 0.4) +
geom_vline(xintercept = 0, colour = "grey92", linewidth = 0.4) +
# Plot known reference samples colored by their unsupervised K-means cluster
geom_point(
data = filter(kmeans_df, class != "unknown"),
aes(colour = KMeans_Cluster, shape = class),
size = 2.5,
alpha = 0.85
) +
# Highlight the Kirkland unknown sample as a bold star colored by its assigned cluster
geom_point(
data = filter(kmeans_df, class == "unknown"),
aes(colour = KMeans_Cluster),
shape = 8,
size = 3.8,
stroke = 1.3,
show.legend = FALSE
) +
# Mark the mathematical center (centroid) of each K-means cluster with a cross (+)
geom_point(
data = centroids_pca,
aes(PC1, PC2),
shape = 3,
size = 3.5,
stroke = 1.2,
colour = "black"
) +
scale_colour_manual(values = cluster_cols, name = "K-Means Partition") +
scale_shape_manual(values = c(arabica = 16, robusta = 17), name = "External Label") +
labs(
x = "PC1 (45.8%)",
y = "PC2 (11.5%)"
) +
theme_bw(base_size = 8, base_family = "sans") +
theme(
panel.grid.major  = element_blank(),
panel.grid.minor  = element_blank(),
panel.border      = element_rect(colour = "black", linewidth = 0.75),
axis.title        = element_text(size = 9, face = "plain"),
axis.text         = element_text(color = "black"),
legend.title      = element_text(size = 7, face = "bold"),
legend.text       = element_text(size = 7),
legend.position   = "top",
legend.box        = "vertical",
legend.box.margin = margin(b = -4),
legend.spacing.y  = unit(0.05, "cm"),
legend.key.size   = unit(0.35, "cm")
)
print(p_kmeans)
ggsave(
filename = file.path(out_dir, "KMeans_Clustering_ACS.png"),
plot = p_kmeans,
width = 3.5,
height = 3.6,
units = "in",
dpi = 600
)
# Load required libraries
library(dplyr)
library(ggplot2)
# =============================================================================
# ---- K-Means Unsupervised Clustering (k = 2) --------------------------------
# =============================================================================
# Ensure output directory exists (defaults to current working directory if unset)
if (!exists("out_dir")) {
out_dir <- "."
}
# 1. Combine the 28 known samples and 1 unknown sample into one full matrix
X_all <- rbind(X_known, X_unknown)
# 2. Run K-Means clustering with k = 2 (representing the two Coffea species)
set.seed(123) # Ensures reproducible cluster assignments every time you run it
km_res <- kmeans(X_all, centers = 2, nstart = 25)
# 3. Attach K-means cluster assignments and format clean labels for plotting
kmeans_df <- all_scores %>%
mutate(
Cluster_Num    = km_res$cluster[match(spectrum, names(km_res$cluster))],
KMeans_Cluster = factor(paste("Cluster", Cluster_Num), levels = c("Cluster 1", "Cluster 2")),
Sample_Role    = factor(
case_when(
class == "arabica" ~ "Arabica",
class == "robusta" ~ "Robusta",
class == "unknown" ~ "Unknown"
),
levels = c("Arabica", "Robusta", "Unknown", "Centroid")
)
)
# Print a cross-tabulation table in the console to verify sample groupings
cat("\nK-Means Cluster Assignments vs. External Labels:\n")
print(table(External_Label = kmeans_df$class, Assigned_Cluster = kmeans_df$KMeans_Cluster))
# 4. Calculate cluster centroids in PCA space and assign the "Centroid" legend role
centroids_pca <- kmeans_df %>%
group_by(KMeans_Cluster) %>%
summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop") %>%
mutate(Sample_Role = factor("Centroid", levels = c("Arabica", "Robusta", "Unknown", "Centroid")))
# 5. Define consistent color and shape palettes
cluster_cols <- c(
"Cluster 1" = "#2a6f97",
"Cluster 2" = "#ca6702"
)
role_shapes <- c(
"Arabica"  = 16, # Solid circle
"Robusta"  = 17, # Solid triangle
"Unknown"  = 8,  # Asterisk (*)
"Centroid" = 3   # Plus (+)
)
# 6. Render the ACS-Compliant K-Means Cluster Plot
p_kmeans <- ggplot() +
geom_hline(yintercept = 0, colour = "grey92", linewidth = 0.4) +
geom_vline(xintercept = 0, colour = "grey92", linewidth = 0.4) +
# Plot known reference samples (circles & triangles) colored by K-means cluster
geom_point(
data = filter(kmeans_df, class != "unknown"),
aes(x = PC1, y = PC2, colour = KMeans_Cluster, shape = Sample_Role),
size = 2.5,
alpha = 0.85
) +
# Plot Kirkland unknown sample (asterisk *) colored consistently by its K-means cluster
geom_point(
data = filter(kmeans_df, class == "unknown"),
aes(x = PC1, y = PC2, colour = KMeans_Cluster, shape = Sample_Role),
size = 3.8,
stroke = 1.3
) +
# Plot cluster centroids (plus +) in black, included in the shape legend
geom_point(
data = centroids_pca,
aes(x = PC1, y = PC2, shape = Sample_Role),
size = 3.5,
stroke = 1.2,
colour = "black"
) +
scale_colour_manual(
values = cluster_cols,
name   = "K-Means Partition",
guide  = guide_legend(order = 1, override.aes = list(shape = 16, size = 2.8))
) +
scale_shape_manual(
values = role_shapes,
name   = "Sample / Marker",
drop   = FALSE,
guide  = guide_legend(order = 2, override.aes = list(colour = "black", size = 2.8))
) +
labs(
x = "PC1 (45.8%)",
y = "PC2 (11.5%)"
) +
theme_bw(base_size = 8, base_family = "sans") +
theme(
panel.grid.major  = element_blank(),
panel.grid.minor  = element_blank(),
panel.border      = element_rect(colour = "black", linewidth = 0.75),
axis.title        = element_text(size = 9, face = "plain"),
axis.text         = element_text(color = "black"),
legend.title      = element_text(size = 7, face = "bold"),
legend.text       = element_text(size = 7),
legend.position   = "top",
legend.box        = "vertical",
legend.box.margin = margin(b = -4),
legend.spacing.y  = unit(0.05, "cm"),
legend.key.size   = unit(0.35, "cm")
)
print(p_kmeans)
ggsave(
filename = file.path(out_dir, "KMeans_Clustering_ACS.png"),
plot = p_kmeans,
width = 3.5,
height = 3.7,
units = "in",
dpi = 600
)
save.image("~/Metabolomics_Analysis_.RData")
load("~/Metabolomics_Analysis_.RData")
# Load required libraries
library(dplyr)
library(ggplot2)
# =============================================================================
# ---- K-Means Unsupervised Clustering (k = 2) --------------------------------
# =============================================================================
# Ensure output directory exists (defaults to current working directory if unset)
if (!exists("out_dir")) {
out_dir <- "."
}
# 1. Combine the 28 known samples and 1 unknown sample into one full matrix
X_all <- rbind(X_known, X_unknown)
# 2. Run K-Means clustering with k = 2 (representing the two Coffea species)
set.seed(123) # Ensures reproducible cluster assignments every time you run it
km_res <- kmeans(X_all, centers = 2, nstart = 25)
# 3. Attach K-means cluster assignments and format clean labels for plotting
kmeans_df <- all_scores %>%
mutate(
Cluster_Num    = km_res$cluster[match(spectrum, names(km_res$cluster))],
KMeans_Cluster = factor(paste("Cluster", Cluster_Num), levels = c("Cluster 1", "Cluster 2")),
Sample_Role    = factor(
case_when(
class == "arabica" ~ "Arabica",
class == "robusta" ~ "Robusta",
class == "unknown" ~ "Unknown"
),
levels = c("Arabica", "Robusta", "Unknown", "Centroid")
)
)
# Print a cross-tabulation table in the console to verify sample groupings
cat("\nK-Means Cluster Assignments vs. External Labels:\n")
print(table(External_Label = kmeans_df$class, Assigned_Cluster = kmeans_df$KMeans_Cluster))
# 4. Calculate cluster centroids in PCA space and assign the "Centroid" legend role
centroids_pca <- kmeans_df %>%
group_by(KMeans_Cluster) %>%
summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop") %>%
mutate(Sample_Role = factor("Centroid", levels = c("Arabica", "Robusta", "Unknown", "Centroid")))
# 5. Define consistent color and shape palettes
cluster_cols <- c(
"Cluster 1" = "#2a6f97", # Blue
"Cluster 2" = "#ca6702"  # Orange
)
role_shapes <- c(
"Arabica"  = 16, # Solid circle
"Robusta"  = 17, # Solid triangle
"Unknown"  = 8,  # Asterisk (*)
"Centroid" = 3   # Plus (+)
)
# 6. Render the ACS-Compliant K-Means Cluster Plot (Figure 2B)
p_kmeans <- ggplot() +
geom_hline(yintercept = 0, colour = "grey92", linewidth = 0.4) +
geom_vline(xintercept = 0, colour = "grey92", linewidth = 0.4) +
# Plot known reference samples (circles & triangles) colored by K-means cluster
geom_point(
data = filter(kmeans_df, class != "unknown"),
aes(x = PC1, y = PC2, colour = KMeans_Cluster, shape = Sample_Role),
size = 2.5,
alpha = 0.85
) +
# Plot Kirkland unknown sample as a bold crimson red asterisk (*), matching Panel A
geom_point(
data = filter(kmeans_df, class == "unknown"),
aes(x = PC1, y = PC2, shape = Sample_Role),
colour = "#9a031e",
size = 3.8,
stroke = 1.3
) +
# Plot cluster centroids (plus +) in black, included in the shape legend
geom_point(
data = centroids_pca,
aes(x = PC1, y = PC2, shape = Sample_Role),
size = 3.5,
stroke = 1.2,
colour = "black"
) +
scale_colour_manual(
values = cluster_cols,
name   = "K-Means Partition",
guide  = guide_legend(order = 1, override.aes = list(shape = 16, size = 2.8))
) +
scale_shape_manual(
values = role_shapes,
name   = "Sample / Marker",
drop   = FALSE,
guide  = guide_legend(
order = 2,
override.aes = list(
colour = c("black", "black", "#9a031e", "black"),
size   = c(2.6, 2.6, 3.2, 3.0)
)
)
) +
labs(
x = "PC1 (45.8%)",
y = "PC2 (11.5%)"
) +
theme_bw(base_size = 8, base_family = "sans") +
theme(
panel.grid.major  = element_blank(),
panel.grid.minor  = element_blank(),
panel.border      = element_rect(colour = "black", linewidth = 0.75),
axis.title        = element_text(size = 9, face = "plain"),
axis.text         = element_text(color = "black"),
legend.title      = element_text(size = 7, face = "bold"),
legend.text       = element_text(size = 7),
legend.position   = "top",
legend.box        = "vertical",
legend.box.margin = margin(b = -4),
legend.spacing.y  = unit(0.05, "cm"),
legend.key.size   = unit(0.35, "cm")
)
print(p_kmeans)
ggsave(
filename = file.path(out_dir, "KMeans_Clustering_ACS.png"),
plot = p_kmeans,
width = 3.5,
height = 3.7,
units = "in",
dpi = 600
)
# Identify which cluster has the lower mean PC1 (Arabica) vs. higher mean PC1 (Robusta)
cluster_means <- kmeans_df %>%
group_by(Cluster_Num) %>%
summarise(mean_PC1 = mean(PC1), .groups = "drop")
arabica_num <- cluster_means$Cluster_Num[which.min(cluster_means$mean_PC1)]
kmeans_df <- kmeans_df %>%
mutate(
KMeans_Cluster = factor(
ifelse(Cluster_Num == arabica_num, "Arabica Cluster", "Robusta Cluster"),
levels = c("Arabica Cluster", "Robusta Cluster")
)
)
cluster_cols <- c(
"Arabica Cluster" = "#2a6f97", # Blue (matches Panel A Arabica)
"Robusta Cluster" = "#ca6702"  # Orange (matches Panel A Robusta)
)
load("~/Metabolomics_Analysis_.RData")
View(X_known)
save.image("~/Metabolomics Lab/Metabolomics_Final_Script_2.RData")
View(normalized_matrix)
View(normalized_matrix)
View(iso_data)
axis.text         = element_text(color = "black"),
savehistory(file = "my_console_history.R")
