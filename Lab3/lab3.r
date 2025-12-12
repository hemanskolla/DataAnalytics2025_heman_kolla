# ------------------- IMPORTS -------------------

library(readr)
library(class)
library(cluster)

# ------------------- LOAD DATA -------------------

setwd("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\DataAnalytics2025_heman_kolla\\Lab3")

# ------------- PROVIDED DATA PREP ----------------

# read dataset
abalone.data <- read.csv("sample\\abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
abalone.data$age.group[abalone.data$rings<=8] <- "young"
abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"

# ---------------- Exercise 1 --------------------- 

# Clean Data
abalone.clean <- na.omit(abalone.data)
abalone.clean$sex <- NULL   # kNN requires numeric features unless encoded

# Create Subsets based on different feature groupings
subset_A <- abalone.clean[, c("length", "diameter", "height")]
subset_B <- abalone.clean[, c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")]

labels <- abalone.clean$age.group

# Create a train/test split (80/20) for both subsets
set.seed(123)
idx <- sample(1:nrow(abalone.clean), size = 0.8 * nrow(abalone.clean))
train_A <- subset_A[idx, ]
test_A  <- subset_A[-idx, ]
train_B <- subset_B[idx, ]
test_B  <- subset_B[-idx, ]
train_labels <- labels[idx]
test_labels  <- labels[-idx]

# kNN Model A
pred_A <- knn(train_A, test_A, cl = train_labels, k = 5)
table_A <- table(Predicted = pred_A, Actual = test_labels)

# kNN Model B
pred_B <- knn(train_B, test_B, cl = train_labels, k = 5)
table_B <- table(Predicted = pred_B, Actual = test_labels)

# Print contingency tables
cat("\nContingency Table - Model A (size features)\n")
print(table_A)
cat("\nContingency Table - Model B (weight features)\n")
print(table_B)

# Compute & Report Accuracy
accA <- sum(diag(table_A)) / sum(table_A)
accB <- sum(diag(table_B)) / sum(table_B)
cat("\nAccuracy Model A:", accA, "\n")
cat("Accuracy Model B:", accB, "\n")

# Determine 
best_subset <- ifelse(accA > accB, "A", "B")
cat("The model with highest accuracy was trained on: Subset ", best_subset, "\n")

# ----- Find optimal k -----

# Identify best subset
subset_best <- if (best_subset == "A") subset_A else subset_B
train_best <- subset_best[idx, ]
test_best  <- subset_best[-idx, ]

# Vary over different k
acc_list <- c()
k_values <- 1:25
for (k in k_values) {
    preds <- knn(train_best, test_best, cl = train_labels, k = k)
    tab_k <- table(preds, test_labels)
    acc_list <- c(acc_list, sum(diag(tab_k)) / sum(tab_k))
}

# Plot Accuracy over Different k
plot(k_values, acc_list, type="b", pch=19,
     xlab="k", ylab="Accuracy",
     main="kNN Accuracy Over Different k")

best_k <- k_values[which.max(acc_list)]
cat("\nThe optimal k value is:", best_k, "\n")

# --------------------------

# ---------------- Exercise 2 --------------------- 

subset_best <- if (best_subset == "A") subset_A else subset_B # Select best Subset form Exercise 1
subset_scaled <- scale(subset_best) # Data Preprocessing is this scaling step

# NOTE. Silhouette is the standard metric for cluster quality

# Computes average silhouette for k
silhouette_for_k <- function(data, k) {
  km <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}

# ----- Find optimal k (K-Means) -----
k_range <- 2:10
sil_kmeans <- sapply(k_range, function(k) silhouette_for_k(subset_scaled, k))
best_k_kmeans <- k_range[which.max(sil_kmeans)]
cat("Optimal K for K-Means:", best_k_kmeans, "\n")
# ------------------------------------

# Train final K-Means
kmeans_final <- kmeans(subset_scaled, centers = best_k_kmeans, nstart = 25)
sil_km_final <- silhouette(kmeans_final$cluster, dist(subset_scaled))

# Make silhouette plot
plot(
  sil_km_final,
  main = paste("Silhouette Plot - K-Means (K =", best_k_kmeans, ")")
)

# ----- Find optimal k (PAM) -----
sil_pam <- c()
for (k in k_range) {
  pam_model <- pam(subset_scaled, k)
  sil_pam <- c(sil_pam, pam_model$silinfo$avg.width)
}

best_k_pam <- k_range[which.max(sil_pam)]
cat("Optimal K for PAM:", best_k_pam, "\n")
# --------------------------------

pam_final <- pam(subset_scaled, best_k_pam)

# Make silhouette plot
plot(
  pam_final$silinfo$widths,
  main = paste("Silhouette Plot - PAM (K =", best_k_pam, ")"),
  xlab = "Cluster",
  ylab = "Silhouette Width"
)

