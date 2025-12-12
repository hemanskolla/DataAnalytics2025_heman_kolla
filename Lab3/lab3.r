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
