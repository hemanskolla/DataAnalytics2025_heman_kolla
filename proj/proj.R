# ------------------- IMPORTS -------------------

library(readr)
library(car)
library(e1071)

# ------------------- LOAD DATA -------------------

setwd("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\DataAnalytics2025_heman_kolla\\proj")
df <- read_csv("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\QML_URP_F25\\data.csv")

# ------------------- DATA DESCRIPTION -------------------

print(head(df))
summary(df)

num_variables <- length(df)
total_observations <- nrow(df)
total_firms <- length(unique(df$permno))
total_dates <- length(unique(df$Date))
cat("Number of Variables:", num_variables,
    "\nNumber of Total Observations:", total_observations,
    "\nNumber of Unique Firms:", total_firms, 
    "\nNumber of Unique Dates Measured:", total_dates, "\n")

# ------------------- DATA CLEANING -------------------

# Clean NA values in df 
first_month <- min(df$Date) # Identify the earliest month in the dataset
is_first_month <- df$Date == first_month
na_only_retlag <- is.na(df$`return (t-1)`) & rowSums(is.na(df[, setdiff(names(df), "return (t-1)")])) == 0 # Identify which rows have NA ONLY in return (t-1)
any_na <- apply(is.na(df), 1, any) # Identify rows with any NA
keep_rows <- (is_first_month & (na_only_retlag | !any_na)) |
             (!is_first_month & !any_na) # Do not keep non-first month rows with any_na or first month rows with any_na (besides ret(t-1) )
df <- df[keep_rows, ] # Perform Filter

summary(df)

total_observations <- nrow(df)
total_firms <- length(unique(df$permno))
total_dates <- length(unique(df$Date))
cat("Number of Total Observations:", total_observations,
    "\nNumber of Unique Firms:", total_firms, 
    "\nNumber of Unique Dates Measured:", total_dates, "\n")

# ------------------- PRELIMINARY ANALYSIS & EXPLORATORY ANALYSIS -------------------

# Define features (exactly as in paper's Table A.2)
FEATURE_COLS <- c(
  'mom1m', 'mom12m', 'chmom', 'indmom', 'mom36m',
  'turn', 'mvel1', 'dolvol', 'ill', 'zerotrade',
  'baspread', 'retvol', 'idiovol', 'beta', 'betasq',
  'ep', 'sp', 'agr', 'nincr', 'return (t-1)'
)

TARGET_COL <- 'return' #  Current month return (NOT t-1)

x <- df[, FEATURE_COLS]
y <- df[[TARGET_COL]]

summary(x)
summary(y)

summary_stats <- function(v) {
  c(
    Min    = round(min(v, na.rm = TRUE), 4),
    Q1     = round(quantile(v, 0.25, na.rm = TRUE), 4),
    Median = round(median(v, na.rm = TRUE), 4),
    Mean   = round(mean(v, na.rm = TRUE), 4),
    Q3     = round(quantile(v, 0.75, na.rm = TRUE), 4),
    Max    = round(max(v, na.rm = TRUE), 4)
  )
}

# Initial Boxplots for x
for (col in FEATURE_COLS) {
  
  v <- x[[col]]
  stats <- summary_stats(v)
  
  par(mfrow = c(1, 2),        # boxplot + stats
      mar = c(5, 4, 4, 1))    # margins
  
  # Boxplot
  boxplot(v,
          horizontal = TRUE,
          col = "lightblue",
          main = paste("Boxplot of", col),
          xlab = col)
  
  # Summary stats panel
  par(mar = c(5, 1, 4, 2))
  plot.new()
  text(
    x = 0,
    y = seq(0.9, 0.1, length.out = length(stats)),
    labels = paste(names(stats), ":", stats),
    adj = 0,
    cex = 1.0
  )
  
    par(mfrow = c(1, 1)) # Reset graphics parameters
}

# Initial Boxplots for y
stats_y <- summary_stats(y)
par(mfrow = c(1, 2),
    mar = c(5, 4, 4, 1))
boxplot(y,
        horizontal = TRUE,
        col = "lightblue",
        main = "Boxplot of Return",
        xlab = "Return")
par(mar = c(5, 1, 4, 2))
plot.new()
text(
  x = 0,
  y = seq(0.9, 0.1, length.out = length(stats_y)),
  labels = paste(names(stats_y), ":", stats_y),
  adj = 0,
  cex = 1.0
)
par(mfrow = c(1, 1))

if(!dir.exists("plots")) {
  dir.create("plots")
}

# Plot histograms for all x
for (col in FEATURE_COLS) {
    file_name <- paste0("plots/hist_", col, ".png")
    png(filename = file_name, width = 800, height = 600)

    hist(x[[col]],
        main = paste("Histogram of", col),
        xlab = col,
        col = "lightblue")

    dev.off() # Close the file
}

# Plot histogram for y
png(filename = paste0("plots/hist_return.png"), width = 800, height = 600)
hist(y,
    main = paste("Histogram of Return"),
    xlab = col,
    col = "lightblue")
dev.off()

# NOTE. "ill" and "mvel1" are both right-skewed
# Perform Necessary log transform
x$ill <- log(x$ill + 1)     # log-transform ill, add 1 to avoid log(0)
x$mvel1 <- log(x$mvel1 + 1) # log-transform mvel1, add 1 to avoid log(0)

png(filename = paste0("plots/hist_ill_log.png"), width = 800, height = 600)
hist(y,
    main = paste("Histogram of log(ill)"),
    xlab = "log(ill)",
    col = "lightblue")
dev.off()

png(filename = paste0("plots/hist_mvel1_log.png"), width = 800, height = 600)
hist(y,
    main = paste("Histogram of log(mvel1)"),
    xlab = "log(mvel1)",
    col = "lightblue")
dev.off()

# NOTE. If I were to do scatterplots to identify promising features then I would be plotting
# 727376 observations on each, which would make it a visual blob. To avoid this and the large
# file size, we instead use a correlation matrix

# Display correlation between features as a heatmap
cor_features <- cor(x, use = "pairwise.complete.obs") 
heatmap(cor_features, symm = TRUE,
    col = colorRampPalette(c("blue", "white", "red"))(100),
    main = "Feature-to-Feature Correlation")

# Print correlations between each feature and return (target)
cor_target <- sapply(x, function(col) cor(col, y))
cor_target_df <- data.frame(Feature = names(cor_target),
                            Correlation_with_return = cor_target)
cor_target_df <- cor_target_df[order(abs(cor_target_df$Correlation_with_return), decreasing = TRUE), ]
print(cor_target_df)

# ------------------- MODEL 1 -------------------

# Create combined dataframe
df_model_one <- data.frame(y, x) 
df_model_one <- df_model_one[!is.na(df_model_one$return..t.1.), ] # Clean first month NA, since this breaks model

# Regress y on all other columns (20 features)
model_one <- lm(y ~ ., data = df_model_one) 
summary(model_one)

# Check for multi-collinearity
# NOTE. This should reflect some of what we saw in the heatmap from "cor_features"
vif(model_one) 

# Check model quality via residuals
par(mfrow = c(2,2))
plot(model_one)

# --- Test PCR ---
x_features <- df_model_one[, -1] # To keep PCA transformations separate
x_scaled <- scale(x_features)

# Perform PCA analysis
pca <- prcomp(x_scaled, center = TRUE, scale. = TRUE)
summary(pca)
plot(pca, type = "l", main = "Scree Plot of PCA")

# Create dataframe and model
num_pcs <- 10
pc_data <- data.frame(y = df_model_one$y, pca$x[, 1:num_pcs])
model_pca <- lm(y ~ ., data = pc_data)
summary(model_pca)

# NOTE. The resulting R-Squared value is not better than the original Multiple Regression Model.
#-----------------

# ------------------- MODEL 2 -------------------

df_model_two <- data.frame(y, x) # Create combined dataframe
df_model_two <- df_model_two[!apply(df_model_two, 1, function(row) any(is.na(row) | is.nan(row))), ]

# Sample first
set.seed(123)  # Reproducible randomness
sub_idx <- sample(1:nrow(df_model_two), size = 20000) 
df_sub <- df_model_two[sub_idx, ]

# Scale the subsample
x_scaled <- scale(df_sub[, -1])
y_scaled <- scale(df_sub$y)
df_scaled <- data.frame(y = y_scaled, x_scaled)

# Save for unscaling:
y_mean <- mean(df_sub$y)
y_sd <- sd(df_sub$y)

# Make Train/Test Split
set.seed(123)  # Reproducible Randomness Seed
train_idx <- sample(1:nrow(df_scaled), size = 0.8 * nrow(df_scaled))
train <- df_scaled[train_idx, ]
test <- df_scaled[-train_idx, ]

# Train SVR model
model_two <- svm(
  y ~ ., 
  data = train, 
  type = "eps-regression",    # epsilon-SVR
  kernel = "linear"           # Linear kernel
)

# NOTE. Linear kernel used in place of radial (RBF) since RBF is less time efficient

# Predict
y_pred <- predict(model_two, test)

# Undo scaling using subsample stats
y_pred_orig <- y_pred * y_sd + y_mean
y_test_orig <- test$y * y_sd + y_mean

# Calculate R-Squared
r2 <- 1 - sum((y_test_orig - y_pred_orig)^2) / sum((y_test_orig - mean(y_test_orig))^2)
cat("SVR R-squared:", r2, "\n")

# Plot predictions vs actuals
plot(y_test_orig, y_pred_orig, 
     xlab = "Actual Returns", ylab = "Predicted Returns", 
     main = "SVR Predictions vs Actuals", pch = 20, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # 45-degree reference line
