# ------------------- IMPORTS -------------------

library(readr)

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

# ------------------- PRELIMINARY ANALYSIS -------------------

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

# ------------------- MODEL 2 -------------------
