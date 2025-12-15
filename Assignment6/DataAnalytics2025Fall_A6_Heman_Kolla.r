# ------------------- IMPORTS -------------------

library(ggplot2)
library(readr)
library(class)
library(randomForest)
library(e1071)
library(caret)

# ------------------- LOAD DATA -------------------

setwd("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\DataAnalytics2025_heman_kolla\\Assignment6")
df <- read_csv("sample\\bank-full.csv")

# ----------- EXPLORATORY DATA ANALYSIS -----------

# Basic Dataset Info
cat("Number of observations:", nrow(df), "\n")
cat("Number of variables:", ncol(df), "\n\n")
head(df)
str(df)

# Clean Missing / NaN Values
df_clean <- df %>%
  filter(complete.cases(.))
cat("Observations after removing NA/NaN:", nrow(df_clean), "\n")

# Analyze Target Variable "y"
df_clean$y <- as.factor(df_clean$y)
y_counts <- table(df_clean$y)
print(y_counts)
y_df <- as.data.frame(y_counts)
colnames(y_df) <- c("Subscription", "Count")
ggplot(y_df, aes(x = "", y = Count, fill = Subscription)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(
    title = "Distribution of Term Deposit Subscription (y)",
    fill = "Subscribed"
  ) +
  theme_void()

# Analyze Numeric Input Variables
numeric_vars <- c(
  "age", "balance", "duration",
  "campaign", "pdays", "previous"
)
summary(df_clean[, numeric_vars])
for (var in numeric_vars) {
  p <- ggplot(df_clean, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(
      title = paste("Histogram of", var),
      x = var,
      y = "Frequency"
    ) +
    theme_minimal()
  
  print(p)
}

# Remove Outliers
remove_outliers_iqr <- function(df, var) {
  Q1 <- quantile(df[[var]], 0.25)
  Q3 <- quantile(df[[var]], 0.75)
  IQR <- Q3 - Q1
  df %>%
    filter(df[[var]] >= (Q1 - 1.5 * IQR),
           df[[var]] <= (Q3 + 1.5 * IQR))
}
df_no_outliers <- df_clean
for (var in numeric_vars) {
  df_no_outliers <- remove_outliers_iqr(df_no_outliers, var)
}
df_clean <- df_no_outliers

# Analyze Categorical Input Variables
categorical_vars <- c(
  "job", "marital", "education", "default",
  "housing", "loan", "contact", "month", "poutcome"
)
df_clean[categorical_vars] <- lapply(
  df_clean[categorical_vars],
  as.factor
)
for (var in categorical_vars) {
  print(table(df_clean[[var]]))
  
  p <- ggplot(df_clean, aes(x = .data[[var]])) +
    geom_bar(fill = "darkorange", color = "black") +
    labs(
      title = paste("Frequency of", var),
      x = var,
      y = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# --------------- MODEL DEVELEPMONT ---------------

# Data Prep
df_clean$y <- as.factor(df_clean$y)
df_clean[categorical_vars] <- lapply(df_clean[categorical_vars], as.factor)
df_clean <- df_clean %>% filter(complete.cases(.[numeric_vars]))
set.seed(123)
train_index <- createDataPartition(df_clean$y, p = 0.7, list = FALSE)
train_data <- df_clean[train_index, ]
test_data  <- df_clean[-train_index, ]
numeric_vars_filtered <- numeric_vars[sapply(train_data[, numeric_vars], function(x) sd(x) != 0)]


# --- Model 1 (kNN) --- 
# kNN Data Prep
x_train <- scale(train_data[, numeric_vars_filtered])
x_test  <- scale(
  test_data[, numeric_vars_filtered],
  center = attr(x_train, "scaled:center"),
  scale  = attr(x_train, "scaled:scale")
)
y_train <- train_data$y
y_test  <- test_data$y

knn_pred <- knn(
  train = x_train,
  test  = x_test,
  cl    = y_train,
  k     = 7
)
cat("\n--- kNN Confusion Matrix ---\n")
print(table(knn_pred, y_test))
print(confusionMatrix(knn_pred, y_test))
# ---------------------

# --- Model 2 (Random Forest) --- 
set.seed(123)
rf_model <- randomForest(
  y ~ age + balance + duration + campaign + pdays + previous +
       job + marital + education + default + housing + loan +
       contact + month + poutcome,
  data = train_data,
  ntree = 200,
  mtry = 4,
  importance = TRUE
)

rf_pred <- predict(rf_model, test_data)
cat("\n--- Random Forest Confusion Matrix ---\n")
print(table(rf_pred, y_test))
print(confusionMatrix(rf_pred, y_test))
importance(rf_model)
varImpPlot(rf_model)
# -------------------------------

# --- Model 3 (Naive Bayes) --- 
nb_model <- naiveBayes(
  y ~ age + balance + duration + campaign + pdays + previous +
       job + marital + education + default + housing + loan +
       contact + month + poutcome,
  data = train_data
)

nb_pred <- predict(nb_model, test_data)
cat("\n--- Naive Bayes Confusion Matrix ---\n")
print(table(nb_pred, y_test))
print(confusionMatrix(nb_pred, y_test))
# -----------------------------

