# ------------------- IMPORTS -------------------

library(readr)
library(ggplot2)
library(dplyr)
library(class)     
library(randomForest) 
library(car)
library(e1071) 
library(caret)

# ------------------- LOAD DATA -------------------

setwd("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\DataAnalytics2025_heman_kolla\\Assignment5")
df <- read_csv("sample\\NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

# ------------------- Problem 1a -------------------

df_manhattan <- df[df$BOROUGH == "MANHATTAN", ]

# NOTE. 
# Trends we look for is relationships between the header fields of the input CSV within the Manhattan borough. This includes 
# some single field distributional information, but also whether those features vary given another feature constraint. I plan
# to explore this by plotting some boxplots, identifying outliers and marginal distributions. This will be done in exploratory
# analysis alongside a correlation matrix. Then, a regression model will be done as part of modeling in Problem 1c.

# ------------------- Problem 1b -------------------

# NOTE. 
# For this section, I cleaned the data. Then I made initial boxplots which showcased the outliers on as beyond the tails of the 
# boxplot. This was done for a few features I wanted to examine as part of the exploratory data analysis (eda). From there, I 
# cleaned outliers, then redid boxplots. From there, I looked at which y-axis scale was so off that the data was not interpretable.
# I found all 4 except year built to have that. Since sale price is something we want to predict, I didnt mess with its scale. So,
# I ended up performing log transformations on the other three variables, and reported updated boxplots that showcase their proper
# y-axis scaling. Additionally, we then preformed feature-to-feature correlation and feature-to-target correlation.We found that
# gross and land sq ft were strongly correlated, while other features displayed a weaker negative correlation. The correlation of 
# each feature with sale price (the target) was also weakly positive.

# Data Cleaning
eda_vars <- c("SALE PRICE", "GROSS SQUARE FEET", "LAND SQUARE FEET", "YEAR BUILT", "RESIDENTIAL UNITS")
eda_data <- df_manhattan[, eda_vars]
eda_data[eda_data == ""] <- NA
eda_data <- na.omit(eda_data)
eda_data <- data.frame(
  lapply(eda_data, function(col) {
    as.numeric(gsub("[^0-9.-]", "", col))
  })
)
eda_data <- data.frame(
  lapply(eda_data, function(col) col[is.finite(col)])
)
 
# Initial Boxplots
boxplot(eda_data$SALE.PRICE,
        main = "Boxplot of SALE PRICE",
        ylab = "SALE PRICE",
        col = "lightblue",
        outline = TRUE)
boxplot(eda_data$GROSS.SQUARE.FEET,
        main = "Boxplot of GROSS SQUARE FEET",
        ylab = "GROSS SQUARE FEET",
        col = "lightgreen",
        outline = TRUE)
boxplot(eda_data$LAND.SQUARE.FEET,
        main = "Boxplot of LAND SQUARE FEET",
        ylab = "LAND SQUARE FEET",
        col = "lightpink",
        outline = TRUE)
boxplot(eda_data$YEAR.BUILT,
        main = "Boxplot of YEAR BUILT",
        ylab = "YEAR BUILT",
        col = "lightyellow",
        outline = TRUE)
boxplot(eda_data$RESIDENTIAL.UNITS,
        main = "Boxplot of RESIDENTIAL UNITS",
        ylab = "RESIDENTIAL UNITS",
        col = "lightgray",
        outline = TRUE)

# Remove Outliers
remove_outliers_iqr_df <- function(df) {
  df_clean <- df
  for (col in colnames(df_clean)) {
    Q1 <- quantile(df_clean[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df_clean[[col]], 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR_val
    upper <- Q3 + 1.5 * IQR_val
    df_clean <- df_clean[df_clean[[col]] >= lower & df_clean[[col]] <= upper, ]
  }
  return(df_clean)
}
eda_data_clean <- remove_outliers_iqr_df(eda_data)

# Boxplots with No Outliers
boxplot(eda_data_clean$SALE.PRICE,
        main = "Boxplot of SALE PRICE (Outliers Removed)",
        ylab = "SALE PRICE",
        col = "lightblue")
boxplot(eda_data_clean$GROSS.SQUARE.FEET,
        main = "Boxplot of GROSS SQUARE FEET (Outliers Removed)",
        ylab = "GROSS SQUARE FEET",
        col = "lightgreen")
boxplot(eda_data_clean$LAND.SQUARE.FEET,
        main = "Boxplot of LAND SQUARE FEET (Outliers Removed)",
        ylab = "LAND SQUARE FEET",
        col = "lightpink")
boxplot(eda_data_clean$YEAR.BUILT,
        main = "Boxplot of YEAR BUILT (Outliers Removed)",
        ylab = "YEAR BUILT",
        col = "lightyellow")
boxplot(eda_data_clean$RESIDENTIAL.UNITS,
        main = "Boxplot of RESIDENTIAL UNITS (Outliers Removed)",
        ylab = "RESIDENTIAL UNITS",
        col = "lightgray")

# Perform Log Transformations where needed
eda_data_clean$log_GROSS_SQFT <- log1p(eda_data_clean$GROSS.SQUARE.FEET)
eda_data_clean$log_LAND_SQFT  <- log1p(eda_data_clean$LAND.SQUARE.FEET)
eda_data_clean$log_RES_UNITS   <- log1p(eda_data_clean$RESIDENTIAL.UNITS)

# Boxplots of Log Transformed Features
boxplot(eda_data_clean$log_GROSS_SQFT,
        main = "Boxplot of log(GROSS SQUARE FEET + 1)",
        col = "lightgreen")
boxplot(eda_data_clean$log_LAND_SQFT,
        main = "Boxplot of log(LAND SQUARE FEET + 1)",
        col = "lightpink")
boxplot(eda_data_clean$log_RES_UNITS,
        main = "Boxplot of log(RESIDENTIAL UNITS + 1)",
        col = "lightgray")

# Summary Stats of Log Transformed Features
summary(eda_data_clean$log_GROSS_SQFT)
summary(eda_data_clean$log_LAND_SQFT)
summary(eda_data_clean$log_RES_UNITS)

# Correlation Data Prep
x <- eda_data_clean %>%
  select(YEAR.BUILT, log_GROSS_SQFT, log_LAND_SQFT, log_RES_UNITS)
y <- eda_data_clean$SALE.PRICE

# Feature to Feature Correlation
cor_features <- cor(x, use = "pairwise.complete.obs")
heatmap(cor_features, symm = TRUE,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Feature-to-Feature Correlation (Manhattan)")

# Feature to Target Correlation
cor_target <- sapply(x, function(col) cor(col, y, use = "complete.obs"))
cor_target_df <- data.frame(Feature = names(cor_target),
                            Correlation_with_SALE_PRICE = cor_target)
cor_target_df <- cor_target_df[order(abs(cor_target_df$Correlation_with_SALE_PRICE), decreasing = TRUE), ]
print(cor_target_df)


# ------------------- Problem 1c -------------------

# NOTE. 
# Initially I had done the below model. However, the summary stats yielded an R-squared of approximately 0.01 with most features being
# poor predictors. As such, I decided to redo the cleaning process.

df_model <- data.frame(SALE_PRICE = y, x) # Create combined dataframe
mlr_model <- lm(SALE_PRICE ~ ., data = df_model) # Fit multiple linear model
summary(mlr_model) # Print summary stats

# NOTE. 
# For data cleaning, I followed the same format as before. I extreacted the data, removed all missing and NA values, then ensured my
# data was finite and numeric.

# Data Cleaning
predictor_vars <- c(
  "BLOCK", "LOT", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "TOTAL UNITS",
  "LAND SQUARE FEET", "GROSS SQUARE FEET", "YEAR BUILT",
  "Latitude", "Longitude", "Community Board", "Council District", 
  "Census Tract", "BIN", "BBL"
)
all_vars <- c("SALE PRICE", predictor_vars)
df_model_raw <- df_manhattan[, all_vars]
df_model_raw[df_model_raw == ""] <- NA
df_model_raw <- na.omit(df_model_raw)
df_model_numeric <- data.frame(
  lapply(df_model_raw, function(col) as.numeric(gsub("[^0-9.-]", "", col)))
)
df_model_numeric <- data.frame(
  lapply(df_model_numeric, function(col) col[is.finite(col)])
)

# Model 2 Data Prep
df_model_2 <- df_model_numeric
df_model_2$log_RESIDENTIAL_UNITS <- log1p(df_model_2$`RESIDENTIAL.UNITS`)
df_model_2$log_GROSS_SQFT       <- log1p(df_model_2$`GROSS.SQUARE.FEET`)
df_model_2$log_LAND_SQFT        <- log1p(df_model_2$`LAND.SQUARE.FEET`)
df_model_2$`RESIDENTIAL.UNITS`  <- NULL
df_model_2$`GROSS.SQUARE.FEET`  <- NULL
df_model_2$`LAND.SQUARE.FEET`   <- NULL

# Model 2
y2 <- df_model_2$SALE.PRICE
x2 <- df_model_2[, setdiff(colnames(df_model_2), "SALE.PRICE")]
df_model_2_final <- data.frame(SALE_PRICE = y2, x2)
mlr_model_2 <- lm(SALE_PRICE ~ ., data = df_model_2_final)
summary(mlr_model_2)

# NOTE. 
# Now, here, I found that the R-squared was 0.044, which meant the model was performing better than before. However, an R-squared value 
# that low still indicates poor performance. It means that the model explains, with an R-squard of 0.044, approximately 4.4% of the 
# variance in sale price given the feature set. 

# Model 3
y <- df_model_numeric$SALE.PRICE
x <- df_model_numeric[, setdiff(colnames(df_model_numeric), "SALE.PRICE")]
df_model_3 <- data.frame(SALE_PRICE = y, x)
mlr_model_3 <- lm(SALE_PRICE ~ ., data = df_model_3)
summary(mlr_model_3)

# NOTE. 
# I wanted to test what would have happened had I not performed the log transformations, and I received a R-Sqaured of 0.05, which is 
# better than the other two models, but again, is poor performance

# ------------------- Problem 1d -------------------

# NOTE. 
# The cleaning done here is to select only the quantitative variables base don price and area as instructed. I resused the cleaned 
# df_numerica dataframe from above in doing so. Then, I created the train/test split

df_class <- data.frame(
  SALE_PRICE = df_model_numeric$SALE.PRICE,
  GROSS_SQFT = df_model_numeric$GROSS.SQUARE.FEET,
  LAND_SQFT  = df_model_numeric$LAND.SQUARE.FEET,
  RES_UNITS  = df_model_numeric$RESIDENTIAL.UNITS,
  NEIGHBORHOOD = factor(
    df_manhattan$NEIGHBORHOOD[as.numeric(rownames(df_model_numeric))]
  )
)
set.seed(123)
train_idx <- createDataPartition(df_class$NEIGHBORHOOD, p = 0.7, list = FALSE)
train_data <- df_class[train_idx, ]
test_data  <- df_class[-train_idx, ]

# --- Model 1 (kNN) --- 
x_train <- scale(train_data[, c("SALE_PRICE", "GROSS_SQFT", "LAND_SQFT", "RES_UNITS")])
x_test  <- scale(
  test_data[, c("SALE_PRICE", "GROSS_SQFT", "LAND_SQFT", "RES_UNITS")],
  center = attr(x_train, "scaled:center"),
  scale  = attr(x_train, "scaled:scale")
)
y_train <- train_data$NEIGHBORHOOD
y_test  <- test_data$NEIGHBORHOOD

knn_pred <- knn(train = x_train, test = x_test, cl = y_train, k = 7)
table(knn_pred, y_test)
confusionMatrix(knn_pred, y_test)
# ---------------------

# --- Model 2 (Random Forest) --- 
set.seed(123)
rf_model <- randomForest(
  NEIGHBORHOOD ~ SALE_PRICE + GROSS_SQFT + LAND_SQFT + RES_UNITS,
  data = train_data,
  ntree = 200,
  mtry = 2
)

rf_pred <- predict(rf_model, test_data)

# Evaluation
table(rf_pred, test_data$NEIGHBORHOOD)
confusionMatrix(rf_pred, test_data$NEIGHBORHOOD)
# -------------------------------

# --- Model 3 (Naive Bayes) ---
nb_model <- naiveBayes(
  NEIGHBORHOOD ~ SALE_PRICE + GROSS_SQFT + LAND_SQFT + RES_UNITS,
  data = train_data
)

nb_pred <- predict(nb_model, test_data)

# Evaluation
table(nb_pred, test_data$NEIGHBORHOOD)
confusionMatrix(nb_pred, test_data$NEIGHBORHOOD)
# -----------------------------

# NOTE. 
# I used kNN, Random Forest, and Naive Bayes to do this. usin the contingency table and the subsequent overall statistics from the
# confusion matrices, we can compare accuracies. The Naive Bayes had the worst accuracy at 0.0363. The k-NN had the second best 
# accuracy at 0.1893. The best performin supervised learning model was the Random Forest model, which had an accuracy of 0.235.

# ------------------- Problem 2a -------------------

# NOTE. 
# The best performing model from 1c was Model 3, so that is what I will be using. I additionally have done the same data cleaning
# steps as before since the code is now being run for a different bororugh—"QUEENS" is what I selected.

# Data Cleaning
df_queens <- df[df$BOROUGH == "QUEENS", ]
all_vars <- c( # Same as Model 3
  "SALE PRICE", "BLOCK", "LOT", "RESIDENTIAL UNITS", "COMMERCIAL UNITS",
  "TOTAL UNITS", "LAND SQUARE FEET", "GROSS SQUARE FEET", "YEAR BUILT",
  "Latitude", "Longitude", "Community Board", "Council District",
  "Census Tract", "BIN", "BBL"
)
df_q_raw <- df_queens[, all_vars]
df_q_raw[df_q_raw == ""] <- NA
df_q_raw <- na.omit(df_q_raw)
df_q_numeric <- data.frame(
  lapply(df_q_raw, function(col) as.numeric(gsub("[^0-9.-]", "", col)))
)
df_q_numeric <- data.frame(
  lapply(df_q_numeric, function(col) col[is.finite(col)])
)

# NOTE. 
# Note that we are using the same model, so no new fit is performed. This is analagous to an Out-of-sample test.

q_predictions <- predict(mlr_model_3, newdata = df_q_numeric)
q_residuals <- df_q_numeric$SALE.PRICE - q_predictions

# Plot: Prediction vs Actual
plot(df_q_numeric$SALE.PRICE, q_predictions,
     main = "Predicted vs Actual Sale Price (Queens)",
     xlab = "Actual Sale Price",
     ylab = "Predicted Sale Price",
     col = "blue", pch = 16)
abline(0, 1, col = "red", lwd = 2)

# Plot: Residuals vs Predicted
plot(q_predictions, q_residuals,
     main = "Residuals vs Predicted Sale Price (Queens)",
     xlab = "Predicted Sale Price",
     ylab = "Residuals",
     col = "darkgreen", pch = 16)
abline(h = 0, col = "red", lwd = 2)

# NOTE. 
# The best performing model from 1c was used to predict data from a different borough, and those were the predictions in
# the above plot. There exists substantial deviation from the 45 degree reference line drawn there. Additionally, the
# residual plot displays no randomness and instead a clear linear pattern. This is not desirable. Combined, this suggests 
# that the model, which worked for Manhattan, does not work well for Queens. If I were to speculate, I would say there is
# some inherent structure and correlation between features that is borough-specific. As such, when we fit on one borough 
# and predict on another, the model is not likely to capture those differeneces, thereby performing poorly.

# ------------------- Problem 2b -------------------

# Data Prep & Cleaning
df_queens_cls <- df[df$BOROUGH == "QUEENS", ]
cls_vars <- c("NEIGHBORHOOD", "SALE PRICE", "GROSS SQUARE FEET", "LAND SQUARE FEET", "RESIDENTIAL UNITS")
df_queens_cls <- df_queens_cls[, cls_vars]
df_queens_cls[df_queens_cls == ""] <- NA
df_queens_cls <- na.omit(df_queens_cls)
df_queens_cls <- data.frame(
  NEIGHBORHOOD = df_queens_cls$NEIGHBORHOOD,
  SALE_PRICE    = as.numeric(gsub("[^0-9.-]", "", df_queens_cls$`SALE PRICE`)),
  GROSS_SQFT    = as.numeric(gsub("[^0-9.-]", "", df_queens_cls$`GROSS SQUARE FEET`)),
  LAND_SQFT     = as.numeric(gsub("[^0-9.-]", "", df_queens_cls$`LAND SQUARE FEET`)),
  RES_UNITS     = as.numeric(gsub("[^0-9.-]", "", df_queens_cls$`RESIDENTIAL UNITS`))
)
x_test <- scale(df_queens_cls[, c("SALE_PRICE", "GROSS_SQFT", "LAND_SQFT", "RES_UNITS")],
                center = attr(x_train, "scaled:center"),
                scale  = attr(x_train, "scaled:scale"))
y_test <- df_queens_cls$NEIGHBORHOOD
all_levels <- union(levels(y_train), unique(y_test))
y_test_factor <- factor(y_test, levels = all_levels)

# --- Model 1 (kNN) --- 
knn_pred_q <- knn(train = x_train, test = x_test, cl = y_train, k = 7)
knn_pred_factor <- factor(knn_pred_q, levels = all_levels)

cat("\n--- k-NN Confusion Matrix (Queens) ---\n")
print(table(knn_pred_factor, y_test_factor))
print(confusionMatrix(knn_pred_factor, y_test_factor))
# ---------------------

# --- Model 2 (Random Forest) --- 
rf_pred_q <- predict(rf_model, df_queens_cls)
rf_pred_factor <- factor(rf_pred_q, levels = all_levels)

cat("\n--- Random Forest Confusion Matrix (Queens) ---\n")
print(table(rf_pred_factor, y_test_factor))
print(confusionMatrix(rf_pred_factor, y_test_factor))
# -------------------------------

# --- Model 3 (Naive Bayes) --- 
nb_pred_q <- predict(nb_model, df_queens_cls)
nb_pred_factor <- factor(nb_pred_q, levels = all_levels)

cat("\n--- Naive Bayes Confusion Matrix (Queens) ---\n")
print(table(nb_pred_factor, y_test_factor))
print(confusionMatrix(nb_pred_factor, y_test_factor))
# -----------------------------

# NOTE.
# When printing the summary stats of each Model from 1d trained on Manhattan data to predict neighbourhoods in Queens, all 3
# models exhbited zero predictive power with an accuracy of 0. This means that the models do not generalize at all to the new
# borough. If I were to speculate, then similar to what I said in 2a, the combinations of input features in Manhattan may have 
# been structurally different from in Queens. As a result, the predicitons on Queens may have been flawed, hence the 100% rate
# of misclassification.

# ------------------- Problem 2c -------------------

# NOTE.
# From 2a and 2b, it definitely seems like the boroughs each have their own structural differences between one another, which
# can explain the difference that we saw. Additionally, I observed something funky. In 1c, between models 2 and 3, the model 3
# that did not perform logistic regression performed better. I found this odd as the exploratory data analysis suggested that
# the transformation should have taken place (see the boxplots).