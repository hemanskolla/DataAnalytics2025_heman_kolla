# ------------------- IMPORTS -------------------

library(ggplot2)
library(readr)

# ------------------- LOAD DATA -------------------

setwd("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\DataAnalytics2025_heman_kolla\\Lab2")
df <- read_csv("sample\\NY-House-Dataset.csv")

# ------------------- CLEANING -------------------

# Remove rows with NA
df <- df[!is.na(df$BEDS), ]
df <- df[!is.na(df$BATH), ]
df <- df[!is.na(df$PROPERTYSQFT), ]
df <- df[!is.na(df$PRICE), ]

# Remove rows with empty strings
df <- df[df$BEDS != "", ]
df <- df[df$BATH != "", ]
df <- df[df$PROPERTYSQFT != "", ]
df <- df[df$PRICE != "", ]

# Function to remove outliers using IQR rule
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  x >= lower_bound & x <= upper_bound
}

# Apply to each variable
df <- df[remove_outliers(df$BEDS), ]
df <- df[remove_outliers(df$BATH), ]
df <- df[remove_outliers(df$PROPERTYSQFT), ]

# Convert variables to numeric
df$BEDS <- as.numeric(df$BEDS)
df$BATH <- as.numeric(df$BATH)
df$PROPERTYSQFT <- as.numeric(df$PROPERTYSQFT)
df$PRICE <- as.numeric(df$PRICE)

print("CLEANED DATAFRAME:")
print(head(df))


# ===========================================================
# ------------- MODEL 1: BEDS + BATH on log(PRICE) ----------
# ===========================================================

ggplot(df, aes(x = BEDS, y = log(PRICE))) + geom_point()
ggplot(df, aes(x = BATH, y = log(PRICE))) + geom_point()

# Initialize Linear model
lin.mod1 <- lm(log(PRICE) ~ BEDS + BATH, df)

summary(lin.mod1)

# Most significant variable vs. log(PRICE) with the best fit line: BATH
ggplot(df, aes(x = BATH, y = log(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm")

# Residual plot
ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residual vs Fitted (Model 1)",
       x = "Fitted Values",
       y = "Residuals")

# ===========================================================
# -------- MODEL 2: BEDS & PROPERTYSQFT on log(PRICE) -------
# ===========================================================

ggplot(df, aes(x = BEDS, y = log(PRICE))) + geom_point()
ggplot(df, aes(x = PROPERTYSQFT, y = log(PRICE))) + geom_point()

# Initialize Linear model
lin.mod2 <- lm(log(PRICE) ~ BEDS + PROPERTYSQFT, df)

summary(lin.mod2)

# Most significant variable vs. log(PRICE) with the best fit line: PROPERTYSQFT
ggplot(df, aes(x = PROPERTYSQFT, y = log(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm")

# Residual plot
ggplot(lin.mod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residual vs Fitted (Model 2)",
       x = "Fitted Values",
       y = "Residuals")


# ===========================================================
# ---- MODEL 3: BATH + PROPERTYSQFT on log(PRICE) -----------
# ===========================================================

ggplot(df, aes(x = BATH, y = log(PRICE))) + geom_point()
ggplot(df, aes(x = PROPERTYSQFT, y = log(PRICE))) + geom_point()

# Initialize Linear model
lin.mod3 <- lm(log(PRICE) ~ BATH + PROPERTYSQFT, df)

summary(lin.mod3)

# Most significant variable vs. log(PRICE) with the best fit line: BATH
ggplot(df, aes(x = BATH, y = log(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm")

# Residual plot
ggplot(lin.mod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residual vs Fitted (Model 3)",
       x = "Fitted Values",
       y = "Residuals")