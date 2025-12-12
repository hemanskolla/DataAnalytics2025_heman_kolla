library(readr)
library(EnvStats)
library(nortest)

setwd("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\DataAnalytics2025_heman_kolla")
epi.data <- read_csv("Lab1\\sample\\epi_results_2024_pop_gdp.csv")

regionA <- "Southern Asia"
regionB <- "Eastern Europe"

dfA <- subset(epi.data, region == regionA)
dfB <- subset(epi.data, region == regionB)

#1.1 Subset A
ECO <- dfA$ECO.new

NAs <- is.na(ECO)
ECO[which(NAs)]        
ECO.noNA <- ECO[!NAs]    

summary(ECO.noNA)
boxplot(ECO.noNA,
        main = "Boxplot of ECO in Southern Asia",
        ylab = "ECO values")

hist(ECO.noNA, prob=TRUE, 
     main="Histogram of ECO in Southern Asia with Density Line",
     xlab="ECO values",
     border="black")
lines(density(ECO.noNA), col="black", lwd=2)

#1.1 Subset B
ECO <- dfB$ECO.new

NAs <- is.na(ECO)
ECO[which(NAs)]        
ECO.noNA <- ECO[!NAs]    

summary(ECO.noNA)
boxplot(ECO.noNA,
        main = "Boxplot of ECO in Eastern Europe",
        ylab = "ECO values")

hist(ECO.noNA, prob=TRUE, 
     main="Histogram of ECO in Eastern Europe with Density Line",
     xlab="ECO values",
     border="black")
lines(density(ECO.noNA), col="black", lwd=2)

#1.2
ECO_A <- dfA$ECO.new
ECO_B <- dfB$ECO.new
ECO_A.noNA <- ECO_A[!is.na(ECO_A)]
ECO_B.noNA <- ECO_B[!is.na(ECO_B)]

qqplot(ECO_A.noNA, ECO_B.noNA,
       main = "QQ-Plot of ECO: Southern Asia vs Eastern Europe",
       xlab = "Southern Asia Quantiles",
       ylab = "Eastern Europe Quantiles",
       pch = 19, col = "black")
abline(0, 1, col = "blue", lwd = 2)  

#2.1
TCG <- epi.data$TCG.new

data_lm <- na.omit(data.frame(TCG = TCG,
                              gdp = epi.data$gdp,
                              population = epi.data$population))
data_lm$log_gdp <- log(data_lm$gdp)
data_lm$log_pop <- log(data_lm$population)

#2.1 Model 1
model1 <- lm(TCG ~ log_gdp, data = data_lm)
summary(model1)

plot(data_lm$log_gdp, data_lm$TCG,
     xlab = "log(GDP)",
     ylab = "TCG",
     main = "TCG vs log(GDP)")
abline(model1, col = "blue", lwd = 2)

plot(model1$fitted.values, model1$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Model 1)")
abline(h = 0, col = "blue")

# 2.1 Model 2
model2 <- lm(TCG ~ log_pop, data = data_lm)
summary(model2)

plot(data_lm$log_pop, data_lm$TCG,
     xlab = "log(Population)",
     ylab = "TCG",
     main = "TCG vs log(Population)")
abline(model2, col = "blue", lwd = 2)

plot(model2$fitted.values, model2$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Model 2)")
abline(h = 0, col = "blue")

# 2.2 Subset A
TCG_A <- dfA$TCG.new

data_lm_A <- na.omit(data.frame(TCG_A = TCG_A,
                                gdp_A = dfA$gdp,
                                population_A = dfA$population))
data_lm_A$log_gdp_A <- log(data_lm_A$gdp_A)
data_lm_A$log_pop_A <- log(data_lm_A$population_A)

# 2.2 Subset A Model 1
model1_A <- lm(TCG_A ~ log_gdp_A, data = data_lm_A)
summary(model1_A)

plot(data_lm_A$log_gdp_A, data_lm_A$TCG_A,
     xlab = "log(GDP)",
     ylab = "TCG",
     main = "TCG vs log(GDP) (Southern Asia)")
abline(model1_A, col = "blue", lwd = 2)

plot(model1_A$fitted.values, model1_A$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Model 1, Southern Asia)")
abline(h = 0, col = "blue")

# 2.2 Subset A Model 2 
model2_A <- lm(TCG_A ~ log_pop_A, data = data_lm_A)
summary(model2_A)

plot(data_lm_A$log_pop_A, data_lm_A$TCG_A,
     xlab = "log(Population)",
     ylab = "TCG",
     main = "TCG vs log(Population) (Southern Asia)")
abline(model2_A, col = "blue", lwd = 2)

plot(model2_A$fitted.values, model2_A$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Model 2, Southern Asia)")
abline(h = 0, col = "blue")

# 2.2 Justification
# Between the two models for Southern Asia, Model 1 (TCG ~ log(GDP)) is a better fit because it explains more variation 
# (high adjusted r^2 value) in TCG, though it has scattered residuals. Model 2 (TCG ~ log(Population)) does not explain 
# the variation with its negative adjusted r^2 value for TCG, and has even more scattered residuals.

# 3.1
library(class)

predictors <- epi.data[, c("BDH.new", "MKP.new", "PAR.new")]
labels <- epi.data$region
data_knn <- na.omit(data.frame(predictors, region = labels))

predictors_scaled <- scale(data_knn[, 1:3])
labels_knn <- data_knn$region
k <- 3 

pred_knn <- knn(train = predictors_scaled, test = predictors_scaled, cl = labels_knn, k = k)

conf_mat <- table(Predicted = pred_knn, Actual = labels_knn)
print(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat("Accuracy:", round(accuracy, 4), "\n")

# 3.2
predictors <- epi.data[, c("SPI.new", "TBN.new", "PAE.new")]
labels <- epi.data$region
data_knn <- na.omit(data.frame(predictors, region = labels))

predictors_scaled <- scale(data_knn[, 1:3])
labels_knn <- data_knn$region
k <- 3 

pred_knn <- knn(train = predictors_scaled, test = predictors_scaled, cl = labels_knn, k = k)

conf_mat <- table(Predicted = pred_knn, Actual = labels_knn)
print(conf_mat)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat("Accuracy:", round(accuracy, 4), "\n")

# 3.2 Justification
# Comparing the two kNN models, the first set of predictors (BDH.new, MKP.new, PAR.new) performed better, achieving 
# an accuracy of 0.6637, while the second set (SPI.new, TBN.new, PAE.new) only had an accuracy of 0.5407. This indicates 
# that the first model from 3.1 is better at classifying regions within the given dataset.
