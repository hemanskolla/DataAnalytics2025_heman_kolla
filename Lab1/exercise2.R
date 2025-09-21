# Exercise 2 Script

library(readr)
library(EnvStats)

setwd("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\DataAnalytics2025_heman_kolla\\Lab1")
epi.data <- read_csv("sample\\epi_results_2024_pop_gdp.csv")

# View(epi.data)
# summary(epi.data$ECO.new)
# summary(epi.data$TCG.new)
# epi.data$ECO.new
# epi.data$TCG.new

attach(epi.data)

ECO <- ECO.new
TCG <- TCG.new
# ECO
# TCG

# Check For & Remove NA's
NAs <- is.na(ECO)
ECO[which(NAs)]
ECO.noNA <- ECO[!NAs]
NAs <- is.na(TCG)
TCG[which(NAs)]
TCG.noNA <- TCG[!NAs]

# Variable Summaries
summary(ECO.noNA)
summary(TCG.noNA)

# Variable Boxplots
boxplot(ECO.noNA, TCG.noNA, 
        names = c("ECO", "TCG"),
        main = "Boxplots of ECO and TCG",
        ylab = "Values")

# Histogram for ECO
x_ECO <- seq(min(ECO.noNA), max(ECO.noNA), length=20)
hist(ECO.noNA, x_ECO, prob=TRUE, 
     main="Histogram of ECO with Theoretical Probability Distribution", 
     xlab="ECO values")
x1 <- seq(min(ECO.noNA), max(ECO.noNA), length=100)
d1 <- dnorm(x1, mean=mean(ECO.noNA), sd=sd(ECO.noNA))
lines(x1, d1)
# lines(density(ECO.noNA, na.rm=TRUE, bw="SJ"))
# rug(ECO.noNA)

# Histogram for TCG
x_TCG <- seq(min(TCG.noNA), max(TCG.noNA), length=20)
hist(TCG.noNA, x_TCG, prob=TRUE, 
     main="Histogram of TCG with Theoretical Probability Distribution", 
     xlab="TCG values")
x2 <- seq(min(TCG.noNA), max(TCG.noNA), length=100)
d2 <- dnorm(x2, mean=mean(TCG.noNA), sd=sd(TCG.noNA))
lines(x2, d2)
# lines(density(TCG.noNA, na.rm=TRUE, bw="SJ"))
# rug(TCG.noNA) 