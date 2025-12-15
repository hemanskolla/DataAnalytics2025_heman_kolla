# ------------------- IMPORTS -------------------

library(ggplot2)
library(readr)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

# ------------------- LOAD DATA -------------------

setwd("C:\\Users\\kollah\\OneDrive - Rensselaer Polytechnic Institute\\Documents\\DataAnalytics2025_heman_kolla\\Lab2")
wine <- read_csv("sample\\wine.data", col_names = FALSE)

# ------------- PROVIDED DATA PREP ----------------

names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)
wine$Type <- as.factor(wine$Type)
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)
ggpairs(wine, ggplot2::aes(colour = Type))

