################################################################################
################################################################################
### Supplementary material for: 
### Jacks et al., (2021)
### Title: Bioassessment of the ecological integrity of freshwater ecosystems using aquatic macroinvertebrates: The case of Sable Island National Park Reserve, Canada
################################################################################
setwd("C:\\Users\\Dal User\\Desktop\\Sable\\R")
# Load the required packages
# (vegan must be loaded after ade4 to avoid some conflicts)
library(ade4)
library(vegan)
library(gclus)
library(ape)
library(FactoMineR)
# Load additionnal functions
# (files must be in the working directory)
source("cleanplot.pca.R")


################################################################################
###the variation of water quality parameters (Supplementary Figure S1)
# Import the data from CSV files
# (files must be in the working directory)
# envdata
gr <- read.csv("PCAs2.csv", row.names=1)
env <- read.csv("PCAs.csv", row.names=1)
sitetype <- as.numeric(gr$factor)
season <- as.numeric(gr$season)
# PCA on the full environmental dataset
# *************************************
env.pca <- rda(env, scale=TRUE)
env.pca
summary(env.pca) # Default scaling 2
summary(env.pca, scaling=1)
# Plots using biplot
#dev.new(width=24, height=12, title="PCA biplots - environmental variables - biplot.rda")
#par(mfrow=c(1,2))
biplot(env.pca, scaling=2, main="PCA - scaling 2", display = "sites", col = "white")
text(env.pca, labels=spe$sitefinal, cex= 0.7, pos=3)
biplot(env.pca, scaling = 2,  main="PCA - scaling 2")  # Default scaling 2
points (env.pca, pch=19 , col=season)
legend("topright",legend = c("2015","2016","2017","2018","2019"), fill = 1:5)


######################################################################################################################
######################################################################################################################
### the variation of macroinvertebrate community structure for each sampling period (Figure 2) 

spe <- read.csv("Sablebiota4.csv", row.names=1)
# Hellinger pre-transformation of the species data

species1 <- spe / rowSums(spe) * 100
#converts the averaged raw counts (these are averages of the 3 transect samples) to relative abundance
spe.h <- decostand(species1, "hellinger")
(spe.h.pca <- rda(spe.h))


# PCA biplots
spe.pca.sc1 <- scores(spe.h.pca, display="species", scaling=1)
spe.pca.sc2 <- scores(spe.h.pca, display="species", scaling=2)

#dev.new(title="PCA on fish species", width=12, height=6)
#par(mfrow=c(1,2))
cleanplot.pca(spe.h.pca, scaling=1, mar.percent=0.06)
cleanplot.pca(spe.h.pca, scaling=2, mar.percent=0.06)


########################################################################################################################
########################################################################################################################
### Visualisation of Diversity indices (S, H', and 1-lambda) and water quality data (Figure 5)

env2 <- read.csv("Sabledivenvfinal.csv", row.names=1)
env.pca <- rda(env2, scale=TRUE)
biplot(env.pca, main="PCA - scaling 2")  # Default scaling 2

