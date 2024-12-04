##########################################
### Principal Component Analysis (PCA) ###
##########################################

install.packages("ggfortify")
install.packages("psych")
library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with iris dataset
wine <- wine
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)



