
# Load required libraries
library(ggplot2)
library(stats)
library(class)
library(cluster)

# Read the CSV file
epi <- epi2024results_DA_F24_lab03
attach(epi)

# 1. Variable Distributions

# Choose two regions and a variable of interest
region1 <- subset(epi_data, region == "Global West")
region2 <- subset(epi_data, region == "Asia-Pacific")
variable <- "EPI"

# Function to plot histogram with density line
plot_hist_density <- function(data, variable, main_title) {
  x <- data[[variable]]
  hist(x, seq(min(x), max(x), length.out=20), prob=TRUE, 
       main=main_title, xlab=variable)
  rug(x)
  lines(density(x, na.rm=TRUE, bw="SJ"), lwd=2)
  lines(density(x, na.rm=TRUE, bw=1), lwd=2)
}

# Plot for Region 1
plot_hist_density(region1, variable, paste("Distribution of", variable, "in Global West"))

# Plot for Region 2
plot_hist_density(region2, variable, paste("Distribution of", variable, "in Asia-Pacific"))

# QQ plots
plot(qqnorm(region1[[variable]]))
qqline(region1[[variable]])

plot(qqnorm(region2[[variable]]))
qqline(region2[[variable]])

# 2. Linear Models

# Linear model for all regions
model_all <- lm(EPI ~ AIR + HPE + BDH + ECO + HLT, data = epi)
summary(model_all)

# Plot the most significant variable against EPI
most_significant <- "ECO"  # Change this based on the summary output
plot(epi[[most_significant]], epi$EPI, 
     main = paste(most_significant, "vs EPI"),
     xlab = most_significant, ylab = "EPI")
abline(lm(EPI ~ epi[[most_significant]], data = epi), col = "red")

# Linear model for a subset of one region
model_subset <- lm(EPI ~ AIR + HPE + BDH + ECO + HLT, data = region2)
summary(model_subset)
plot(region2[[most_significant]], region2$EPI, 
     main = paste(most_significant, "vs EPI"),
     xlab = most_significant, ylab = "EPI")
abline(lm(EPI ~ region2[[most_significant]], data = region2), col = "red")

# 3. Classification (kNN)

library("e1071")
library("ggplot2")

# kNN model for 3 regions
regions_3 <- c("Global West", "Asia-Pacific", "Latin America & Caribbean")
regions <- c("Southern Asia", "Eastern Europe", "Former Soviet States")
subset <- subset(epi, region %in% regions)
subset_3 <- subset(epi, region %in% regions_3)
variables <- c("region", "AIR", "HPE", "BDH", "ECO", "HLT")

n = nrow(subset)
n
## training set indexes
train.indexes <- sample(n,n*.7)

## create training/test sets
subset_3.train <-subset_3[train.indexes]
subset_3.test <-subset_3[-train.indexes]

subset.train <-subset[train.indexes, variables]
subset.test <-subset[-train.indexes, variables]


subset
y <- subset.train$region

y
subset.test
subset.train

# Train and evaluate model
set.seed(123)
knn_pred <- knn(train = subset.train[2:6], test = subset.test[2:6], cl = y, k = 10)
knn_pred
length(y)
length(knn_pred)
nrow(subset.test)
# Create contingency matrix
contingency_matrix <- table(Predicted = knn_pred, Actual = subset.test$region, dnn = list('predicted', 'actual'))
print(contingency_matrix)

# Calculate accuracy
accuracy <- sum(diag(contingency_matrix)) / sum(contingency_matrix)
print(paste("Accuracy:", accuracy))

# kNN model for 3 other regions
regions_3_other <- c("Eastern Europe", "Sub-Saharan Africa", "Greater Middle East")
subset_3_other <- subset(epi_data, region %in% regions_3_other)

X_other <- subset_3_other[, variables]
y_other <- subset_3_other$region

# Train and evaluate model
set.seed(123)
knn_model_other <- knn(train = X_other, test = X_other, cl = y_other, k = 5)
confusion_matrix_other <- table(knn_model_other, y_other)
print(confusion_matrix_other)
accuracy_other <- sum(diag(confusion_matrix_other)) / sum(confusion_matrix_other)
print(paste("Accuracy:", accuracy_other))

# 4. Clustering

# K-means clustering for two groups of regions
regions_group1 <- c("Global West", "Asia-Pacific", "Latin America & Caribbean")
regions_group2 <- c("Eastern Europe", "Sub-Saharan Africa", "Greater Middle East")

subset_group1 <- subset(epi_data, region %in% regions_group1)
subset_group2 <- subset(epi_data, region %in% regions_group2)

kmeans_group1 <- kmeans(subset_group1[, variables], centers = 3)
kmeans_group2 <- kmeans(subset_group2[, variables], centers = 3)

print(paste("WCSS for Group 1:", kmeans_group1$tot.withinss))
print(paste("WCSS for Group 2:", kmeans_group2$tot.withinss))

# WCSS across different k values
k_values <- 1:10
wcss_group1 <- sapply(k_values, function(k) {
  kmeans(subset_group1[, variables], centers = k)$tot.withinss
})
wcss_group2 <- sapply(k_values, function(k) {
  kmeans(subset_group2[, variables], centers = k)$tot.withinss
})

# Plot WCSS across k values
plot(k_values, wcss_group1, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Optimal k (Group 1)")

plot(k_values, wcss_group2, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Optimal k (Group 2)")

