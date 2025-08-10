#Install Libraries
library(tidyverse)
library(factoextra)
library(cluster, lib.loc = "C:/Program Files/R/R-4.4.1/library")

#Load the Dataset
churn <- read.csv("C:/Users/Kiara/OneDrive/WGU MSDA/D212 Data Mining II/churn_clean.csv")
View(churn)

# Inspect the data structure
str(churn)
summary(churn)

# Select continuous variables for clustering
cluster_data <- churn %>%
  select(Tenure, MonthlyCharge, Bandwidth_GB_Year) %>%
  na.omit()  # Remove rows with missing values

# Standardize the continuous variables
cluster_data_scaled <- scale(cluster_data)

# Boxplot to identify potential outliers
boxplot(cluster_data)

# Save the cleaned data to the specified file path
write.csv(
  cluster_data_scaled,
  "C:/Users/Kiara/OneDrive/WGU MSDA/D212 Data Mining II/cleaned_cluster_data.csv",
  row.names = FALSE
)
View(cluster_data_scaled)

fviz_nbclust(cluster_data_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters (k)", y = "Total within-cluster sum of squares (WSS)")

# Perform k-means clustering with 4 clusters
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)

# View the cluster centers
kmeans_result$centers

# View the number of observations in each cluster
kmeans_result$size


# Calculate and print the total within-cluster sum of squares (WCSS)
wcss <- kmeans_result$tot.withinss
print(paste("The total within-cluster sum of squares (WCSS):", wcss))