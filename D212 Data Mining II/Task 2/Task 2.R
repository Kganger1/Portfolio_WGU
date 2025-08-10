churn <- read.csv("C:/Users/Kiara/OneDrive/WGU MSDA/D212 Data Mining II/churn_clean.csv")
View(churn)

# Select the continuous variables
continuous_vars <- churn[, c(
  "Tenure",
  "MonthlyCharge",
  "Bandwidth_GB_Year",
  "Outage_sec_perweek",
  "Income"
)]

# Standardize the continuous variables
standardized_data <- scale(continuous_vars)

# View the standardized data
head(standardized_data)

write.csv(
  standardized_data,
  "C:/Users/Kiara/OneDrive/WGU MSDA/D212 Data Mining II/Task 2/standardized_data.csv",
  row.names = FALSE
)

# Perform PCA on standardized data
pca_result <- prcomp(standardized_data, center = TRUE, scale. = TRUE)

# View the matrix of principal components
pca_result$x 

# Generate scree plot
screeplot(pca_result, type = "lines", main = "Scree Plot")

# Variance explained by the principal component selected in D2
explained_variance_first_two <- summary(pca_result)$importance[2, 1:2]
explained_variance_first_two

# Calculate the total variance captured by the first two components
total_variance_first_two <- sum(explained_variance_first_two)
total_variance_first_two


