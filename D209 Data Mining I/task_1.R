# Install Packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("e1071")
install.packages("dummy")
install.packages("car")
# Load necessary libraries
library(readr)   # For reading data
library(dplyr)   # For data manipulation
library(ggplot2) # For data visualization
library(caret)   # For model training and evaluation
library(e1071)   # For Naive Bayes classifier
library(dummy)   # For one-hot encoding
library(car)     # For VIF calculation
library(pROC)    # For ROC curve analysis

# Load the dataset
churn <- read_csv('C:/Users/kiara/OneDrive/WGU MSDA/D209/churn_clean.csv')
View(churn)
# Inspect the data
head(data)
str(data)

# Inspect the data for missing, duplicated or outliers
sum(is.na(churn))
sum(duplicated(churn))
# Function to identify and remove outliers using IQR method
remove_outliers <- function(df, var) {
  Q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df <- df[df[[var]] >= lower_bound & df[[var]] <= upper_bound, ]
  return(df)
}

# Apply the function to numeric variables
numeric_vars <- c('Children', 'Income', 'Outage_sec_perweek', 'Yearly_equip_failure', 'Tenure', 'MonthlyCharge', 'Bandwidth_GB_Year')

for (var in numeric_vars) {
  churn <- remove_outliers(churn, var)
}

# Apply label encoding to other categorical variables
churn <- churn %>%
  mutate(
    Churn = ifelse(Churn == "Yes", 1, 0),
    Phone = ifelse(Phone == "Yes", 1, 0),
    Multiple = ifelse(Multiple == "Yes", 1, 0),
    PaperlessBilling = ifelse(PaperlessBilling == "Yes", 1, 0),
    OnlineSecurity = ifelse(OnlineSecurity == "Yes", 1, 0),
    OnlineBackup = ifelse(OnlineBackup == "Yes", 1, 0),
    DeviceProtection = ifelse(DeviceProtection == "Yes", 1, 0),
    TechSupport = ifelse(TechSupport == "Yes", 1, 0),
    StreamingTV = ifelse(StreamingTV == "Yes", 1, 0),
    StreamingMovies = ifelse(StreamingMovies == "Yes", 1, 0)
  )
# One Hot Encode
internet_dummies <- model.matrix( ~ InternetService - 1, data = churn)
churn <- cbind(churn, internet_dummies)
churn <- churn %>% select(-InternetService)

gender_dummies <- model.matrix( ~ Gender - 1, data = churn)
churn <- cbind(churn, gender_dummies)
churn <- churn %>% select(-Gender)

marital_dummies <- model.matrix( ~ Marital - 1, data = churn)
churn <- cbind(churn, marital_dummies)
churn <- churn %>% select(-Marital)

contract_dummies <- model.matrix( ~ Contract - 1, data = churn)
churn <- cbind(churn, contract_dummies)
churn <- churn %>% select(-Contract)

# Rename the column for better coding
churn <- churn %>%
  rename(
    InternetService_None = `InternetServiceNone`,
    InternetService_Fiber_Optic = `InternetServiceFiber Optic`,
    InternetService_DSL = `InternetServiceDSL`,
    Contract_Monthly = 'ContractMonth-to-month',
    Contract_One_Year = 'ContractOne year',
    Contract_Two_Year = 'ContractTwo Year',
    Gender_Female = 'GenderFemale',
    Gender_Male = 'GenderMale',
    Gender_Nonbinary = 'GenderNonbinary',
    Marital_Divorced = 'MaritalDivorced',
    Marital_Married = 'MaritalMarried',
    Marital_Never_Married = 'MaritalNever Married',
    Marital_Separated = 'MaritalSeparated',
    Marital_Widowed = 'MaritalWidowed'
  )

# Subset of the Dataset with the variables for my initial model -1 dummy variable
variables_used <- c(
  "Churn",
  "Children",
  "Income",
  "Outage_sec_perweek",
  "Yearly_equip_failure",
  "Phone",
  "Multiple",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "PaperlessBilling",
  "Tenure",
  "MonthlyCharge",
  "Bandwidth_GB_Year",
  "InternetService_DSL",
  "InternetService_Fiber_Optic",
  "Gender_Female",
  "Gender_Male",
  "Marital_Divorced",
  "Marital_Married",
  "Marital_Never_Married",
  "Marital_Separated",
  "Contract_Monthly",
  "Contract_One_Year"
)
task_1 <- churn[variables_used]
View(task_1)

# checking for correlation
ini_mod <- glm(
  Churn ~ .,
  family = "binomial",
  data = task_1
)
print(ini_mod)
vif(ini_mod)

ini_mod2 <- glm(
  Churn ~ Children + Income + Outage_sec_perweek + Yearly_equip_failure + Phone + Multiple +
    OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
    PaperlessBilling + Tenure + MonthlyCharge + InternetService_DSL + InternetService_Fiber_Optic +
    Gender_Female + Gender_Male + Marital_Divorced + Marital_Married + Marital_Never_Married + 
    Marital_Separated + Contract_Monthly + Contract_One_Year,
  family = "binomial",
  data = task_1
)
print(ini_mod2)
vif(ini_mod2)

ini_mod3 <- glm(
  Churn ~ Children + Income + Outage_sec_perweek + Yearly_equip_failure + Phone + Multiple +
    OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
    PaperlessBilling + Tenure + InternetService_DSL + InternetService_Fiber_Optic +
    Gender_Female + Gender_Male + Marital_Divorced + Marital_Married + Marital_Never_Married + 
    Marital_Separated + Contract_Monthly + Contract_One_Year,
  family = "binomial",
  data = task_1
)
print(ini_mod3)
vif(ini_mod3)

ini_mod4 <- glm(
  Churn ~ Children + Income + Outage_sec_perweek + Yearly_equip_failure + Phone + Multiple +
    OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
    PaperlessBilling + Tenure + InternetService_DSL + InternetService_Fiber_Optic +
    Gender_Female + Marital_Divorced + Marital_Married + Marital_Never_Married + 
    Marital_Separated + Contract_Monthly + Contract_One_Year,
  family = "binomial",
  data = task_1
)
print(ini_mod4)
vif(ini_mod4)


variables_used <- c(
  "Churn",
  "Children",
  "Income",
  "Outage_sec_perweek",
  "Yearly_equip_failure",
  "Phone",
  "Multiple",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "PaperlessBilling",
  "Tenure",
  "InternetService_DSL",
  "InternetService_Fiber_Optic",
  "Gender_Female",
  "Marital_Divorced",
  "Marital_Married",
  "Marital_Never_Married",
  "Marital_Separated",
  "Contract_Monthly",
  "Contract_One_Year"
)
task_1 <- task_1[variables_used]
#write_csv(task_1, "C:/Users/kiara/OneDrive/WGU MSDA/D209/task_1.csv")
task_1 <- read_csv("C:/Users/kiara/OneDrive/WGU MSDA/D209/task_1.csv")
View(task_1)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
trainIndex <- createDataPartition(task_1$Churn, p = .8, list = FALSE, times = 1)
trainData <- task_1[trainIndex, ]
testData <- task_1[-trainIndex, ]

# Save the training and testing datasets
write_csv(trainData, "C:/Users/kiara/OneDrive/WGU MSDA/D209/trainData.csv")
write_csv(testData, "C:/Users/kiara/OneDrive/WGU MSDA/D209/testData.csv")                    

# Load the saved files to verify
trainData <- read_csv("C:/Users/kiara/OneDrive/WGU MSDA/D209/trainData.csv")
testData <- read_csv("C:/Users/kiara/OneDrive/WGU MSDA/D209/testData.csv")

# Ensure the data is correctly loaded
print(head(trainData))
print(head(testData))

# Train the Naive Bayes model
nb_model <- naiveBayes(Churn ~ ., data = trainData)

# Predict on the test data
predictions <- predict(nb_model, testData)

# Ensure that both predictions and actual values are factors with the same levels
testData$Churn <- factor(testData$Churn)
predictions <- factor(predictions, levels = levels(testData$Churn))

# Evaluate the model performance
conf_matrix <- confusionMatrix(predictions, testData$Churn)

# Print the confusion matrix and its table
print(conf_matrix)
print(conf_matrix$table)

# Calculate and print the accuracy of the model
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy: ", round(accuracy * 100, 2), "%", sep = ""))

# Calculate and print the sensitivity and specificity
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
print(paste("Sensitivity: ", round(sensitivity * 100, 2), "%", sep = ""))
print(paste("Specificity: ", round(specificity * 100, 2), "%", sep = ""))

# Calculate and print the AUC
predictions_prob <- predict(nb_model, testData, type = "raw")
roc_obj <- roc(testData$Churn, predictions_prob[, 2])
auc_value <- auc(roc_obj)
print(paste("AUC: ", round(auc_value, 4)))

# Plot the ROC curve
plot(roc_obj, col = "#1c61b6", main = "ROC Curve for Naive Bayes Model")
abline(a = 0, b = 1, col = "red", lty = 2) # Add a diagonal line for reference