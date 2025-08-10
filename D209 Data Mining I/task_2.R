# Install Packages
install.packages("readr")
install.packages("dplyr")
install.packages("dummy")
install.packages("ggplot2")
install.packages("caret")
install.packages("randomForest")
install.packages("car")
install.packages("pROC")
# Load necessary libraries
library(readr)   # For reading data
library(dplyr)   # For data manipulation
library(dummy)   # For one-hot encoding
library(ggplot2) # For data visualization
library(caret)   # For model training and evaluation
library(randomForest)   # For Random Forest Classifier 
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
    OnlineSecurity = ifelse(OnlineSecurity == "Yes", 1, 0),
    OnlineBackup = ifelse(OnlineBackup == "Yes", 1, 0),
    DeviceProtection = ifelse(DeviceProtection == "Yes", 1, 0),
    TechSupport = ifelse(TechSupport == "Yes", 1, 0),
    StreamingTV = ifelse(StreamingTV == "Yes", 1, 0),
    StreamingMovies = ifelse(StreamingMovies == "Yes", 1, 0),
    PaperlessBilling = ifelse(PaperlessBilling == "Yes", 1, 0)
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
    Marital_Widowed = 'MaritalWidowed',
    Timely_Response = 'Item1',
    Timely_Fixes = 'Item2',
    Timely_Replacements = 'Item3',
    Reliability = 'Item4',
    Options = 'Item5',
    Respectful_Response = 'Item6',
    Courteous_Exchange = 'Item7',
    Active_Listening = 'Item8'
  )

# Subset of the Dataset with the variables for my initial model -1 dummy variable
variables_used <- c(
  "Churn",
  "Tenure",
  "MonthlyCharge",
  "Bandwidth_GB_Year",
  "Outage_sec_perweek",
  "Email",
  "Contacts",
  "Yearly_equip_failure",
  "Population",
  "Age",
  "Income",
  "Children",
  "Timely_Response",
  "Timely_Fixes",
  "Timely_Replacements",
  "Reliability",
  "Options",
  "Respectful_Response",
  "Courteous_Exchange",
  "Active_Listening",
  "Gender_Female",
  "Gender_Male",
  "Marital_Divorced",
  "Marital_Married",
  "Marital_Never_Married",
  "Marital_Separated",
  "Phone",
  "Multiple",
  "InternetService_Fiber_Optic",
  "InternetService_DSL",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "Contract_Monthly",
  "Contract_One_Year",
  "PaperlessBilling"
)
task_2 <- churn[variables_used]
View(task_2) 

# checking for correlation
ini_mod <- glm(
  Churn ~ .,
  family = "binomial",
  data = task_2
)
vif(ini_mod)

# Remove Bandwidth_GB_Year and refit the model
task_2 <- task_2 %>%
  select(-Bandwidth_GB_Year)
# Refit the model
ini_mod <- glm(
  Churn ~ .,
  family = "binomial",
  data = task_2
) 
vif(ini_mod)

# Remove MonthlyCharge and refit the model
task_2 <- task_2 %>%
  select(-MonthlyCharge)
# Refit the model
ini_mod <- glm(
  Churn ~ .,
  family = "binomial",
  data = task_2
)
vif(ini_mod)

# Remove Gender_Male and refit the model
task_2 <- task_2 %>%
  select(-Gender_Male)
# Refit the model
ini_mod <- glm(
  Churn ~ .,
  family = "binomial",
  data = task_2
)
vif(ini_mod)

# drop unneeded variables
variables_kept <- c(
  "Churn",
  "Tenure",
  "Outage_sec_perweek",
  "Email",
  "Contacts",
  "Yearly_equip_failure",
  "Population",
  "Age",
  "Income",
  "Children",
  "Timely_Response",
  "Timely_Fixes",
  "Timely_Replacements",
  "Reliability",
  "Options",
  "Respectful_Response",
  "Courteous_Exchange",
  "Active_Listening",
  "Gender_Female",
  "Marital_Divorced",
  "Marital_Married",
  "Marital_Never_Married",
  "Marital_Separated",
  "Phone",
  "Multiple",
  "InternetService_Fiber_Optic",
  "InternetService_DSL",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "Contract_Monthly",
  "Contract_One_Year",
  "PaperlessBilling"
)
task_2 <- task_2[variables_kept]
#write_csv(task_2, "C:/Users/kiara/OneDrive/WGU MSDA/D209/task_2.csv")
task_2 <- read_csv("C:/Users/kiara/OneDrive/WGU MSDA/D209/task_2.csv")
View(task_2)

# Split the data
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(task_2$Churn, p = 0.8, list = FALSE)
trainData2 <- task_2[trainIndex, ]
testData2 <- task_2[-trainIndex, ]

# Save the training and test datasets
write_csv(trainData2, "C:/Users/kiara/OneDrive/WGU MSDA/D209/traindata2.csv")
write_csv(testData2, "C:/Users/kiara/OneDrive/WGU MSDA/D209/testdata2.csv")

# Load training and test datasets
trainData2 <- read_csv("C:/Users/kiara/OneDrive/WGU MSDA/D209/traindata2.csv")
testData2 <- read_csv("C:/Users/kiara/OneDrive/WGU MSDA/D209/testdata2.csv")
View(testData2)
View(trainData2)

# Convert the Churn variable to a factor for classification
trainData2$Churn <- as.factor(trainData2$Churn)
testData2$Churn <- as.factor(testData2$Churn)

# Perform classification with random forest
model <- randomForest(
formula = Churn ~ .,
data = trainData2,
ntree = 500,    # Number of trees
mtry = floor(sqrt(ncol(trainData2) - 1)),  # Number of variables tried at each split
importance = TRUE
)

# Print the model summary
print(model)

# Plot model error rates
plot(model)

# Variable importance plot
varImpPlot(model)

# Predictions on test data
predictions <- predict(model, newdata = testData2)


# Ensure predictions are factors with the same levels as the actual data
predictions <- as.factor(predictions)
levels(predictions) <- levels(testData2$Churn)

# Confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(predictions, testData2$Churn)
print(conf_matrix)

# Check the number of predictors
num_predictors <- ncol(trainData2) - 1  # subtracting the response variable

# Ensure the mtry values are within the valid range
valid_mtry <- c(2, 4, 6, 8)
valid_mtry <- valid_mtry[valid_mtry <= num_predictors]

# Perform hyperparameter tuning using caret with a valid mtry range
control <- trainControl(method = "cv", number = 3)
tunegrid <- expand.grid(.mtry = valid_mtry)
set.seed(42)
rf_model <- train(Churn ~ ., data = trainData2, method = "rf",
trControl = control, tuneGrid = tunegrid)

# Best parameters from GridSearchCV
best_params <- rf_model$bestTune
print(paste("Best parameters: ", best_params))
best_score <- max(rf_model$results$Accuracy)
print(paste("Best Accuracy: ", best_score))

# Mean Squared Error (MSE) - For regression
mse <- mean((as.numeric(predictions) - as.numeric(testData2$Churn))^2)
rmse <- sqrt(mse)
rsquared <- 1 - (sum((as.numeric(predictions) - as.numeric(testData2$Churn))^2) / sum((mean(as.numeric(trainData2$Churn)) - as.numeric(testData2$Churn))^2))
print(paste("MSE: ", mse))
print(paste("RMSE: ", rmse))
print(paste("R-Squared: ", rsquared))

# Confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(predictions, testData2$Churn)
print(conf_matrix)

# Variable importance plot
varImpPlot(model)

