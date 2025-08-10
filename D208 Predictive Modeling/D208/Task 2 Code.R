#Part C1.
# Install the tidyverse package
install.packages("tidyverse")
install.packages("caret")
install.packages("car")

#Load the ggplot2 and tidyverse library
library(tidyverse)
library(ggplot2)
library(caret)
library(magrittr)
library(car)

#Import the churn data
churn <-
  read.csv("C:/Users/kiara/OneDrive/WGU MSDA/churn_clean.csv")
View(churn)

#Getting to know the DATA
summary(churn)

# Detecting Duplicates
duplicates <- duplicated(churn)
duplicate_count <- sum(duplicates)
#Print the results
print(duplicate_count)

# Missing count per column
colSums(is.na(churn))
#treating hidden missing values
pop_percentage_of_zeros <- sum(churn$Population == 0) / nrow(churn) * 100
print(pop_percentage_of_zeros)
#since under 1% drop
churn <- subset(churn, Population != 0)

# Remove Outliers
convars <- c(
  'Tenure',
  'Children',
  'Age',
  'Income',
  'Email',
  'Population',
  'Outage_sec_perweek',
  'Contacts',
  'Yearly_equip_failure',
  'Bandwidth_GB_Year',
  'MonthlyCharge'
)
churn <- churn %>%
  filter(if_any(all_of(convars), ~ abs(. - mean(.)) <= 3 * sd(.)))

#the renaming of the survey columns Item1, Item2 etc.
churn %<>% rename(
  Timely_Response = Item1,
  Timely_Fixes = Item2,
  Timely_Replacements = Item3,
  Reliability = Item4,
  Options = Item5,
  Respectful_Response = Item6,
  Courteous_Exchange = Item7,
  Active_Listening = Item8
)
#Zip formatted as a five-digit string with leading zeros
churn$Zip <- sprintf("%05d", churn$Zip)

# List of all categorical variables
categorical_vars <- c(
  "Churn",
  "Contract",
  "InternetService",
  "City",
  "State",
  "County",
  "Zip",
  "Area",
  "TimeZone",
  "Job",
  "Marital",
  "Gender",
  "Techie",
  "Port_modem",
  "Tablet",
  "Phone",
  "Multiple",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "PaperlessBilling",
  "PaymentMethod",
  "Timely_Response",
  "Timely_Fixes",
  "Timely_Replacements",
  "Reliability",
  "Options",
  "Respectful_Response",
  "Courteous_Exchange",
  "Active_Listening"
)
# Convert all categorical variables to factors
churn <- churn %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Part C2.
summary(churn$Churn)
summary(churn[c(
  "Population",
  "Children",
  "Age",
  "Income",
  "Outage_sec_perweek",
  "Email",
  "Contacts",
  "Yearly_equip_failure",
  "Tenure",
  "MonthlyCharge",
  "Bandwidth_GB_Year",
  "Timely_Response",
  "Timely_Fixes",
  "Timely_Replacements",
  "Reliability",
  "Options",
  "Respectful_Response",
  "Courteous_Exchange",
  "Active_Listening"
)])

# Part C3.
# Univariate visualizations for selected variables
ggplot(churn, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  ggtitle("Distribution of Churn") +
  scale_fill_manual(values = c("Yes" = "skyblue", "No" = "yellow"))

ggplot(churn, aes(x = Population)) +
  geom_histogram(binwidth = 10000, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Population")

ggplot(churn, aes(x = Children)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black") +
  ggtitle("Distribution of Children")

ggplot(churn, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "pink", color = "black") +
  ggtitle("Distribution of Age")

ggplot(churn, aes(x = Income)) +
  geom_histogram(binwidth = 10000, fill = "green", color = "black") +
  ggtitle("Distribution of Income")

ggplot(churn, aes(x = Outage_sec_perweek)) +
  geom_histogram(binwidth = .75, fill = "purple", color = "black") +
  ggtitle("Distribution of Outage Seconds per Week")

ggplot(churn, aes(x = Email)) +
  geom_histogram(binwidth = 2, fill = "green4", color = "black") +
  ggtitle("Distribution of Email")

ggplot(churn, aes(x = Contacts)) +
  geom_histogram(binwidth = 2, fill = "thistle", color = "black") +
  ggtitle("Distribution of Contacts")

ggplot(churn, aes(x = Yearly_equip_failure)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  ggtitle("Distribution of Yearly Equipment Failure")

ggplot(churn, aes(x = Tenure)) +
  geom_histogram(binwidth = 5, fill = "#7FFFD4", color = "black") +
  ggtitle("Distribution of Tenure")

ggplot(churn, aes(x = MonthlyCharge)) +
  geom_histogram(binwidth = 5, fill = "#7AC5CD", color = "black") +
  ggtitle("Distribution of Monthly Charge")

ggplot(churn, aes(x = Bandwidth_GB_Year)) +
  geom_histogram(binwidth = 200, fill = "burlywood", color = "black") +
  ggtitle("Distribution of Bandwidth GB Year")

ggplot(churn, aes(x = Timely_Response)) +
  geom_histogram(binwidth = 1, fill = "darkseagreen", color = "black") +
  ggtitle("Distribution of Timely_Response")

ggplot(churn, aes(x = Timely_Fixes)) +
  geom_histogram(binwidth = 1, fill = "chocolate", color = "black") +
  ggtitle("Distribution of Timely_Fixes")

ggplot(churn, aes(x = Timely_Replacements)) +
  geom_histogram(binwidth = 1, fill = "darkslategray", color = "black") +
  ggtitle("Distribution of Timely_Replacements")

ggplot(churn, aes(x = Reliability)) +
  geom_histogram(binwidth = 1, fill = "deepskyblue", color = "black") +
  ggtitle("Distribution of Reliability")

ggplot(churn, aes(x = Options)) +
  geom_histogram(binwidth = 1, fill = "honeydew3", color = "black") +
  ggtitle("Distribution of Options")

ggplot(churn, aes(x = Respectful_Response)) +
  geom_histogram(binwidth = 1, fill = "lemonchiffon3", color = "black") +
  ggtitle("Distribution of Respectful_Response")

ggplot(churn, aes(x = Courteous_Exchange)) +
  geom_histogram(binwidth = 1, fill = "lavenderblush", color = "black") +
  ggtitle("Distribution of Courteous_Exchange")

ggplot(churn, aes(x = Active_Listening)) +
  geom_histogram(binwidth = 1, fill = "goldenrod1", color = "black") +
  ggtitle("Distribution of Active_Listening")


# Bivariate visualizations including the dependent variable
ggplot(churn, aes(x = Population, fill = Churn)) +
  geom_histogram(binwidth = 10000, color = "black") +
  ggtitle("Churn Vs Population")

# Continuous variables
ggplot(churn, aes(x = Children, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Children") +
  scale_fill_manual(values = c("Yes" = "pink", "No" = "skyblue"))

ggplot(churn, aes(x = Age, fill = Churn)) +
  geom_histogram(binwidth = 5, color = "black") +
  ggtitle("Age vs Churn") +
  scale_fill_manual(values = c("Yes" = "white", "No" = "pink"))

ggplot(churn, aes(x = Income, fill = Churn)) +
  geom_histogram(binwidth = 15000, position = "dodge") +
  ggtitle("Income vs Churn") +
  scale_fill_manual(values = c("Yes" = "white", "No" = "green"))

ggplot(churn, aes(x = Outage_sec_perweek, fill = Churn)) +
  geom_histogram(binwidth = 100, position = "dodge") +
  ggtitle("Outage Seconds per Week vs Churn")+
  scale_fill_manual(values = c("Yes" = "black", "No" = "purple"))

ggplot(churn, aes(x = Email, fill = Churn)) +
  geom_histogram(binwidth = 2, color = "black") +
  ggtitle("Churn vs Email") +
  scale_fill_manual(values = c("Yes" = "snow", "No" = "green4"))

ggplot(churn, aes(x = Contacts, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Contacts") +
  scale_fill_manual(values = c("Yes" = "snow", "No" = "thistle"))

ggplot(churn, aes(x = Yearly_equip_failure, fill = Churn)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  ggtitle("Yearly Equipment Failure vs Churn")+
  scale_fill_manual(values = c("Yes" = "azure", "No" = "salmon3"))

ggplot(churn, aes(x = Tenure, fill = Churn)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  ggtitle("Tenure vs Churn")+
  scale_fill_manual(values = c("Yes" = "aquamarine4", "No" = "#7FFFD4"))

ggplot(churn, aes(x = MonthlyCharge, fill = Churn)) +
  geom_histogram(binwidth = 15, position = "dodge") +
  ggtitle("Monthly Charge vs Churn")+
  scale_fill_manual(values = c("Yes" = "#5F9EA0", "No" = "#7AC5CD"))

ggplot(churn, aes(x = Bandwidth_GB_Year, fill = Churn)) +
  geom_histogram(binwidth = 400, position = "dodge") +
  ggtitle("Bandwidth GB Year vs Churn")+
  scale_fill_manual(values = c("Yes" = "burlywood", "No" = "burlywood4"))

ggplot(churn, aes(x = Timely_Response, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Timely_Response") +
  scale_fill_manual(values = c("Yes" = "darkseagreen", "No" = "#698B69"))

ggplot(churn, aes(x = Timely_Fixes, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Timely_Fixes") +
  scale_fill_manual(values = c("Yes" = "chocolate4", "No" = "chocolate"))

ggplot(churn, aes(x = Timely_Replacements, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Timely_Replacements") +
  scale_fill_manual(values = c("Yes" = "darkslategray", "No" = "darkslategray1"))

ggplot(churn, aes(x = Reliability, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Reliability") +
  scale_fill_manual(values = c("Yes" = "deepskyblue", "No" = "deepskyblue4"))

ggplot(churn, aes(x = Options, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Options") +
  scale_fill_manual(values = c("Yes" = "honeydew", "No" = "honeydew3"))

ggplot(churn, aes(x = Respectful_Response, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Respectful_Response") +
  scale_fill_manual(values = c("Yes" = "lemonchiffon", "No" = "lemonchiffon3"))

ggplot(churn, aes(x = Courteous_Exchange, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Courteous_Exchange") +
  scale_fill_manual(values = c("Yes" = "lavenderblush4", "No" = "lavenderblush"))

ggplot(churn, aes(x = Active_Listening, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  ggtitle("Churn vs Active_Listening") +
  scale_fill_manual(values = c("Yes" = "goldenrod1", "No" = "goldenrod4"))

# Part C4.
# DATA Transformation
# Ordinal encoding for binary variables (yes/no to 1/0)
# Convert Churn to binary numeric
churn$Churn <- ifelse(churn$Churn == "Yes", 1, 0)

base_model1 <- glm(
  Churn ~ Age + Income + Outage_sec_perweek + Tenure + MonthlyCharge + Bandwidth_GB_Year + Population + Children + Email + Contacts + Yearly_equip_failure + Timely_Response + Timely_Fixes + Timely_Replacements + Reliability + Options + Respectful_Response + Courteous_Exchange + Active_Listening,
  family = "binomial",
  data = churn
)
vif(base_model1)

# Remove Bandwidth_GB_Year
base_model2 <- glm(
  Churn ~ Age + Income + Outage_sec_perweek + Tenure + MonthlyCharge + Population + Children + Email + Contacts + Yearly_equip_failure + Timely_Response + Timely_Fixes + Timely_Replacements + Reliability + Options + Respectful_Response + Courteous_Exchange + Active_Listening,
  family = "binomial",
  data = churn
)
vif(base_model2)

variables_used <- c(
  "Churn",
  "Population",
  "Children",
  "Age",
  "Income",
  "Outage_sec_perweek",
  "Email",
  "Contacts",
  "Yearly_equip_failure",
  "Tenure",
  "MonthlyCharge",
  "Timely_Response",
  "Timely_Fixes",
  "Timely_Replacements",
  "Reliability",
  "Options",
  "Respectful_Response",
  "Courteous_Exchange",
  "Active_Listening"
)
task_2 <- churn[variables_used]
# Save task_2 as CSV file in the specified path
#write.csv(task_2, file = "C:/Users/kiara/OneDrive/WGU MSDA/D208/task_2.csv", row.names = FALSE)
task_2 <- read.csv("C:/Users/kiara/OneDrive/WGU MSDA/D208/task_2.csv")
view(task_2)

# Part D1
# Initial Model
ini_mod <- glm(
  Churn ~ Age + Income + Outage_sec_perweek + Tenure + MonthlyCharge + Population + Children + Email + Contacts + Yearly_equip_failure + Timely_Response + Timely_Fixes + Timely_Replacements + Reliability + Options + Respectful_Response + Courteous_Exchange + Active_Listening,
  family = "binomial",
  data = task_2
)
summary(ini_mod)
vif(ini_mod)

# Part D3
# Remove Timely Replacement
red_mod_01 <- glm(
  Churn ~ Age + Income + Outage_sec_perweek + Tenure + MonthlyCharge + Population + Children + Email + Contacts + Yearly_equip_failure + Timely_Response + Timely_Fixes + Reliability + Options + Respectful_Response + Courteous_Exchange + Active_Listening,
  family = "binomial",
  data = task_2
)
summary(red_mod_01)
vif(red_mod_01)

# Remove Respectful_Response
red_mod_02 <- glm(
  Churn ~ Age + Income + Outage_sec_perweek + Tenure + MonthlyCharge + Population + Children + Email + Contacts + Yearly_equip_failure + Timely_Response + Timely_Fixes + Reliability + Options + Courteous_Exchange + Active_Listening,
  family = "binomial",
  data = task_2
)
summary(red_mod_02)
vif(red_mod_02)

# Remove Active_Listening
red_mod_03 <- glm(
  Churn ~ Age + Income + Outage_sec_perweek + Tenure + MonthlyCharge + Population + Children + Email + Contacts + Yearly_equip_failure + Timely_Response + Timely_Fixes + Reliability + Options + Courteous_Exchange,
  family = "binomial",
  data = task_2
)
summary(red_mod_03)
vif(red_mod_03)

# Remove Email
red_mod_04 <- glm(
  Churn ~ Age + Income + Outage_sec_perweek + Tenure + MonthlyCharge + Population + Children + Contacts + Yearly_equip_failure + Timely_Response + Timely_Fixes + Reliability + Options + Courteous_Exchange,
  family = "binomial",
  data = task_2
)
summary(red_mod_04)
vif(red_mod_04)

# Remove Outage_sec_perweek
red_mod_05 <- glm(
  Churn ~ Age + Income + Tenure + MonthlyCharge + Population + Children + Contacts + Yearly_equip_failure + Timely_Response + Timely_Fixes + Reliability + Options + Courteous_Exchange,
  family = "binomial",
  data = task_2
)
summary(red_mod_05)
vif(red_mod_05)

# Remove Timely_Fixes
red_mod_06 <- glm(
  Churn ~ Age + Income + Tenure + MonthlyCharge + Population + Children + Contacts + Yearly_equip_failure + Timely_Response + Reliability + Options + Courteous_Exchange,
  family = "binomial",
  data = task_2
)
summary(red_mod_06)
vif(red_mod_06)

# Remove Children
red_mod_07 <- glm(
  Churn ~ Age + Income + Tenure + MonthlyCharge + Population + Contacts + Yearly_equip_failure + Timely_Response + Reliability + Options + Courteous_Exchange,
  family = "binomial",
  data = task_2
)
summary(red_mod_07)
vif(red_mod_07)

# Remove Yearly_equip_failure
red_mod_08 <- glm(
  Churn ~ Age + Income + Tenure + MonthlyCharge + Population + Contacts + Timely_Response + Reliability + Options + Courteous_Exchange,
  family = "binomial",
  data = task_2
)
summary(red_mod_08)
vif(red_mod_08)

# Remove Income
red_mod_09 <- glm(
  Churn ~ Age + Tenure + MonthlyCharge + Population + Contacts + Timely_Response + Reliability + Options + Courteous_Exchange,
  family = "binomial",
  data = task_2
)
summary(red_mod_09)
vif(red_mod_09)

# Remove Courteous_Exchange
red_mod_10 <- glm(
  Churn ~ Age + Tenure + MonthlyCharge + Population + Contacts + Timely_Response + Reliability + Options,
  family = "binomial",
  data = task_2
)
summary(red_mod_10)
vif(red_mod_10)

# Remove Options
red_mod_11 <- glm(
  Churn ~ Age + Tenure + MonthlyCharge + Population + Contacts + Timely_Response + Reliability,
  family = "binomial",
  data = task_2
)
summary(red_mod_11)
vif(red_mod_11)

# Remove Contacts
red_mod_12 <- glm(
  Churn ~ Age + Tenure + MonthlyCharge + Population + Timely_Response + Reliability,
  family = "binomial",
  data = task_2
)
summary(red_mod_12)
vif(red_mod_12)

# Remove Reliability
red_mod_13 <- glm(
  Churn ~ Age + Tenure + MonthlyCharge + Population + Timely_Response,
  family = "binomial",
  data = task_2
)
summary(red_mod_13)
vif(red_mod_13)

# Remove Age
red_mod_14 <- glm(
  Churn ~ Tenure + MonthlyCharge + Population + Timely_Response,
  family = "binomial",
  data = task_2
)
summary(red_mod_14)
vif(red_mod_14)

# Remove Population
red_mod_15 <- glm(
  Churn ~ Tenure + MonthlyCharge + Timely_Response,
  family = "binomial",
  data = task_2
)
summary(red_mod_15)
vif(red_mod_15)

# Remove Timely_Response
red_mod_16 <- glm(
  Churn ~ Tenure + MonthlyCharge,
  family = "binomial",
  data = task_2
)
summary(red_mod_16)
vif(red_mod_16)

# Confusion Matrix
predictions <- ifelse(predict(red_mod_16, type = "response") > 0.5, 1, 0)
# Creating a confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = task_2$Churn)
print(conf_matrix)


# Calculating accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)