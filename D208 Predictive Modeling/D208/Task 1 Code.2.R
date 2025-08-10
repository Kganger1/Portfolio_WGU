#Part C1.
# Install the tidyverse package
install.packages("tidyverse")
install.packages("caret")

#Load the ggplot2 and tidyverse library
library(tidyverse)
library(ggplot2)
library(caret)
library(magrittr)
library(readr)
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

# Remove outliers
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
#Zip formatted as a five-digit string with leading zeros if necessary
churn$Zip <- sprintf("%05d", churn$Zip)

# Convert categorical variables to factors
churn$Area <- as.factor(churn$Area)
churn$Gender <- as.factor(churn$Gender)
churn$Contract <- as.factor(churn$Contract)
churn$Port_modem <- as.factor(churn$Port_modem)
churn$Tablet <- as.factor(churn$Tablet)
churn$InternetService <- as.factor(churn$InternetService)
churn$Phone <- as.factor(churn$Phone)
churn$Multiple <- as.factor(churn$Multiple)
churn$OnlineSecurity <- as.factor(churn$OnlineSecurity)
churn$OnlineBackup <- as.factor(churn$OnlineBackup)
churn$DeviceProtection <- as.factor(churn$DeviceProtection)
churn$TechSupport <- as.factor(churn$TechSupport)
churn$StreamingTV <- as.factor(churn$StreamingTV)
churn$StreamingMovies <- as.factor(churn$StreamingMovies)

#Part C2.
summary(churn$MonthlyCharge)
summary(churn[c(
  "Tenure",
  "Children",
  "Age",
  "Email",
  "Population",
  "Bandwidth_GB_Year",
  "Port_modem",
  "Tablet",
  "InternetService",
  "Phone",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies"
)])

#Part C3.
# Univariate Visualization
ggplot(churn, aes(x = MonthlyCharge)) +
  geom_histogram(binwidth = 5,
                 fill = "#7AC5CD",
                 color = "black") +
  ggtitle("Distribution of Monthly Charge")

ggplot(churn, aes(x = Tenure)) +
  geom_histogram(binwidth = 5,
                 fill = "salmon",
                 color = "black") +
  ggtitle("Distribution of Tenure")

ggplot(churn, aes(x = Children)) +
  geom_histogram(binwidth = 1,
                 fill = "#7FFFD4",
                 color = "black") +
  ggtitle("Distribution of Children")

ggplot(churn, aes(x = Age)) +
  geom_histogram(binwidth = 5,
                 fill = "burlywood",
                 color = "black") +
  ggtitle("Distribution of Age")

ggplot(churn, aes(x = Email)) +
  geom_histogram(
    binwidth = 5,
    fill = "darkseagreen",
    color = "black"
  ) +
  ggtitle("Distribution of Email")

ggplot(churn, aes(x = Population)) +
  geom_histogram(binwidth = 10000,
                 fill = "chocolate",
                 color = "black") +
  ggtitle("Distribution of Population")

ggplot(churn, aes(x = Bandwidth_GB_Year)) +
  geom_histogram(
    binwidth = 200,
    fill = "darkslategray",
    color = "black"
  ) +
  ggtitle("Distribution of Bandwidth GB Year")

ggplot(churn, aes(x = Port_modem, fill = Port_modem)) +
  geom_bar() +
  ggtitle("Distribution of Port Modem") +
  scale_fill_manual(values = c("Yes" = "deepskyblue", "No" = "deepskyblue4"))

ggplot(churn, aes(x = Tablet, fill = Tablet)) +
  geom_bar() +
  ggtitle("Distribution of Tablet") +
  scale_fill_manual(values = c("Yes" = "honeydew", "No" = "honeydew4"))

ggplot(churn, aes(x = InternetService, fill = InternetService)) +
  geom_bar() +
  ggtitle("Distribution of Internet Service") +
  scale_fill_manual(
    values = c(
      "DSL" = "lemonchiffon3",
      "None" = "lemonchiffon",
      "Fiber Optic" = "lemonchiffon4"
    )
  )

ggplot(churn, aes(x = Phone, fill = Phone)) +
  geom_bar() +
  ggtitle("Distribution of Phone") +
  scale_fill_manual(values = c(
    "Yes" = "lavenderblush",
    "No" = "lavenderblush4"
  ))

ggplot(churn, aes(x = OnlineSecurity, fill = OnlineSecurity)) +
  geom_bar() +
  ggtitle("Distribution of Online Security") +
  scale_fill_manual(values = c("Yes" = "pink", "No" = "pink4"))

ggplot(churn, aes(x = OnlineBackup, fill = OnlineBackup)) +
  geom_bar() +
  ggtitle("Distribution of Online Backup") +
  scale_fill_manual(values = c("Yes" = "goldenrod1", "No" = "goldenrod4"))

ggplot(churn, aes(x = DeviceProtection, fill = DeviceProtection)) +
  geom_bar() +
  ggtitle("Distribution of Device Protection") +
  scale_fill_manual(values = c("Yes" = "green", "No" = "green4"))

ggplot(churn, aes(x = TechSupport, fill = TechSupport)) +
  geom_bar() +
  ggtitle("Distribution of Tech Support") +
  scale_fill_manual(values = c("Yes" = "snow", "No" = "snow4"))

ggplot(churn, aes(x = StreamingTV, fill = StreamingTV)) +
  geom_bar() +
  ggtitle("Distribution of Streaming TV") +
  scale_fill_manual(values = c("Yes" = "purple", "No" = "purple4"))

ggplot(churn, aes(x = StreamingMovies, fill = StreamingMovies)) +
  geom_bar() +
  ggtitle("Distribution of Streaming Movies") +
  scale_fill_manual(values = c("Yes" = "aquamarine4", "No" = "aquamarine"))

# Bivariate Visualizations between MonthlyCharge and other variables
ggplot(churn, aes(x = Tenure, y = MonthlyCharge)) +
  geom_point(color = "#7AC5CD") +
  ggtitle("Monthly Charge vs Tenure")

ggplot(churn, aes(x = Children, y = MonthlyCharge)) +
  geom_point(color = "#7FFFD4") +
  ggtitle("Monthly Charge vs Children")

ggplot(churn, aes(x = Age, y = MonthlyCharge)) +
  geom_point(color = "burlywood") +
  ggtitle("Monthly Charge vs Age")

ggplot(churn, aes(x = Email, y = MonthlyCharge)) +
  geom_point(color = "darkseagreen") +
  ggtitle("Monthly Charge vs Email")

ggplot(churn, aes(x = Population, y = MonthlyCharge)) +
  geom_point(color = "chocolate") +
  ggtitle("Monthly Charge vs Population")

ggplot(churn, aes(x = Bandwidth_GB_Year, y = MonthlyCharge)) +
  geom_point(color = "darkslategray") +
  ggtitle("Monthly Charge vs Bandwidth GB Year")

ggplot(churn, aes(x = Port_modem, y = MonthlyCharge, fill = Port_modem)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Port Modem") +
  scale_fill_manual(values = c("Yes" = "deepskyblue", "No" = "deepskyblue4"))

ggplot(churn, aes(x = Tablet, y = MonthlyCharge, fill = Tablet)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Tablet") +
  scale_fill_manual(values = c("Yes" = "honeydew", "No" = "honeydew4"))

ggplot(churn,
       aes(x = InternetService, y = MonthlyCharge, fill = InternetService)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Internet Service") +
  scale_fill_manual(
    values = c(
      "DSL" = "lemonchiffon3",
      "None" = "lemonchiffon",
      "Fiber Optic" = "lemonchiffon4"
    )
  )

ggplot(churn, aes(x = Phone, y = MonthlyCharge, fill = Phone)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Phone") +
  scale_fill_manual(values = c(
    "Yes" = "lavenderblush",
    "No" = "lavenderblush4"
  ))

ggplot(churn,
       aes(x = OnlineSecurity, y = MonthlyCharge, fill = OnlineSecurity)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Online Security") +
  scale_fill_manual(values = c("Yes" = "pink", "No" = "pink4"))

ggplot(churn,
       aes(x = OnlineBackup, y = MonthlyCharge, fill = OnlineBackup)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Online Backup") +
  scale_fill_manual(values = c("Yes" = "goldenrod1", "No" = "goldenrod4"))

ggplot(churn,
       aes(x = DeviceProtection, y = MonthlyCharge, fill = DeviceProtection)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Device Protection") +
  scale_fill_manual(values = c("Yes" = "green", "No" = "green4"))

ggplot(churn, aes(x = TechSupport, y = MonthlyCharge, fill = TechSupport)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Tech Support") +
  scale_fill_manual(values = c("Yes" = "snow", "No" = "snow4"))

ggplot(churn, aes(x = StreamingTV, y = MonthlyCharge, fill = StreamingTV)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Streaming TV") +
  scale_fill_manual(values = c("Yes" = "purple", "No" = "purple4"))

ggplot(churn,
       aes(x = StreamingMovies, y = MonthlyCharge, fill = StreamingMovies)) +
  geom_boxplot() +
  ggtitle("Monthly Charge vs Streaming Movies") +
  scale_fill_manual(values = c("Yes" = "aquamarine4", "No" = "aquamarine"))

# Part C4.
# Apply label encoding to other categorical variables
churn <- churn %>%
  mutate(
    Port_modem = ifelse(Port_modem == "Yes", 1, 0),
    Tablet = ifelse(Tablet == "Yes", 1, 0),
    Phone = ifelse(Phone == "Yes", 1, 0),
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

# Rename the column for better coding
churn <- churn %>%
  rename(
    InternetService_None = `InternetServiceNone`,
    InternetService_Fiber_Optic = `InternetServiceFiber Optic`,
    InternetService_DSL = `InternetServiceDSL`
  )

# Subset of the Dataset with the variables for my initial model
# Exclude reference categories for dummy variables
variables_used <- c(
  "MonthlyCharge",
  "Tenure",
  "Children",
  "Age",
  "Email",
  "Population",
  "Bandwidth_GB_Year",
  "Port_modem",
  "Tablet",
  "InternetService_Fiber_Optic",
  "InternetService_DSL",
  "Phone",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies"
)
task_1 <- churn[variables_used]
write.csv(task_1, file = "C:/Users/kiara/OneDrive/WGU MSDA/D208/task_1.csv", row.names = FALSE)
task_1 <- read.csv("C:/Users/kiara/OneDrive/WGU MSDA/D208/task_1.csv")
view(task_1)

   
# Part D1.
initial_model <- lm(
  MonthlyCharge ~ Tenure + Children + Age + Email + Population + Bandwidth_GB_Year + Port_modem + Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + Phone + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(initial_model)
vif(initial_model)

# Part D3.
# Remove Bandwidth_GB_Year
red_mod_01 <- lm(
  MonthlyCharge ~ Tenure + Children + Age + Email + Population + Port_modem + Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + Phone + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_01)
vif(red_mod_01)

# Remove Population
red_mod_02 <- lm(
  MonthlyCharge ~ Tenure + Children + Age + Email + Port_modem + Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + Phone + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_02)
vif(red_mod_02)

# Remove Age
red_mod_03 <- lm(
  MonthlyCharge ~ Tenure + Children + Email + Port_modem + Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + Phone + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_03)
vif(red_mod_03)

# Remove Email
red_mod_04 <- lm(
  MonthlyCharge ~ Tenure + Children + Port_modem + Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + Phone + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_04)
vif(red_mod_04)

# Remove Phone
red_mod_05 <- lm(
  MonthlyCharge ~ Tenure + Children + Port_modem + Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_05)
vif(red_mod_05)

# Remove Port_modem
red_mod_06 <- lm(
  MonthlyCharge ~ Tenure + Children + Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_06)
vif(red_mod_06)

# Remove Tenure
red_mod_07 <- lm(
  MonthlyCharge ~ Children + Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_07)
vif(red_mod_07)

# Remove Children
red_mod_08 <- lm(
  MonthlyCharge ~ Tablet +
    InternetService_Fiber_Optic + InternetService_DSL + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_08)
vif(red_mod_08)

# Remove Tablet
red_mod_09 <- lm(
  MonthlyCharge ~ InternetService_Fiber_Optic + InternetService_DSL + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(red_mod_09)
vif(red_mod_09)


# Part E2.
final_model <- lm(
  MonthlyCharge ~ InternetService_Fiber_Optic + InternetService_DSL + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies,
  data = task_1
)
summary(final_model)
vif(final_model)

# Create the residual plot for the reduced model
plot(fitted(final_model), residuals(final_model), 
     main = "Residual Plot for Reduced Model", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red")

# Generate the residuals from the final reduced model
residuals_final_model <- residuals(final_model)
# Create Q-Q plot for residuals
qqplot <- ggplot(data = data.frame(residuals = residuals_final_model), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")
# Print the Q-Q plot
print(qqplot)