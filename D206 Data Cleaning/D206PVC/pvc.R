library(plyr)
install.packages("visdat")
library(visdat)
library(tidyverse)
install.packages("factoextra")
library(factoextra)

Churn <- read.csv(
  "C:/Users/kiara/OneDrive/WGU MSDA/D206/D 206 Task 1 Final Presentation/Clean_Churn.csv",
  row.names = 1
)
churn.pvc <- select(Churn,
                 Lat,
                 Lng,
                 Income,
                 Outage_sec_perweek,
                 Tenure,
                 MonthlyCharge,
                 Bandwidth_GB_Year)

pvc <- prcomp(churn.pvc[, c(1:7)], center = TRUE, scale. = TRUE)

