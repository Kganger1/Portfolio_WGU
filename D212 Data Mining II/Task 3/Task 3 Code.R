# Part C1
#Step 1: Load library
library(tidyverse)
library(arules)

# Step 2: Load the data set
tmb <- read.csv(
  "C:/Users/kiara/OneDrive/WGU MSDA/D212 Data Mining II/Task 3/teleco_market_basket.csv"
)
View(tmb)

# Step 3: Remove completely empty rows from the set
tmb <- tmb[!apply(tmb, 1, function(row)
  all(is.na(row) | row == "")), ]
# Reset row numbers so they go 1,2,3
rownames(tmb) <- NULL

# Step 4: Find all the different Items in the data set
all_items <- as.vector(as.matrix(tmb))
distinct_items <- unique(all_items)
distinct_items

tmb <- tmb %>%
  mutate(TransactionID = row_number())

tmb_long <- tmb %>%
  pivot_longer(cols = starts_with("Item"), values_to = "Item") %>%
  filter(Item != "")

transactions <- as(split(tmb_long$Item, tmb_long$TransactionID), "transactions")
inspect(transactions[1:5])

# Convert transactions to matrix and then to data frame
basket_matrix <- as(transactions, "matrix")
basket <- as.data.frame(basket_matrix)

# View first few rows of the data frame as a table
head(basket)

write.csv(
  basket,
  "C:/Users/Kiara/OneDrive/WGU MSDA/D212 Data Mining II/Task 3/basket.csv",
  row.names = FALSE
)
View(basket)

# Part C2
# Run Apriori algorithm on transactions
arules <- apriori(
  basket,
  control = list(verbose = F),
  parameter = list(
    supp = 0.008,
    conf = 0.4,
    minlen = 2
  )
)

# Remove redundant rules
redundant_r <- is.redundant(arules)
refined_arules <- arules[!redundant_r]

#inspect top rules
inspect(head(sort(
  refined_arules, by = "lift", decreasing = T
), 10))

summary(refined_arules)