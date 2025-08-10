install.packages("ggplot2")
install.packages("tidytext")
install.packages("stringi")
install.packages("stringr")
install.packages("corpus")
install.packages("purr")
install.packages("lubridate")
install.packages("stopwords")
install.packages("qdap")
install.packages("SnowballC")
install.packages("tensorflow")
install.packages("keras")
install.packages("rmarkdown")
install.packages("tokenizers")
install.packages("tm")
install.packages("visdat")

library(ggplot2)
library(tidytext)
library(stringi)
library(stringr)
library(purrr)
library(lubridate)
library(stopwords)
library(qdap)
library(SnowballC)
library(tensorflow)
library(keras)
library(rmarkdown)
library(tokenizers)
library(tm)
library(visdat)
library(readr)


# Load datasets
imdb <- read_delim(
  "C:/Users/kiara/OneDrive/WGU MSDA/D213 Advanced Data Analytics/Task 2/sentiment labelled sentences/imdb_labelled.txt",
  delim = "\t",
  quote = "\\\"",
  escape_double = FALSE,
  locale = locale()
)
View(imdb)

adb <- read_delim(
  "C:/Users/kiara/OneDrive/WGU MSDA/D213 Advanced Data Analytics/Task 2/sentiment labelled sentences/amazon_cells_labelled.txt",
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
)
View(adb)

ydb <- read_delim(
  "C:/Users/kiara/OneDrive/WGU MSDA/D213 Advanced Data Analytics/Task 2/sentiment labelled sentences/yelp_labelled.txt",
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
)
View(ydb)

# Combing the Dataset with an Added source column
adb$source <- "Amazon"
imdb$source <- "IMDB"
ydb$source <- "Yelp"
combined_data <- rbind(adb, imdb, ydb)
View(combined_data)


# B. Data exploration and cleaning
sum(is.na(combined_data$review)) #Checking for NA in review
sum(is.na(combined_data$score)) #Checking for NA in score

# B1a. Presence of unusual characters
combined_data$review <- gsub("[^a-zA-Z0-9 ]", " ", combined_data$review)
head(combined_data$review)
strsplit(combined_data$review[c(1:6)], "[^a-zA-Z0-9]+")
row_emoticon <- str_detect(combined_data$review, ":\\)|:\\(|:-\\)")
combined_data$review[row_emoticon]
strsplit(combined_data$review[724], "[^a-zA-Z0-9]+")

# B1b. Vocabulary Size
words <- unique(str_split(combined_data, "\\s+")[[1]])
vocab_size <- length(words)
print(paste("Vocabulary size:", vocab_size))

# B1c. Word embedding length
combined_data$review %>% strsplit(" ") %>% sapply(length) %>% summary()

# B1d.  Statistical justification for the chosen maximum sequence length
# Calculate word counts
combined_data$word_count <- sapply(strsplit(combined_data$review, " "), length)
# Plot histogram
ggplot(combined_data, aes(x = word_count)) +
  geom_histogram(binwidth = 5,
                 fill = "#CDC8B1",
                 alpha = 0.7) +
  labs(title = "Distribution of Word Counts in Reviews", x = "Word Count", y = "Frequency") +
  theme_minimal()

# Cumulative distribution
cumulative_coverage <- ecdf(combined_data$word_count)
coverage_at_50 <- cumulative_coverage(50) * 100
coverage_at_100 <- cumulative_coverage(100) * 100
print(paste(
  "Percentage of reviews captured with max length 50:",
  coverage_at_50
))
print(paste(
  "Percentage of reviews captured with max length 100:",
  coverage_at_100
))


