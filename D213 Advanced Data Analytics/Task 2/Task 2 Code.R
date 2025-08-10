install.packages("tensorflow")
install.packages("stringi")
install.packages("reticulate")
install.packages("keras")
install.packages("readr")
install.packages("tokenizers")
install.packages("ggplot2")

library(tensorflow)
library(stringr)
library(reticulate)
library(keras)
library(readr)
library(tokenizers)
library(stringi)
library(ggplot2)

use_virtualenv("r-tensorflow", required = TRUE)

install_tensorflow()



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

# B2. Tolenization
max_words <- 10000
max_seq_length <- 50
tokenizer <- text_tokenizer(num_words = max_words)
fit_text_tokenizer(tokenizer, combined_data$review)
tokenized_reviews <- texts_to_sequences(tokenizer, combined_data$review)


# B3. Padding
padded_reviews <- pad_sequences(tokenized_reviews, maxlen = max_seq_length, padding = "post")
print(padded_reviews[1, ])

# B5. Split Data
set.seed(123)
train_idx <- sample(1:nrow(combined_data), 0.8 * nrow(combined_data))
train_data <- padded_reviews[train_idx, ]
train_labels <- combined_data$score[train_idx]

val_test_data <- padded_reviews[-train_idx, ]
val_test_labels <- combined_data$score[-train_idx]

val_idx <- sample(1:nrow(val_test_data), 0.5 * nrow(val_test_data))
val_data <- val_test_data[val_idx, ]
val_labels <- val_test_labels[val_idx]

test_data <- val_test_data[-val_idx, ]
test_labels <- val_test_labels[-val_idx]

# Output sizes for verification
print(paste("Training data size:", nrow(train_data)))
print(paste("Validation data size:", nrow(val_data)))
print(paste("Test data size:", nrow(test_data)))


# c1. output of the model summary
# Define parameters
vocab_size <- 10000         # Size of the vocabulary
embedding_dim <- 100        # Embedding dimensions
max_length <- 50            # Maximum sequence length

# Functional API model definition
input <- layer_input(shape = c(max_length), dtype = "int32") # Define input layer
output <- input %>%
  layer_embedding(input_dim = vocab_size, output_dim = embedding_dim) %>% # Embedding layer
  layer_lstm(units = 64, return_sequences = FALSE) %>%                    # LSTM layer
  layer_dense(units = 1, activation = 'sigmoid')                         # Output layer

model <- keras_model(inputs = input, outputs = output)
# Summarize the model
model$summary()


# C3e. eary stopping
callback_early_stopping <- callback_early_stopping(
  monitor = "val_accuracy", # Monitor validation accuracy
  patience = 2              # Stop after 2 epochs of no improvement
)

# D1
# Ensure data is numeric
train_data <- as.matrix(train_data)
train_labels <- as.numeric(train_labels)
val_data <- as.matrix(val_data)
val_labels <- as.numeric(val_labels)

# Compile the model
model %>% keras::compile(
  optimizer = optimizer_adam(),          # Use the adam optimizer
  loss = "binary_crossentropy",          # Binary crossentropy for binary classification
  metrics = c("accuracy")                # Evaluate accuracy during training
)
# Define Early Stopping Callback
callback_early_stopping <- callback_early_stopping(
  monitor = "val_loss",                  # Monitor validation loss
  patience = 3,                          # Stop training after 3 epochs of no improvement
  restore_best_weights = TRUE            # Restore the best model weights
)
# Fit the Model with Early Stopping
history <- model %>% fit(
  x = train_data,                        # Training data
  y = train_labels,                      # Training labels
  validation_data = list(val_data, val_labels), # Validation data
  epochs = 20,                           # Maximum number of epochs
  batch_size = 32,                       # Batch size
  callbacks = list(callback_early_stopping), # Include the early stopping callback
  verbose = 1                            # Verbose output
)

