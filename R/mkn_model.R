# English text data extracted using winrar in file system

# Load in all 3 text data sets
library(readr)
twitter <- read_lines("data/en_US/en_US.twitter.txt", progress = TRUE,
                      num_threads = 4)
blogs <- read_lines("data/en_US/en_US.blogs.txt", progress = TRUE,
                    num_threads = 4)
news <- read_lines("data/en_US/en_US.news.txt", progress = TRUE,
                   num_threads = 4)

# Combining all into one long character vector since we will be
# removing numbers and punctuation which will bring all 3 closer to each other
# as discussed in EDA Milestone Report
textData <- c(twitter, blogs, news)

# Remove everything beside the textData
rm(twitter, blogs, news)
gc()

# Defining a function to clean
clean_up <- function(x) {
    require(dplyr)
    require(qdapRegex)
    require(stringr)

    # Remove RT
    x <- gsub("RT :", "", x)

    # Remove any hashtags and anything following
    x <- str_remove_all(x, pattern = "#[:graph:]+")

    # Lower-case everything and remove characters which are not alphanumeric
    # or punctuation
    x <- kgrams::preprocess(x, erase = "[^.?!'[:alpha:][:space:]]")

    # Remove any ' with nothing following it
    x <- str_replace_all(x, pattern = "' ", replacement = " ")

    # Collapse everything to a single string
    x <- paste(x, collapse = " ")

    # Tokenize sentences
    x <- kgrams::tknz_sent(x, keep_first = TRUE)

    # Remove empty sentences
    x <- x[x != ""]

    # Remove repeating characters
    x <- rm_repeated_characters(x)

    # Remove non words (words misspelled, or out of language words)
    x <- rm_non_words(x)

    # Remove really short sentences
    x <- x[-which(nchar(x) <= 5)]

    # Add bos and eos
    # These are added for 2 purposes
    # 1. Allows to predict even when nothing is entered by the user
    # 2. Prerequisites to applying Kneser-ney Model
    x <- paste0("bos1 bos2 bos3 ", x, " eos")

    # Returning x
    return(x)
}

# Tidyup the data and split intp sentences
clean_data <- clean_up(textData)

# Remove everything beside the tidyData
rm(textData, clean_up)
gc()

# Further subset to only include sentences with more than 5 words
# and less than 100
clean_data <- tibble(text = clean_data) %>%
    mutate(n_words = tokenizers::count_words(text)) %>%
    filter(n_words <= 100, n_words > 5)
clean_data <- clean_data$text

# Save this as an Robject so it can retrieved at any time
save(clean_data, file = "data/cleaned_text_sentences.RData")

# Data contain 7.1 million sentences in total
# A test set of 2500 sentences is extracted
# A validation set of 2500 sentences is also extracted
# 2500 is chosen as per the recommendations in Chen & Goodman 1995
# 2 sets are extracted since parameters will be adjusted on the validation
# set and then evaluate the best performing model on the test set
# Defining a function which splits the data into train, test and validation and
# save them to the disc
split_text <- function(sentences) {

    require(dplyr)

    # Setting seed for reproducibility
    set.seed(15823)
    inTest <- sample(1:length(sentences), size = 2500)
    test <- tibble(text = sentences[inTest])
    notTest <- sentences[-inTest]

    # Setting seed for reproducibility
    set.seed(15823)
    inValidation <- sample(1:length(notTest), size = 2500)
    validation <- tibble(text = notTest[inValidation])
    train <- tibble(text = notTest[-inValidation])

    # Save the test and train set to the disc
    save(train, file = "data/train_sentences.RData")
    save(validation, file = "data/validation_sentences.RData")
    save(test, file = "data/test_sentences.RData")
}

# Splitting data
split_text(clean_data)

# Remove everything from the environment
rm(clean_data, split_text)
gc()

# We will load the train sentences and select a minimum sample size to start
# with for model building
# Plan - start with random 1M sentences and keep going up if needed
load("data/train_sentences.RData")

Nsample <- 200000
# Take a random sample of 200k sentences
set.seed(20823) # setting seed for reproducibility
train_sample <- train[sample(1:nrow(train), size = Nsample),]

# Remove train dataset
rm(train)
gc()

# Let's get the distribution of words for each sentence
library(dplyr)
library(ggplot2)
library(tokenizers)
train_sample %>%
    mutate(n_words = tokenizers::count_words(text)) %>%
    ggplot(aes(n_words)) +
    geom_bar()
#' **At 200k sample**
#' Very few have sentences longer then 75, max 175
#' Most are under 20 words

# Divide into tokens and give back a one hot encoded data table and
# corresponding word to id index
# value of n determines the n-gram level
tokenizer <- function(train, n = 2L) {

    require(dplyr)
    require(tokenizers)

    # Converting n to integer
    n <- as.integer(n)

    if(n < 2) {
        warning("n cannot be less than 2 for a Kneser-Ney Model")
        return(NULL)
    }

    # Get the words which only occur once
    words_at_1 <- tibble(words = unlist(tokenize_ngrams(train$text, n = 1,
                                                        simplify = TRUE))) %>%
        count(words) %>%
        filter(n < 2) %>%
        select(words)

    # Divide the train set into quadgrams
    train_token <- tibble(text = unlist(tokenize_ngrams(train$text, n = n,
                                                        simplify = TRUE)))

    require(textrecipes)
    # Recipe which will give one hot encoding of these sequences
    train_rec <- recipe(~text, data = train_token) %>%
        step_tokenize(text, options = list(strip_punct = FALSE,
                                           strip_numeric = FALSE)) %>%
        step_sequence_onehot(text, sequence_length = n)

    # Prep the recipe
    train_prep <- prep(train_rec)

    # Bake the data
    baked_train <- bake(train_prep, new_data = NULL)
    word_nums <- 1L:n
    column_names <- paste0("word", word_nums)
    names(baked_train) <- column_names

    # Get the word to integer conversion index
    word_index <- tidy(train_prep, 2)

    # Get the vocabulary indices which occur only once
    words_at_1_index <- semi_join(word_index[,2:3], words_at_1,
                                  by = join_by(token == words))

    # Get the replacement index of the unk token
    unk_word_vocab <- nrow(word_index) + 1L
    # Replace all words at 1 with unk token in baked train
    baked_train <- baked_train %>%
        mutate(across(all_of(column_names),
                      ~replace(.x, .x %in% words_at_1_index$vocabulary,
                               unk_word_vocab)))

    # Update the word_index to exclude words at 1 and include the unk token
    word_index <- anti_join(word_index[,2:3], words_at_1,
                            by = join_by(token == words))
    word_index <- rbind(word_index, tibble(vocabulary = unk_word_vocab,
                                           token = "<unk>"))

    return(list(baked_train, word_index))
}

# Tokenize our train sample
tokens <- tokenizer(train_sample, n = 2)

# Extracting the one hot encoded data table and word index
baked_train <- tokens[[1]]
vocab_index <- tokens[[2]]

# Remove the train sample, and list return
rm(train_sample, tokens)
gc()

# Moving onto the bigram model
bigram_mkn_model <- function(data, vocab) {

    require(dplyr)
    require(tidyr)

    # -----Term 1-----
    # Highest level probability based on raw counts
    counts <- data %>%
        count(word1, word2, name = "raw_count")

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(raw_count) %>%
        filter(raw_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(raw_count == 1) %>%
        mutate(mkn_count = raw_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(raw_count == 2) %>%
        mutate(mkn_count = raw_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(raw_count >= 3) %>%
        mutate(mkn_count = raw_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, count of word1
    counts <- counts %>%
        add_count(word1, name = "den")

    # Calculate the term1 for the model
    counts <- counts %>%
        mutate(term1 = mkn_count/den) %>%
        select(-mkn_count)

    # -----BO weight 1-----
    # BO weight will be used when the word1 doesn't occur in the test corpus
    # Numerator = D1/D2/D3+ * Number of unique words that follow word1
    counts <- counts %>%
        mutate(lambda_num = length(unique(word2)), .by = word1)

    # Adjust the numerator by multiplying the appropriate D(i)
    counts1 <- counts %>%
        filter(lambda_num == 1) %>%
        mutate(lambda_num = D1*lambda_num)
    counts2 <- counts %>%
        filter(lambda_num == 2) %>%
        mutate(lambda_num = D2*lambda_num)
    counts3plus <- counts %>%
        filter(lambda_num >= 3) %>%
        mutate(lambda_num = D3*lambda_num)
    counts <- rbind(counts1, counts2, counts3plus)

    # Normalize by dividing by den
    counts <- counts %>%
        mutate(lambda1 = lambda_num/den, .keep = "unused")
    # Calculate a mean value to use with unk probability after unigram
    mean_lambda1 <- mean(counts$lambda1)

    # -----Term 2-----
    # Term 2 will be the BO-weighted unigram probabilities
    counts <- counts %>%
        mutate(kn_count = length(unique(word1)), .by = word2)

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(kn_count) %>%
        filter(kn_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(kn_count == 1) %>%
        mutate(mkn_count = kn_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = kn_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = kn_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, total number of unique bigrams
    den <- data %>%
        count(word1, word2) %>%
        nrow()

    # Calculate the term1 for the model
    probs <- counts %>%
        mutate(term2 = lambda1 * mkn_count/den) %>%
        select(-c(mkn_count, lambda1))

    # Change the names to be more descriptive
    names(probs) <- c("word1", "output", "bigram_prob", "bo_unigram_prob")

    # Get vocabulary index of bos1, bos2, bos3 and eos
    index <- vocab$vocabulary[which(vocab$token %in% c("bos1", "bos2",
                                                       "bos3", "eos"))]
    # Remove bos1, bos2, bos3, and eos from the table
    probs <- probs[-which(probs$output %in% index),]

    # Return the probability table and updated vocabulary
    return(list(probs, vocab))
}

# Apply the bigram model to the baked train sample
result <- bigram_mkn_model(baked_train, vocab_index)

# Extract the tables from the result
bigram_model_probs <- result[[1]]
bigram_model_vocab <- result[[2]]
rm(result, baked_train, vocab_index)
gc()

# Accuracy and perplexity on the test set
evaluate_bigram_model <- function(prob_table, vocab, test, N) {

    require(stringr)
    require(tokenizers)
    require(dplyr)
    require(tidyr)

    # Remove bos1, bos2, bos3, and eos from the data set
    pattern <- "bos1 |bos2 | eos"
    words <- str_remove_all(test$text, pattern = pattern)

    # Divide each sentence into bigrams
    words <- unlist(tokenize_ngrams(words, n = 2L, simplify = TRUE))

    # Store as a tibble and separate into individual words
    words <- tibble(bigrams = words) %>%
        separate(bigrams, c("word1", "word2"), sep = " ")

    # Convert to one hot encodes from the train set
    words$word1 <- vocab$vocabulary[match(words$word1, vocab$token)]
    words$word2 <- vocab$vocabulary[match(words$word2, vocab$token)]

    # Replace any NAs with unk token index
    unk_vocab <- vocab$vocabulary[which(vocab$token == "<unk>")]
    words$word1[which(is.na(words$word1))] <- unk_vocab
    words$word2[which(is.na(words$word2))] <- unk_vocab

    # Remove duplicates
    words <- words %>%
        distinct(word1, word2)

    # Calculate the number of bigrams in the test set
    Wt <- nrow(words)

    # Print for sanity check
    print(paste0("Total number of unique bigrams in the test set: ", Wt))

    # Get the probabilities
    probs1 <- inner_join(prob_table, words,
                         by = join_by(word1 == word1, output == word2)) %>%
        select(word1, output, bigram_prob)
    # Subset the words with no match
    words_subset <- anti_join(words, probs1,
                              by = join_by(word1 == word1, word2 == output))

    # Tracker
    print("Bigram Probabilities matched")

    # Back off to weighted unigram probabilities
    unigram_prob_table <- prob_table %>%
        select(output, bo_unigram_prob) %>%
        distinct(output, .keep_all = TRUE)
    probs2 <- inner_join(unigram_prob_table, words_subset,
                         by = join_by(output == word2))

    # Tracker
    print("Unigram Probabilities matched")

    # Convert to log
    probs <- c(probs1$bigram_prob, probs2$bo_unigram_prob)
    probs <- log2(probs)

    # Calculate perplexity
    cross_entropy <- -sum(probs)/Wt
    perplexity <- 2^cross_entropy

    # Sanity check
    print(paste0("Perplexity = ", perplexity))

    # Calculate accuracy
    accuracy <- vector(mode = "integer")
    # Check at bigram level
    top3 <- prob_table %>%
        select(word1, output, bigram_prob) %>%
        slice_max(order_by = bigram_prob, n = 3,
                  by = word1, with_ties = FALSE)
    # Match
    match <- inner_join(words, top3, by = join_by(word1 == word1, word2 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched bigrams
    words_subset <- anti_join(words, match, by = join_by(word1 == word1,
                                                         word2 == word2))
    # Tracker
    print("Bigrams matched")

    # Get the top 3 choices for each word2
    top3 <- prob_table %>%
        select(output, bo_unigram_prob) %>%
        distinct(output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3, with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word2 == output))
    accuracy <- c(accuracy, nrow(match))
    # Tracker
    print("Unigrams matched")


    # Calculate accuracy
    accuracy <- sum(accuracy)/Wt

    # Sanity check
    print(paste0("Accuracy = ", round(accuracy*100, 2), " %"))

    # Return a evaluation table
    return(tibble(model = "mkn_bigram",
                  sample_size = N,
                  vocab_size = nrow(vocab) - 5,
                  cross_entropy = cross_entropy,
                  perplexity = perplexity, accuracy = accuracy))
}

# Load in the validation data set
load("data/validation_sentences.RData")

# Evaluate the unigram model
result <- evaluate_bigram_model(bigram_model_probs, bigram_model_vocab, validation, Nsample)

# Save this result
write.csv(result, file = "data/mkn_metrics.csv", quote = FALSE, row.names = FALSE)

# Save the model
save(bigram_model_probs, bigram_model_vocab, file = "data/mkn_bigram_model200k.RData")

# Remove
rm(bigram_model_probs, bigram_model_vocab, result)
gc()

#----------------------------------------------------------------------------

load("data/train_sentences.RData")

# Take a random sample of 200k sentences
set.seed(20823) # setting seed for reproducibility
train_sample <- train[sample(1:nrow(train), size = Nsample),]

# Remove train dataset
rm(train)
gc()

# Moving on to trigram model
# Tokenize our train sample
tokens <- tokenizer(train_sample, n = 3)

# Extracting the one hot encoded data table and word index
baked_train <- tokens[[1]]
vocab_index <- tokens[[2]]

# Remove the train sample, and list return
rm(train_sample, tokens)
gc()

# Defining the model
trigram_mkn_model <- function(data, vocab) {

    require(dplyr)
    require(tidyr)

    # -----Term 1-----
    # Highest level probability based on raw counts
    counts <- data %>%
        count(word1, word2, word3, name = "raw_count")

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(raw_count) %>%
        filter(raw_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(raw_count == 1) %>%
        mutate(mkn_count = raw_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(raw_count == 2) %>%
        mutate(mkn_count = raw_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(raw_count >= 3) %>%
        mutate(mkn_count = raw_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, count of word1
    counts <- counts %>%
        add_count(word1, word2, name = "den")

    # Calculate the term1 for the model
    counts <- counts %>%
        mutate(term1 = mkn_count/den) %>%
        select(-mkn_count)

    # -----BO weight 1-----
    # BO weight will be used when the (word1, word2, word3) doesn't occur in the test corpus
    # Numerator = D1/D2/D3+ * Number of unique words that follow (word1, word2, word3)
    counts <- counts %>%
        mutate(lambda_num = length(unique(word3)), .by = c(word1, word2))

    # Adjust the numerator by multiplying the appropriate D(i)
    counts1 <- counts %>%
        filter(lambda_num == 1) %>%
        mutate(lambda_num = D1*lambda_num)
    counts2 <- counts %>%
        filter(lambda_num == 2) %>%
        mutate(lambda_num = D2*lambda_num)
    counts3plus <- counts %>%
        filter(lambda_num >= 3) %>%
        mutate(lambda_num = D3*lambda_num)
    counts <- rbind(counts1, counts2, counts3plus)

    # Normalize by dividing by den
    counts <- counts %>%
        mutate(lambda1 = lambda_num/den, .keep = "unused")

    # -----Term 2-----
    # Term 2 will be the BO-weighted bigram probabilities
    counts <- counts %>%
        mutate(kn_count = length(unique(word1)), .by = c(word2, word3))

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(kn_count) %>%
        filter(kn_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(kn_count == 1) %>%
        mutate(mkn_count = kn_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = kn_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = kn_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, summation of numerator for each distinct word2
    counts <- counts %>%
        mutate(den = length(unique(word1)), .by = word2)

    # Calculate the term2 for the model
    counts <- counts %>%
        mutate(term2 = lambda1 * mkn_count/den) %>%
        select(-mkn_count)

    # -----BO weight 2-----
    # Numerator = D1/D2/D3 * Number of unique words that follow word2
    counts <- counts %>%
        mutate(lambda_num = length(unique(word3)), .by = word2)

    # Adjust the numerator by multiplying the appropriate D(i)
    counts1 <- counts %>%
        filter(lambda_num == 1) %>%
        mutate(lambda_num = D1*lambda_num)
    counts2 <- counts %>%
        filter(lambda_num == 2) %>%
        mutate(lambda_num = D2*lambda_num)
    counts3plus <- counts %>%
        filter(lambda_num >= 3) %>%
        mutate(lambda_num = D3*lambda_num)
    counts <- rbind(counts1, counts2, counts3plus)

    # Normalize by dividing by den
    counts <- counts %>%
        mutate(lambda2 = lambda_num/den) %>%
        select(-c(lambda_num, den))

    # -----Term 3-----
    # Term 3 will be the BO-weighted unigram probabilities
    counts <- counts %>%
        mutate(kn_count = length(unique(word2)), .by = word3)

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(kn_count) %>%
        filter(kn_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(kn_count == 1) %>%
        mutate(mkn_count = kn_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = kn_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = kn_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, total number of unique bigrams
    den <- data %>%
        count(word2, word3) %>%
        nrow()

    # Calculate the term3 for the model
    probs <- counts %>%
        mutate(term3 = lambda1 * lambda2 * mkn_count/den) %>%
        select(-c(mkn_count, lambda1, lambda2))

    # Change the names to be more descriptive
    names(probs) <- c("word1", "word2", "output",
                      "trigram_prob", "bo_bigram_prob", "bo_unigram_prob")

    # Get vocabulary index of bos1, bos2, bos3 and eos
    index <- vocab$vocabulary[which(vocab$token %in% c("bos1", "bos2",
                                                       "bos3", "eos"))]
    # Remove bos1, bos2, bos3, and eos from the output
    probs <- probs[-which(probs$output %in% index),]

    # Return the probability table and updated vocabulary
    return(list(probs, vocab))
}

# Apply the trigram model to the baked train sample
result <- trigram_mkn_model(baked_train, vocab_index)

# Extract the tables from the result
trigram_model_probs <- result[[1]]
trigram_model_vocab <- result[[2]]
rm(result, baked_train, vocab_index)
gc()

# Accuracy and perplexity on the test set
evaluate_trigram_model <- function(prob_table, vocab, test, N) {

    require(stringr)
    require(tokenizers)
    require(dplyr)
    require(tidyr)

    # Remove bos1, bos2, bos3, and eos from the data set
    pattern <- "bos1 | eos"
    words <- str_remove_all(test$text, pattern = pattern)

    # Divide each sentence into bigrams
    words <- unlist(tokenize_ngrams(words, n = 3L, simplify = TRUE))

    # Store as a tibble and separate into individual words
    words <- tibble(bigrams = words) %>%
        separate(bigrams, c("word1", "word2", "word3"), sep = " ")

    # Convert to one hot encodes from the train set
    words$word1 <- vocab$vocabulary[match(words$word1, vocab$token)]
    words$word2 <- vocab$vocabulary[match(words$word2, vocab$token)]
    words$word3 <- vocab$vocabulary[match(words$word3, vocab$token)]

    # Replace any NAs with unk token index
    unk_vocab <- vocab$vocabulary[which(vocab$token == "<unk>")]
    words$word1[which(is.na(words$word1))] <- unk_vocab
    words$word2[which(is.na(words$word2))] <- unk_vocab
    words$word3[which(is.na(words$word3))] <- unk_vocab

    # Remove duplicates
    words <- words %>%
        distinct(word1, word2, word3)

    # Calculate the number of bigrams in the test set
    Wt <- nrow(words)

    # Print for sanity check
    print(paste0("Total number of unique bigrams in the test set: ", Wt))

    # Get the probabilities
    probs1 <- inner_join(prob_table, words,
                         by = join_by(word1 == word1, word2 == word2,
                                      output == word3)) %>%
        select(word1, word2, output, trigram_prob)
    # Subset the words with no match
    words_subset <- anti_join(words, probs1,
                              by = join_by(word1 == word1, word2 == word2,
                                           word3 == output))
    # Tracker
    print("Trigram Probabilities matched")

    #  Back off to weighted bigram probabilities
    bigram_prob_table <- prob_table %>%
        select(word2, output, bo_bigram_prob) %>%
        distinct(word2, output, .keep_all = TRUE)
    probs2 <- inner_join(bigram_prob_table, words_subset,
                         by = join_by(word2 == word2,
                                      output == word3)) %>%
        select(word2, output, bo_bigram_prob)
    # Subset the words with no match
    words_subset <- anti_join(words_subset, probs2,
                              by = join_by(word2 == word2,
                                           word3 == output))
    # Tracker
    print("Bigram Probabilities matched")

    # Back off to weighted unigram probabilities
    unigram_prob_table <- prob_table %>%
        select(output, bo_unigram_prob) %>%
        distinct(output, .keep_all = TRUE)
    probs3 <- inner_join(unigram_prob_table, words_subset,
                         by = join_by(output == word3))
    # Tracker
    print("Unigram Probabilities matched")

    # Convert to log
    probs <- c(probs1$trigram_prob, probs2$bo_bigram_prob, probs3$bo_unigram_prob)
    probs <- log2(probs)

    # Calculate perplexity
    cross_entropy <- -sum(probs)/Wt
    perplexity <- 2^cross_entropy

    # Sanity check
    print(paste0("Perplexity = ", perplexity))

    # Calculate accuracy
    accuracy <- vector(mode = "integer")
    # Check at the trigram level
    top3 <- prob_table %>%
        select(word1, word2, output, trigram_prob) %>%
        slice_max(order_by = trigram_prob, n = 3,
                  by = c(word1, word2), with_ties = FALSE)
    # Match
    match <- inner_join(words, top3, by = join_by(word1 == word1, word2 == word2,
                                                  word3 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched bigrams
    words_subset <- anti_join(words, match, by = join_by(word1 == word1,
                                                         word2 == word2,
                                                         word3 == word3))

    # Back off to bigram level
    top3 <- prob_table %>%
        select(word2, output, bo_bigram_prob) %>%
        distinct(word2, output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_bigram_prob, n = 3,
                  by = word2, with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word2 == word2,
                                                         word3 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched bigrams
    words_subset <- anti_join(words_subset, match,
                              by = join_by(word2 == word2, word3 == word3))
    # Tracker
    print("Bigrams matched")

    # Back off to unigrams
    # Get the top 3 choices for unigrams
    top3 <- prob_table %>%
        select(output, bo_unigram_prob) %>%
        distinct(output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3, with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word3 == output))
    accuracy <- c(accuracy, nrow(match))
    # Tracker
    print("Unigrams matched")

    # Calculate accuracy
    accuracy <- sum(accuracy)/Wt

    # Sanity check
    print(paste0("Accuracy = ", round(accuracy*100, 2), " %"))

    # Return a evaluation table
    return(tibble(model = "mkn_trigram",
                  sample_size = N,
                  vocab_size = nrow(vocab) - 5,
                  cross_entropy = cross_entropy,
                  perplexity = perplexity,
                  accuracy = accuracy))
}

# Load in the validation data set
load("data/validation_sentences.RData")

# Evaluate the unigram model
result <- evaluate_trigram_model(trigram_model_probs, trigram_model_vocab, validation, Nsample)

# Save this result
write.table(result, file = "data/mkn_metrics.csv", row.names = FALSE,
            append = TRUE, col.names = FALSE, sep = ",", quote = FALSE)

# Save the model
save(trigram_model_probs, trigram_model_vocab, file = "data/mkn_trigram_model200k.RData")

# Remove
rm(trigram_model_probs, trigram_model_vocab, result)
gc()

#----------------------------------------------------------------------------

load("data/train_sentences.RData")

Nsample <- 200000
# Take a random sample of 200k sentences
# Other sample sizes tried 400k, 600k, 800k, 1M, and 1.5M
# 2M sentences on the train sample causes ran out of memory issues
set.seed(20823) # setting seed for reproducibility
train_sample <- train[sample(1:nrow(train), size = Nsample),]

# Remove train dataset
rm(train)
gc()

# Distribution of words
library(dplyr)
# library(ggplot2)
# train_sample %>%
#     mutate(n_words = tokenizers::count_words(text)) %>%
#     ggplot(aes(n_words)) +
#     geom_bar()
#' **At 400k sample**
#' Very few have sentences longer then 75
#' Max words per sentence = 12
#'
#' **At 600k sample**
#' Very few have sentences longer then 75, max 600
#' Most are under 12
#'
#' **At 800k sample**
#' Very few have sentences longer then 75
#' Most are under 12
#'
#' **At 1M sample**
#' Very few have sentences longer then 75
#' Most are under 12 words
#'
#' #' **At 1.5M sample**
#' Very few have sentences longer then 75
#' Most are under 12 words

# Moving on to quadgram model
# Tokenize our train sample
tokens <- tokenizer(train_sample, n = 4)
# Ran out of memory at 2M sentences

# Extracting the one hot encoded data table and word index
baked_train <- tokens[[1]]
vocab_index <- tokens[[2]]

# Remove the train sample, and list return
rm(train_sample, tokens)
gc()

# Defining the model
quadgram_mkn_model <- function(data, vocab) {

    require(dplyr)
    require(tidyr)

    data <- baked_train

    # -----Term 1-----
    # Highest level probability based on raw counts
    counts <- data %>%
        count(word1, word2, word3, word4, name = "raw_count")

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(raw_count) %>%
        filter(raw_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(raw_count == 1) %>%
        mutate(mkn_count = raw_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(raw_count == 2) %>%
        mutate(mkn_count = raw_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(raw_count >= 3) %>%
        mutate(mkn_count = raw_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, count of word1, word2, word3
    counts <- counts %>%
        add_count(word1, word2, word3, name = "den")

    # Calculate the term1 for the model
    counts <- counts %>%
        mutate(term1 = mkn_count/den) %>%
        select(-mkn_count)

    # -----BO weight 1-----
    # BO weight will be used when the (word1, word2, word3) doesn't occur in the test corpus
    # Numerator = D1/D2/D3+ * Number of unique words that follow (word1, word2, word3)
    counts <- counts %>%
        mutate(lambda_num = length(unique(word4)), .by = c(word1, word2, word3))

    # Adjust the numerator by multiplying the appropriate D(i)where i ∈ c(1,2,3)
    counts1 <- counts %>%
        filter(lambda_num == 1) %>%
        mutate(lambda_num = D1*lambda_num)
    counts2 <- counts %>%
        filter(lambda_num == 2) %>%
        mutate(lambda_num = D2*lambda_num)
    counts3plus <- counts %>%
        filter(lambda_num >= 3) %>%
        mutate(lambda_num = D3*lambda_num)
    counts <- rbind(counts1, counts2, counts3plus)

    # Normalize by dividing by den
    counts <- counts %>%
        mutate(lambda1 = lambda_num/den, .keep = "unused")

    # -----Term 2-----
    # Term 2 will be the BO-weighted trigram probabilities
    counts <- counts %>%
        mutate(kn_count = length(unique(word1)), .by = c(word2, word3, word4))

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(kn_count) %>%
        filter(kn_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(kn_count == 1) %>%
        mutate(mkn_count = kn_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = kn_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = kn_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, same as numerator for each distinct word2, word3
    # for all values of word4
    counts <- counts %>%
        mutate(den = length(unique(word1)), .by = c(word2, word3))

    # Calculate the term2 for the model which will be back-off weighted
    counts <- counts %>%
        mutate(term2 = lambda1 * mkn_count/den) %>%
        select(-mkn_count)

    # -----BO weight 2-----
    # Numerator = D1/D2/D3 * Number of unique words that follow word2, word3
    counts <- counts %>%
        mutate(lambda_num = length(unique(word4)), .by = c(word2, word3))

    # Adjust the numerator by multiplying the appropriate D(i) where i ∈ c(1,2,3)
    counts1 <- counts %>%
        filter(lambda_num == 1) %>%
        mutate(lambda_num = D1*lambda_num)
    counts2 <- counts %>%
        filter(lambda_num == 2) %>%
        mutate(lambda_num = D2*lambda_num)
    counts3plus <- counts %>%
        filter(lambda_num >= 3) %>%
        mutate(lambda_num = D3*lambda_num)
    counts <- rbind(counts1, counts2, counts3plus)

    # Normalize by dividing by den
    counts <- counts %>%
        mutate(lambda2 = lambda_num/den) %>%
        select(-c(lambda_num, den))

    # -----Term 3-----
    # Term 3 will be the BO-weighted bigram probabilities
    counts <- counts %>%
        mutate(kn_count = length(unique(word2)), .by = c(word3, word4))

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(kn_count) %>%
        filter(kn_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(kn_count == 1) %>%
        mutate(mkn_count = kn_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = kn_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = kn_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, same as numerator for each distinct word2, word3
    # for all values of word4
    counts <- counts %>%
        mutate(den = length(unique(word2)), .by = word3)

    # Calculate the term3 for the model
    counts <- counts %>%
        mutate(term3 = lambda1 * lambda2 * mkn_count/den) %>%
        select(-mkn_count)

    # -----BO weight 3-----
    # Numerator = D1/D2/D3 * Number of unique words that follow word3
    counts <- counts %>%
        mutate(lambda_num = length(unique(word4)), .by = word3)

    # Adjust the numerator by multiplying the appropriate D(i) where i ∈ c(1,2,3)
    counts1 <- counts %>%
        filter(lambda_num == 1) %>%
        mutate(lambda_num = D1*lambda_num)
    counts2 <- counts %>%
        filter(lambda_num == 2) %>%
        mutate(lambda_num = D2*lambda_num)
    counts3plus <- counts %>%
        filter(lambda_num >= 3) %>%
        mutate(lambda_num = D3*lambda_num)
    counts <- rbind(counts1, counts2, counts3plus)

    # Normalize by dividing by den
    counts <- counts %>%
        mutate(lambda3 = lambda_num/den) %>%
        select(-c(lambda_num, den))

    # -----Term 4-----
    # Term 4 will be the BO-weighted unigram probabilities
    counts <- counts %>%
        mutate(kn_count = length(unique(word3)), .by = word4)

    # Find out the values for D1, D2, and D3
    count_of_counts <- counts %>%
        count(kn_count) %>%
        filter(kn_count < 5)
    # Y = n1/(n1+2*n2)
    Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
    # D1 = 1 - 2Y(n2/n1)
    D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
    # D2 = 2 - 3Y(n3/n2)
    D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
    # D3 = 3 - 4Y(n4/n3)
    D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])

    # Calculate the numerator as discounted counts
    counts1 <- counts %>%
        filter(kn_count == 1) %>%
        mutate(mkn_count = kn_count - D1, .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = kn_count - D2, .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = kn_count - D3, .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, total number of unique bigrams
    den <- data %>%
        count(word3, word4) %>%
        nrow()

    # Calculate the term4 for the model
    # This will be backoff weighted thrice
    probs <- counts %>%
        mutate(term4 = lambda1 * lambda2 * lambda3 * mkn_count/den) %>%
        select(-c(mkn_count, lambda1, lambda2, lambda3))

    # Change the names to be more descriptive
    names(probs) <- c("word1", "word2", "word3", "output", "quadgram_prob",
                      "bo_trigram_prob", "bo_bigram_prob", "bo_unigram_prob")

    # Get vocabulary index of bos1, bos2, bos3 and eos
    index <- vocab$vocabulary[which(vocab$token %in% c("bos1", "bos2",
                                                       "bos3", "eos"))]
    # Remove bos1, bos2, bos3, and eos from the output
    probs <- probs[-which(probs$output %in% index),]

    # Return the probability table and updated vocabulary
    return(list(probs, vocab))
}

# Apply the quadgram model to the baked train sample
result <- quadgram_mkn_model(baked_train, vocab_index)

# Extract the tables from the result
quadgram_model_probs <- result[[1]]
quadgram_model_vocab <- result[[2]]
rm(result, baked_train, vocab_index)
gc()

# Accuracy and perplexity on the test set
evaluate_quadgram_model <- function(prob_table, vocab, test, N) {

    require(stringr)
    require(tokenizers)
    require(dplyr)
    require(tidyr)

    # Remove bos1, bos2, bos3, and eos from the data set
    pattern <- " eos"
    words <- str_remove_all(test$text, pattern = pattern)

    # Divide each sentence into quadgrams
    words <- unlist(tokenize_ngrams(words, n = 4L, simplify = TRUE))

    # Store as a tibble and separate into individual words
    words <- tibble(quadgrams = words) %>%
        separate(quadgrams, c("word1", "word2", "word3", "word4"), sep = " ")

    # Convert to one hot encodes from the train set
    words$word1 <- vocab$vocabulary[match(words$word1, vocab$token)]
    words$word2 <- vocab$vocabulary[match(words$word2, vocab$token)]
    words$word3 <- vocab$vocabulary[match(words$word3, vocab$token)]
    words$word4 <- vocab$vocabulary[match(words$word4, vocab$token)]

    # Replace any NAs with unk token index
    unk_vocab <- vocab$vocabulary[which(vocab$token == "<unk>")]
    words$word1[which(is.na(words$word1))] <- unk_vocab
    words$word2[which(is.na(words$word2))] <- unk_vocab
    words$word3[which(is.na(words$word3))] <- unk_vocab
    words$word4[which(is.na(words$word4))] <- unk_vocab

    # Remove duplicates
    words <- words %>%
        distinct(word1, word2, word3, word4)

    # Calculate the number of quadgrams in the test set
    Wt <- nrow(words)

    # Print for sanity check
    print(paste0("Total number of unique quadgrams in the test set: ", Wt))

    # Get the probabilities
    probs1 <- inner_join(prob_table, words,
                         by = join_by(word1 == word1, word2 == word2,
                                      word3 == word3, output == word4)) %>%
        select(word1, word2, word3, output, quadgram_prob)
    # Subset the words with no match
    words_subset <- anti_join(words, probs1,
                              by = join_by(word1 == word1, word2 == word2,
                                           word3 == word3, word4 == output))
    # Tracker
    print("Quadgram Probabilities matched")

    # Back off to trigrams
    trigram_prob_table <- prob_table %>%
        select(word2, word3, output, bo_trigram_prob) %>%
        distinct(word2, word3, output, .keep_all = TRUE)
    probs2 <- inner_join(trigram_prob_table, words_subset,
                         by = join_by(word2 == word2, word3 == word3,
                                      output == word4)) %>%
        select(word2, word3, output, bo_trigram_prob)
    # Subset the words with no match
    words_subset <- anti_join(words_subset, probs2,
                              by = join_by(word2 == word2, word3 == word3,
                                           word4 == output))
    # Tracker
    print("Trigram Probabilities matched")

    #  Back off to weighted bigram probabilities
    bigram_prob_table <- prob_table %>%
        select(word3, output, bo_bigram_prob) %>%
        distinct(word3, output, .keep_all = TRUE)
    probs3 <- inner_join(bigram_prob_table, words_subset,
                         by = join_by(word3 == word3,
                                      output == word4)) %>%
        select(word3, output, bo_bigram_prob)
    # Subset the words with no match
    words_subset <- anti_join(words_subset, probs3,
                              by = join_by(word3 == word3,
                                           word4 == output))
    # Tracker
    print("Bigram Probabilities matched")

    # Back off to weighted unigram probabilities
    unigram_prob_table <- prob_table %>%
        select(output, bo_unigram_prob) %>%
        distinct(output, .keep_all = TRUE)
    probs4 <- inner_join(unigram_prob_table, words_subset,
                         by = join_by(output == word4))
    # Tracker
    print("Unigram Probabilities matched")

    # Convert to log
    probs <- c(probs1$quadgram_prob, probs2$bo_trigram_prob,
               probs3$bo_bigram_prob, probs4$bo_unigram_prob)
    probs <- log2(probs)

    # Calculate perplexity
    cross_entropy <- -sum(probs)/Wt
    perplexity <- 2^cross_entropy

    # Sanity check
    print(paste0("Perplexity = ", perplexity))

    # Defining an empty vector for accuracy
    accuracy <- vector(mode = "integer")
    # Get matches at the quadgram level
    # Get the top 3 choices for each combination of word1, word2, and word3
    top3 <- prob_table %>%
        select(word1, word2, word3, output, quadgram_prob) %>%
        slice_max(order_by = quadgram_prob, n = 3,
                  by = c(word1, word2, word3), with_ties = FALSE)
    # Match
    match <- inner_join(words, top3, by = join_by(word1 == word1, word2 == word2,
                                                  word3 == word3, word4 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    words_subset <- anti_join(words, match, by = join_by(word1 == word1, word2 == word2,
                                                         word3 == word3, word4 == word4))
    # Tracker
    print("Quadgrams matched")

    # Back-off to the trigram level for the remaining
    # Get the top 3 choices for each combination of word2, and word3
    top3 <- prob_table %>%
        select(word2, word3, output, bo_trigram_prob) %>%
        distinct(word2, word3, output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_trigram_prob, n = 3,
                  by = c(word2, word3), with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word2 == word2,
                                                         word3 == word3,
                                                         word4 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    words_subset <- anti_join(words_subset, match, by = join_by(word2 == word2,
                                                                word3 == word3,
                                                                word4 == word4))
    # Tracker
    print("Trigrams matched")

    # Back-off to the bigram level for the remaining
    # Get the top 3 choices for each word3
    top3 <- prob_table %>%
        select(word3, output, bo_bigram_prob) %>%
        distinct(word3, output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_bigram_prob, n = 3,
                  by = word3, with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word3 == word3,
                                                         word4 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    words_subset <- anti_join(words_subset, match, by = join_by(word3 == word3,
                                                                word4 == word4))
    # Tracker
    print("Bigrams matched")

    # Back-off to the unigram level for the remaining
    # Get the top 3 choices for each word1, word2
    top3 <- prob_table %>%
        select(word1, word2, output, bo_unigram_prob) %>%
        distinct(word1, word2, output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3,
                  by = c(word1, word2), with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word1 == word1,
                                                         word2 == word2,
                                                         word4 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    words_subset <- anti_join(words_subset, match, by = join_by(word1 == word1,
                                                                word2 == word2,
                                                                word4 == word4))
    # Tracker
    print("Unigrams with word1, word2 context matched")

    # Get the top 3 choices for each word2
    top3 <- prob_table %>%
        select(word2, output, bo_unigram_prob) %>%
        distinct(word2, output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3,
                  by = word2, with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word2 == word2,
                                                         word4 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    words_subset <- anti_join(words_subset, match, by = join_by(word2 == word2,
                                                                word4 == word4))
    # Tracker
    print("Unigrams with word2 context matched")

    # Get the top 3 choices for each word1
    top3 <- prob_table %>%
        select(word1, output, bo_unigram_prob) %>%
        distinct(word1, output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3,
                  by = word1, with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word1 == word1,
                                                         word4 == output))
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    words_subset <- anti_join(words_subset, match, by = join_by(word1 == word1,
                                                                word4 == word4))
    # Tracker
    print("Unigrams with word1 context matched")

    # Get the top 3 choices without any context
    top3 <- prob_table %>%
        select(output, bo_unigram_prob) %>%
        distinct(output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3, with_ties = FALSE)
    # Match
    match <- inner_join(words_subset, top3, by = join_by(word4 == output))
    accuracy <- c(accuracy, nrow(match))
    # Tracker
    print("Unigrams matched")

    # Calculate accuracy
    accuracy <- sum(accuracy)/Wt

    # Sanity check
    print(paste0("Accuracy = ", round(accuracy*100, 2), " %"))

    # Return a evaluation table
    return(tibble(model = "mkn_quadgram",
                  sample_size = N,
                  vocab_size = nrow(vocab) - 5,
                  cross_entropy = cross_entropy,
                  perplexity = perplexity,
                  accuracy = accuracy))
}

# Load in the validation data set
load("data/validation_sentences.RData")

# Evaluate the quadgram model
result <- evaluate_quadgram_model(quadgram_model_probs, quadgram_model_vocab,
                                  validation, Nsample)

# Save this result
write.table(result, file = "data/mkn_metrics.csv", row.names = FALSE,
            append = TRUE, col.names = FALSE, sep = ",", quote = FALSE)

# Save the model
save(quadgram_model_probs, quadgram_model_vocab, file = "data/mkn_quadgram_model200k.RData")

# Remove
rm(quadgram_model_probs, quadgram_model_vocab, result)
gc()

# Quadgram model finalized
# Pentagram model has too much sparsity to be viable since all pentagrams
# occur only once in the train sample
#' **Next step**
# ------------------Tuning the number of train samples-------------------------

# Since all will have different amount of vocab size, best comparison
# can only be with accuracy, and not perplexity/cross_entropy
# These are recorded just in case we decide to limit vocabulary size
library(dplyr)
load("data/train_sentences.RData")

# Load in the validation data set
load("data/validation_sentences.RData")

Nsample <- c(500000, 750000, 1000000, 1250000, 1500000)
# Other sample sizes tried 500k, 750k, 1M, 1.25M, 1.5M
# 2M sentences on the train sample causes ran out of memory issues
set.seed(20823) # setting seed for reproducibility
train_sample <- train[sample(1:nrow(train), size = Nsample[i]),]

# Remove train dataset
rm(train)
gc()

# Distribution of words
# library(dplyr)
# library(ggplot2)
# train_sample %>%
#     mutate(n_words = tokenizers::count_words(text)) %>%
#     ggplot(aes(n_words)) +
#     geom_bar()
#' **At 500k sample**
#' Very few have sentences longer then 75
#' Most sentences under 12
#'
#' **At 750k sample**
#' Very few have sentences longer then 75
#' Most sentences under 12
#'
#' **At 1M sample**
#' Very few have sentences longer then 75
#' Most sentences under 12
#'
#' **At 1.25M sample**
#' Very few have sentences longer then 75
#' Most sentences under 15
#'
#' **At 1.5M sample**
#' Very few have sentences longer then 75
#' Most sentences under 15

# Moving on to quadgram model
# Tokenize our train sample
tokens <- tokenizer(train_sample, n = 4)
# Ran out of memory at 2M sentences

# Extracting the one hot encoded data table and word index
baked_train <- tokens[[1]]
vocab_index <- tokens[[2]]

# Remove the train sample, and list return
rm(train_sample, tokens)
gc()

# Apply the quadgram model to the baked train sample
result <- quadgram_mkn_model(baked_train, vocab_index)

# Extract the tables from the result
quadgram_model_probs <- result[[1]]
quadgram_model_vocab <- result[[2]]
rm(result, baked_train, vocab_index)
gc()

# # Load in the validation data set
# load("data/validation_sentences.RData")

# Evaluate the quadgram model
result <- evaluate_quadgram_model(quadgram_model_probs, quadgram_model_vocab,
                                  validation, Nsample[i])

# Check the metrics
result

# Save the metrics
write.table(result, file = "data/mkn_metrics.csv", row.names = FALSE,
            append = TRUE, col.names = FALSE, sep = ",", quote = FALSE)

# Save the model
save(quadgram_model_probs, quadgram_model_vocab,
     file = paste0("data/mkn_quadgram_model", Nsample[i]/10^6, "M.RData"))

# Remove
rm(quadgram_model_probs, quadgram_model_vocab, result)
gc()

#' The increase in accuracy is affected by limited gains above 750k/1M
#' Thus, A smaller model thus would perform as good as the bigger one
#' **Next step**
# -------------Determine how large of data we can use in the app-------------

library(dplyr)
# Prepare the model data to use in the app
# Get the top3 values for each n-gram level at 1.5 M, which we can reduce down
# step by step
load("data/mkn_quadgram_model1.5M.RData")
# Remove bad words from the output column
# Download the Kaggle bad bad words data set
#' [Link](https://www.kaggle.com/datasets/nicapotato/bad-bad-words/download?datasetVersionNumber=1)
bad_words <- read.csv("data/bad-words.csv")
# Find out the vocabulary values if any occur in the vocabulary
bad_words_vocab <- quadgram_model_vocab$vocabulary[match(bad_words$jigaboo,
                                                         quadgram_model_vocab$token)]
# Remove any non matches, ie NAs
bad_words_vocab <- bad_words_vocab[-which(is.na(bad_words_vocab))]
# Convert to a tibble to ensure easier anti-join
bad_words_vocab <- tibble(output = bad_words_vocab)

# Remove any rows which contain these as outputs
final_model <- anti_join(quadgram_model_probs, bad_words_vocab)
vocab <- quadgram_model_vocab

# Remove and free up memory
rm(quadgram_model_probs, quadgram_model_vocab, bad_words, bad_words_vocab)
gc()

# Remove any rows with unk_token from the output column
unk_vocab <- vocab$vocabulary[match("<unk>", vocab$token)]
unk_vocab <- tibble(output = unk_vocab)
final_model <- anti_join(final_model, unk_vocab)

# Prepare top3 probability tables for each n-gram level
top3_quadgram <- final_model %>%
    select(word1, word2, word3, output, quadgram_prob) %>%
    slice_max(order_by = quadgram_prob, n = 3,
              by = c(word1, word2, word3), with_ties = FALSE) %>%
    select(word1, word2, word3, output)

# Back off to trigrams
top3_trigram <- final_model %>%
    select(word2, word3, output, bo_trigram_prob) %>%
    distinct(word2, word3, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_trigram_prob, n = 3,
              by = c(word2, word3), with_ties = FALSE) %>%
    select(word2, word3, output)

# Back off to bigrams
top3_bigram <- final_model %>%
    select(word3, output, bo_bigram_prob) %>%
    distinct(word3, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_bigram_prob, n = 3,
              by = word3, with_ties = FALSE) %>%
    select(word3, output)

# Back off to unigrams
# with word1 and word2 context
top3_unigram12 <- final_model %>%
    select(word1, word2, output, bo_unigram_prob) %>%
    distinct(word1, word2, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_unigram_prob, n = 3,
              by = c(word1, word2), with_ties = FALSE) %>%
    select(word1, word2, output)
# with word2 context
top3_unigram2 <- final_model %>%
    select(word2, output, bo_unigram_prob) %>%
    distinct(word2, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_unigram_prob, n = 3,
              by = word2, with_ties = FALSE) %>%
    select(word2, output)
# with word1 context
top3_unigram1 <- final_model %>%
    select(word1, output, bo_unigram_prob) %>%
    distinct(word1, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_unigram_prob, n = 3,
              by = word1, with_ties = FALSE) %>%
    select(word1, output)
# with no context
top3_unigram <- final_model %>%
    select(output, bo_unigram_prob) %>%
    distinct(output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_unigram_prob, n = 3, with_ties = FALSE) %>%
    select(output)

# Check the total size of these
print(object.size(top3_quadgram) +
          object.size(top3_trigram) +
          object.size(top3_bigram) +
          object.size(top3_unigram12) +
          object.size(top3_unigram2) +
          object.size(top3_unigram1) +
          object.size(top3_unigram) +
          object.size(vocab), units = "Mb")
# 331.5 Mb at 1.5M train sentences

# Save these and the vocab to the app directory
save(top3_quadgram, top3_trigram, top3_bigram, top3_unigram12,
     top3_unigram2, top3_unigram1, top3_unigram, vocab,
     file = "text_ease/data/model.RData")

# Define the app ui and server logic

# Run profvis to look for bottlenecks
library(profvis)
library(shiny)
profvis({
    runApp("text_ease")
}, prof_output = "data")

profvis(prof_input = "data/file2410317b2072.Rprof")

# Ran out of memory on the shinyapps webiste
# 2023-08-25T08:41:01.185818+00:00 shinyapps:
# Container event from container-8544890: oom (out of memory)

# --------------------------------------------------------------------------

# Check out 1M train sentences model
library(dplyr)
load("data/mkn_quadgram_model1M.RData")

# Remove bad words
bad_words <- read.csv("data/bad-words.csv")
# Find out the vocabulary values if any occur in the vocabulary
bad_words_vocab <- quadgram_model_vocab$vocabulary[match(bad_words$jigaboo,
                                                         quadgram_model_vocab$token)]
# Remove any non matches, ie NAs
bad_words_vocab <- bad_words_vocab[-which(is.na(bad_words_vocab))]
# Convert to a tibble to ensure easier anti-join
bad_words_vocab <- tibble(output = bad_words_vocab)

# Remove any rows which contain these as outputs
final_model <- anti_join(quadgram_model_probs, bad_words_vocab)
vocab <- quadgram_model_vocab

# Remove and free up memory
rm(quadgram_model_probs, quadgram_model_vocab, bad_words, bad_words_vocab)
gc()

# Remove any rows with unk_token from the output column
unk_vocab <- vocab$vocabulary[match("<unk>", vocab$token)]
unk_vocab <- tibble(output = unk_vocab)
final_model <- anti_join(final_model, unk_vocab)

# Prepare top3 probability tables for each n-gram level
top3_quadgram <- final_model %>%
    select(word1, word2, word3, output, quadgram_prob) %>%
    slice_max(order_by = quadgram_prob, n = 3,
              by = c(word1, word2, word3), with_ties = FALSE) %>%
    select(word1, word2, word3, output)

# Back off to trigrams
top3_trigram <- final_model %>%
    select(word2, word3, output, bo_trigram_prob) %>%
    distinct(word2, word3, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_trigram_prob, n = 3,
              by = c(word2, word3), with_ties = FALSE) %>%
    select(word2, word3, output)

# Back off to bigrams
top3_bigram <- final_model %>%
    select(word3, output, bo_bigram_prob) %>%
    distinct(word3, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_bigram_prob, n = 3,
              by = word3, with_ties = FALSE) %>%
    select(word3, output)

# Back off to unigrams
# with word1 and word2 context
top3_unigram12 <- final_model %>%
    select(word1, word2, output, bo_unigram_prob) %>%
    distinct(word1, word2, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_unigram_prob, n = 3,
              by = c(word1, word2), with_ties = FALSE) %>%
    select(word1, word2, output)
# with word2 context
top3_unigram2 <- final_model %>%
    select(word2, output, bo_unigram_prob) %>%
    distinct(word2, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_unigram_prob, n = 3,
              by = word2, with_ties = FALSE) %>%
    select(word2, output)
# with word1 context
top3_unigram1 <- final_model %>%
    select(word1, output, bo_unigram_prob) %>%
    distinct(word1, output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_unigram_prob, n = 3,
              by = word1, with_ties = FALSE) %>%
    select(word1, output)
# with no context
top3_unigram <- final_model %>%
    select(output, bo_unigram_prob) %>%
    distinct(output, .keep_all = TRUE) %>%
    slice_max(order_by = bo_unigram_prob, n = 3, with_ties = FALSE) %>%
    select(output)

# Check the total size of these
print(object.size(top3_quadgram) +
          object.size(top3_trigram) +
          object.size(top3_bigram) +
          object.size(top3_unigram12) +
          object.size(top3_unigram2) +
          object.size(top3_unigram1) +
          object.size(top3_unigram) +
          object.size(vocab), units = "Mb")
# 331.5 Mb at 1.5M train sentences
# 230.3 Mb at 1M train sentences

# Save these and the vocab to the app directory
save(top3_quadgram, top3_trigram, top3_bigram, top3_unigram12,
     top3_unigram2, top3_unigram1, top3_unigram, vocab,
     file = "text_ease/data/model.RData")

# Update the app on the website
# No memory issues

#' **Quadgram model with 1M sentences in the training set is finalized**

# Save the model
save(final_model, vocab, file = "data/final_model.RData")

# --------------------- Final Test on the test set --------------------------

load("data/test_sentences.RData")
load("data/final_model.RData")

# Evaluate the quadgram model
result <- evaluate_quadgram_model(final_model, vocab, test, N = 1000000)

# Check the metrics
result

# Save the result
write.csv(result, file = "data/final_model_metrics.csv", quote = FALSE,
          row.names = FALSE)
