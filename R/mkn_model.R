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
        mutate(mkn_count = max(raw_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(raw_count == 2) %>%
        mutate(mkn_count = max(raw_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(raw_count >= 3) %>%
        mutate(mkn_count = max(raw_count - D3, 0), .keep = "unused")
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
        mutate(mkn_count = max(kn_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = max(kn_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = max(kn_count - D3, 0), .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, total number of unique bigrams
    den <- data %>%
        count(word1, word2) %>%
        nrow()

    # Calculate the term1 for the model
    probs <- counts %>%
        mutate(term2 = lambda1 * mkn_count/den) %>%
        select(-c(mkn_count, lambda1))

    # -----BO weight 2-----
    # BO weight will be used when the word1 and word2 both don't occur in the test corpus
    # This will be used for <unk> token
    # Add the token for <unk> to the vocabulary
    unk_token_vocab <- vocab$vocabulary[nrow(vocab)] + 1L
    vocab <- rbind(vocab[,2:3], tibble(vocabulary = unk_token_vocab,
                                       token = "<unk>"))

    # mean(lambda1) * lambda(empty string) * 1/V
    # where V is vocabulary
    # which can be simplified to mean(lambda1) * mean(D)/den
    probs <- rbind(probs, tibble(word1 = unk_token_vocab,
                                 word2 = unk_token_vocab,
                                 term1 = mean_lambda1 * mean(c(D1, D2, D3))/den,
                                 term2 = mean_lambda1 * mean(c(D1, D2, D3))/den))

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

    # Print for sanity check
    print(paste0("Total number of unique bigrams in the test set: ", nrow(words)))

    # Convert it to a matrix
    words <- as.matrix(words)

    # Get the probabilities from the table
    probs <- sapply(1:nrow(words), function(i) {

        # Check for bigram
        p <- prob_table %>%
            filter(word1 == words[i, 1], output == words[i, 2]) %>%
            select(bigram_prob)

        # Get the unk token probability if nothing matches
        if(nrow(p) == 0) {
            p <- prob_table$bo_unigram_prob[nrow(prob_table)]
        }

        # Tracker
        if(i %% 1000 == 0) {
            print(paste0("Probability: ", i, " bigrams done"))
        }

        return(unlist(p))
    })

    # Convert to log
    probs <- log2(probs)

    # Calculate the number of bigrams in the test set
    Wt <- nrow(words)

    # Calculate perplexity
    cross_entropy <- -sum(probs)/Wt
    perplexity <- 2^cross_entropy

    # Check the accuracy
    accuracy <- sapply(1:nrow(words), function(i) {

        library(dplyr)

        # Check for bigram
        predictions <- prob_table %>%
            filter(word1 == words[i, 1]) %>%
            arrange(desc(bigram_prob)) %>%
            select(output) %>%
            unlist()

        # Check for unigram if necessary
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                arrange(desc(bo_unigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }

        # Get the top 3
        predictions <- predictions[1:3]

        # Tracker
        if(i %% 1000 == 0) {
            print(paste0("Predictions: ", i, " bigrams done"))
        }

        # Check if it matches w[2]
        return(words[i, 2] %in% predictions)
    })

    # Calculate accuracy
    accuracy <- sum(accuracy)/Wt

    # Return a evaluation table
    return(tibble(model = "mkn_bigram", sample_size = N,
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
save(bigram_model_probs, bigram_model_vocab, file = "data/mkn_bigram_model1M.RData")

# Remove
rm(bigram_model_probs, bigram_model_vocab, result)
gc()

#----------------------------------------------------------------------------

load("data/train_sentences.RData")

Nsample <- 3500000
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
        mutate(mkn_count = max(raw_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(raw_count == 2) %>%
        mutate(mkn_count = max(raw_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(raw_count >= 3) %>%
        mutate(mkn_count = max(raw_count - D3, 0), .keep = "unused")
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
    # Calculate a mean value to use with unk probability after unigram
    mean_lambda1 <- mean(counts$lambda1)

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
        mutate(mkn_count = max(kn_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = max(kn_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = max(kn_count - D3, 0), .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, summation of numerator for each distinct word2
    counts <- counts %>%
        mutate(den = sum(mkn_count), .by = word2)

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
        mutate(lambda2 = lambda1 * lambda_num/den) %>%
        select(-c(lambda_num, den))
    # Calculate a mean value to use with unk probability after unigram
    mean_lambda2 <- mean(counts$lambda2)

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
        mutate(mkn_count = max(kn_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = max(kn_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = max(kn_count - D3, 0), .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, total number of unique bigrams
    den <- data %>%
        count(word2, word3) %>%
        nrow()

    # Calculate the term3 for the model
    probs <- counts %>%
        mutate(term3 = lambda1 * lambda2 * mkn_count/den) %>%
        select(-c(mkn_count, lambda1, lambda2))

    # -----BO weight 3-----
    # This will be used for <unk> token
    # Add the token for <unk> to the vocabulary
    unk_token_vocab <- vocab$vocabulary[nrow(vocab)] + 1L
    vocab <- rbind(vocab[,2:3], tibble(vocabulary = unk_token_vocab,
                                       token = "<unk>"))

    # mean(lambda1) * lambda2 * lambda(empty string) * 1/V
    # where V is vocabulary
    # which can be simplified to mean(lambda1) * lambda2 * mean(D)/den
    probs <- rbind(probs, tibble(word1 = unk_token_vocab,
                                 word2 = unk_token_vocab,
                                 word3 = unk_token_vocab,
                                 term1 = 0,
                                 term2 = 0,
                                 term3 = mean_lambda1 * mean_lambda2 *
                                     mean(c(D1, D2, D3))/den))

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

    # Convert it to a matrix
    words <- as.matrix(words)

    # For probabilities we will use interpolated model
    interpolated_prob_table <- prob_table %>%
        mutate(prob = trigram_prob + bo_bigram_prob + bo_unigram_prob,
               .keep = "unused")

    # Get the probabilities from the table
    probs <- sapply(1:nrow(words), function(i) {

        # Check for trigram
        p <- interpolated_prob_table %>%
            filter(word1 == words[i, 1], word2 == words[2], output == words[3]) %>%
            select(prob)

        # Get the unk token probability if nothing matches
        if(nrow(p) == 0) {
            p <- interpolated_prob_table$prob[nrow(interpolated_prob_table)]
        }

        # Tracker
        if(i %% 1000 == 0) {
            print(paste0("Probability: ", i, " trigrams done"))
        }

        return(unlist(p))
    })

    # Convert to log
    probs <- log2(probs)

    # Calculate the number of trigrams in the test set
    Wt <- nrow(words)

    # Calculate perplexity
    cross_entropy <- -sum(probs)/Wt
    perplexity <- 2^cross_entropy

    # Check the accuracy
    accuracy <- sapply(1:nrow(words), function(i) {

        # Check for trigram
        predictions <- prob_table %>%
            filter(word1 == words[i, 1], word2 == words[i, 2]) %>%
            arrange(desc(trigram_prob)) %>%
            select(output) %>%
            unlist()

        # Back off to bigrams
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                filter(word2 == words[i, 2]) %>%
                arrange(desc(bo_bigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }

        # Back off to unigrams
        # Limit to word1
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                filter(word1 == words[i, 1]) %>%
                arrange(desc(bo_unigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }
        # If nothing matches
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                arrange(desc(bo_unigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }

        # Get the top 3 predictions
        predictions <- predictions[1:3]

        # Tracker
        if(i %% 1000 == 0) {
            print(paste0("Predictions: ", i, " trigrams done"))
        }

        # Check if it matches w[2]
        return(words[i, 3] %in% predictions)
    })

    # Calculate accuracy
    accuracy <- sum(accuracy)/Wt

    # Return a evaluation table
    return(tibble(model = "mkn_trigram", sample_size = N,
                  cross_entropy = cross_entropy,
                  perplexity = perplexity, accuracy = accuracy))
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

Nsample <- 1500000
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
library(ggplot2)
train_sample %>%
    mutate(n_words = tokenizers::count_words(text)) %>%
    ggplot(aes(n_words)) +
    geom_bar()
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
        mutate(mkn_count = max(raw_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(raw_count == 2) %>%
        mutate(mkn_count = max(raw_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(raw_count >= 3) %>%
        mutate(mkn_count = max(raw_count - D3, 0), .keep = "unused")
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
    # Calculate a mean value to use with unk probability after unigram
    mean_lambda1 <- mean(counts$lambda1)

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
        mutate(mkn_count = max(kn_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = max(kn_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = max(kn_count - D3, 0), .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, summation of numerator for each distinct word2, word3
    counts <- counts %>%
        mutate(den = sum(mkn_count), .by = c(word2, word3))

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
        mutate(lambda2 = lambda1 * lambda_num/den) %>%
        select(-c(lambda_num, den))
    # Calculate a mean value to use with unk probability after unigram
    mean_lambda2 <- mean(counts$lambda2)

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
        mutate(mkn_count = max(kn_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = max(kn_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = max(kn_count - D3, 0), .keep = "unused")
    counts <- rbind(counts1, counts2, counts3plus)

    # Calculate the denominator, summation of numerator for each distinct word3
    counts <- counts %>%
        mutate(den = sum(mkn_count), .by = word3)

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
        mutate(lambda3 = lambda1 * lambda2 * lambda_num/den) %>%
        select(-c(lambda_num, den))
    # Calculate a mean value to use with unk probability after unigram
    mean_lambda3 <- mean(counts$lambda3)

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
        mutate(mkn_count = max(kn_count - D1, 0), .keep = "unused")
    counts2 <- counts %>%
        filter(kn_count == 2) %>%
        mutate(mkn_count = max(kn_count - D2, 0), .keep = "unused")
    counts3plus <- counts %>%
        filter(kn_count >= 3) %>%
        mutate(mkn_count = max(kn_count - D3, 0), .keep = "unused")
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

    # -----BO weight 4-----
    # This will be used for <unk> token
    # Add the token for <unk> to the vocabulary
    unk_token_vocab <- vocab$vocabulary[nrow(vocab)] + 1L
    vocab <- rbind(vocab[,2:3], tibble(vocabulary = unk_token_vocab,
                                       token = "<unk>"))

    # mean(lambda1) * mean(lambda2) * mean(lambda3) * lambda(empty string) * 1/V
    # where V is vocabulary
    # which can be simplified to mean(lambda1) * lambda2 * lambda3 * mean(D)/den
    probs <- rbind(probs, tibble(word1 = unk_token_vocab,
                                 word2 = unk_token_vocab,
                                 word3 = unk_token_vocab,
                                 word4 = unk_token_vocab,
                                 term1 = 0,
                                 term2 = 0,
                                 term3 = 0,
                                 term4 = mean_lambda1 * mean_lambda2 *
                                     mean_lambda3 *
                                     mean(c(D1, D2, D3))/den))

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

    # Print for sanity check
    print(paste0("Total number of unique quadgrams in the test set: ", nrow(words)))

    # Convert it to a matrix
    words <- as.matrix(words)

    # For probabilities we will use interpolated model
    interpolated_prob_table <- prob_table %>%
        mutate(prob = quadgram_prob + bo_trigram_prob + bo_bigram_prob + bo_unigram_prob,
               .keep = "unused")

    # Get the probabilities from the table
    probs <- sapply(1:nrow(words), function(i) {

        # Check for quadgram
        p <- interpolated_prob_table %>%
            filter(word1 == words[i, 1], word2 == words[i, 2],
                   word3 == words[i, 3], output == words[i, 4]) %>%
            select(prob)

        # Get the unk token probability if no matches
        if(nrow(p) == 0) {
            p <- interpolated_prob_table$prob[nrow(interpolated_prob_table)]
        }

        # Tracker
        if(i %% 1000 == 0) {
            print(paste0("Probability: ", i, " quadgrams done"))
        }

        return(unlist(p))
    })

    # Convert to log
    probs <- log2(probs)

    # Calculate the number of quadgrams in the test set
    Wt <- nrow(words)

    # Calculate perplexity
    cross_entropy <- -sum(probs)/Wt
    perplexity <- 2^cross_entropy

    # Check the accuracy
    accuracy <- sapply(1:nrow(words), function(i) {

        # Check for trigram
        predictions <- prob_table %>%
            filter(word1 == words[i, 1], word2 == words[i, 2], word3 == words[i, 3]) %>%
            arrange(desc(quadgram_prob)) %>%
            select(output) %>%
            unlist()

        # Back off to trigrams
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                filter(word2 == words[i, 2], word3 == words[i, 3]) %>%
                arrange(desc(bo_trigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }

        # Back off to bigrams
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                filter(word3 == words[i, 3]) %>%
                arrange(desc(bo_bigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }

        # Back off to unigrams
        # Limit to word1 and word2
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                filter(word1 == words[i, 1], word2 == words[i, 2]) %>%
                arrange(desc(bo_unigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }
        # Limit to word2
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                filter(word2 == words[i, 2]) %>%
                arrange(desc(bo_unigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }
        # Limit to word1
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                filter(word1 == words[i, 1]) %>%
                arrange(desc(bo_unigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }
        # If nothing matches
        if(length(predictions) < 3) {
            predictions <- prob_table %>%
                arrange(desc(bo_unigram_prob)) %>%
                select(output) %>%
                unlist()
            predictions <- unique(predictions)
        }

        # Get the top 3 predictions
        predictions <- predictions[1:3]

        # Tracker
        if(i %% 1000 == 0) {
            print(paste0("Predictions: ", i, " quadgrams done"))
        }

        # Check if it matches w[i, 4]
        return(words[i, 4] %in% predictions)
    })

    # Calculate accuracy
    accuracy <- sum(accuracy)/Wt

    # Return a evaluation table
    return(tibble(model = "mkn_quadgram", sample_size = N,
                  cross_entropy = cross_entropy,
                  perplexity = perplexity, accuracy = accuracy))
}

# Accuracy on different sizes of train samples
evaluate_quadgram_model <- function(prob_table, vocab, test, N) {

    require(stringr)
    require(tokenizers)
    require(dplyr)
    require(tidyr)

    # Remove eos from the data set
    pattern <- " eos"
    words <- str_remove_all(test$text, pattern = pattern)

    # Divide each sentence into quadgrams
    words <- unlist(tokenize_ngrams(words, n = 4L, simplify = TRUE))

    # Store as a tibble and separate into individual words
    words <- tibble(quadgrams = words) %>%
        separate(quadgrams, c("word1", "word2", "word3", "output"), sep = " ")

    # Convert to one hot encodes from the train set
    words$word1 <- vocab$vocabulary[match(words$word1, vocab$token)]
    words$word2 <- vocab$vocabulary[match(words$word2, vocab$token)]
    words$word3 <- vocab$vocabulary[match(words$word3, vocab$token)]
    words$output <- vocab$vocabulary[match(words$output, vocab$token)]

    # Replace any NAs with unk token index
    unk_vocab <- vocab$vocabulary[which(vocab$token == "<unk>")]
    words$word1[which(is.na(words$word1))] <- unk_vocab
    words$word2[which(is.na(words$word2))] <- unk_vocab
    words$word3[which(is.na(words$word3))] <- unk_vocab
    words$output[which(is.na(words$output))] <- unk_vocab

    # Calculate the number of quadgrams in the test set
    Wt <- nrow(words)

    # Print for sanity check
    print(paste0("Total number of quadgrams in the test set: ", Wt))

    # Defining an empty vector for accuracy
    accuracy <- vector(mode = "integer")

    # Get matches at the quadgram level
    # Get the top 3 choices for each combination of word1, word2, and word3
    top3 <- prob_table %>%
        select(word1, word2, word3, output, quadgram_prob) %>%
        slice_max(order_by = quadgram_prob, n = 3,
                  by = c(word1, word2, word3), with_ties = FALSE)
    # Match
    match <- inner_join(words, top3)
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    word_subset <- anti_join(words, match)
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
    match <- inner_join(word_subset, top3)
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    word_subset <- anti_join(word_subset, match)
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
    match <- inner_join(word_subset, top3)
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    word_subset <- anti_join(word_subset, match)
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
    match <- inner_join(word_subset, top3)
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    word_subset <- anti_join(word_subset, match)
    # Tracker
    print("Unigrams with word1, word2 context matched")

    # Get the top 3 choices for each word2
    top3 <- prob_table %>%
        select(word2, output, bo_unigram_prob) %>%
        distinct(word2, output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3,
                  by = word2, with_ties = FALSE)
    # Match
    match <- inner_join(word_subset, top3)
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    word_subset <- anti_join(word_subset, match)
    # Tracker
    print("Unigrams with word2 context matched")

    # Get the top 3 choices for each word2
    top3 <- prob_table %>%
        select(word1, output, bo_unigram_prob) %>%
        distinct(word1, output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3,
                  by = word1, with_ties = FALSE)
    # Match
    match <- inner_join(word_subset, top3)
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    word_subset <- anti_join(word_subset, match)
    # Tracker
    print("Unigrams with word1 context matched")

    # Get the top 3 choices for each word2
    top3 <- prob_table %>%
        select(output, bo_unigram_prob) %>%
        distinct(output, .keep_all = TRUE) %>%
        slice_max(order_by = bo_unigram_prob, n = 3, with_ties = FALSE)
    # Match
    match <- inner_join(word_subset, top3)
    accuracy <- c(accuracy, nrow(match))
    # Exclude out already matched quadgrams
    word_subset <- anti_join(word_subset, match)
    # Tracker
    print("Unigrams with no context matched")

    # Calculate accuracy
    accuracy <- sum(accuracy)/Wt

    # Return a evaluation table
    return(tibble(model = "mkn_quadgram", sample_size = N,
                  cross_entropy = NA, perplexity = NA, accuracy = accuracy))
}

# Load in the validation data set
load("data/validation_sentences.RData")

# Evaluate the unigram model
result <- evaluate_quadgram_model(quadgram_model_probs, quadgram_model_vocab, validation, Nsample)

# Save this result
write.table(result, file = "data/mkn_metrics.csv", row.names = FALSE,
            append = TRUE, col.names = FALSE, sep = ",", quote = FALSE)

# Save the model
save(quadgram_model_probs, quadgram_model_vocab, file = "data/mkn_quadgram_model_1halfM.RData")

# Remove
rm(quadgram_model_probs, quadgram_model_vocab, result)
gc()

