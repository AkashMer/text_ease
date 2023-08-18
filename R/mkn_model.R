# English text data extracted using winrar in file system

# Getting line counts for faster reading with count_lines function
library(dplyr)
dataSize <- tibble(twitter = system("wc data/en_US/en_US.twitter.txt",
                                    intern = TRUE),
                   blogs = system("wc data/en_US/en_US.blogs.txt",
                                  intern = TRUE),
                   news = system("wc data/en_US/en_US.news.txt",
                                 intern = TRUE))
t(dataSize)

# Reading in 50000 random lines from each file
library(LaF)
library(dplyr)
Nsample <- 200000
set.seed(8923)
twitter <- sample_lines("data/en_US/en_US.twitter.txt", n = Nsample, nlines = 2360148)
set.seed(8923)
blogs <- sample_lines("data/en_US/en_US.blogs.txt", n = Nsample, nlines = 899288)
set.seed(8923)
news <- sample_lines("data/en_US/en_US.news.txt", n = Nsample, nlines = 1010242)

# Combining all into one long character vector
set.seed(8923)
textData <- c(twitter, blogs, news) %>%
    sample()

# Defining a function to clean
tidy_up <- function(x) {
    require(dplyr)
    require(qdapRegex)

    # Remove \r
    x <- gsub("\r", "", x)

    # Remove RT
    x <- gsub("RT :", "", x)

    # Lower-case everything and remove characters which are not alphanumeric
    # or punctuation
    x <- kgrams::preprocess(x, erase = "[^.?![:alpha:][:space:]]")

    # Collapse everything to a single string
    x <- paste(x, collapse = " ")

    # Tokenize sentences
    x <- kgrams::tknz_sent(x, keep_first = TRUE)

    # Remove empty sentences
    x <- x[x != ""]

    # # Remove non words
    # x <- rm_non_words(x)

    # Remove repeating characters
    x <- rm_repeated_characters(x)

    # Remove really short sentences
    x <- x[-which(nchar(x) <= 5)]

    # Add bos and eos
    x <- paste0("bos1 bos2 bos3 ", x, " eos")

    # Returning x
    return(x)
}

# Tidyup the data and split intp sentences
tidyData <- tidy_up(textData)

# Remove everything beside the tidyData
rm(blogs, news, Nsample, textData, twitter, tidy_up)
gc()

# Split data into train and test sets
# Test set should be just 100 sentences, since testing takes a long time
# otherwise
library(dplyr)
set.seed(15823)
inTest <- sample(1:length(tidyData), size = 100)
test <- tibble(text = tidyData[inTest])
train <- tibble(text = tidyData[-inTest])

# Remove
rm(tidyData, inTest)
gc()

# Let's get the vocabulary values and a distribution of words for each sentence
library(ggplot2)
library(tokenizers)
train %>%
    mutate(n_words = tokenizers::count_words(text)) %>%
    ggplot(aes(n_words)) +
    geom_bar()
#' Very few have sentences longer then 75
#' Max words per sentence = 55000
length(unique(unlist((tokenize_words(train$text, strip_punct = FALSE,
                                     strip_numeric = FALSE, simplify = TRUE)))))
#' 265534 which includes bos1, bos2, bos3, eos
word_list <- unlist(tokenize_words(train$text, strip_punct = FALSE,
                                   strip_numeric = FALSE, simplify = TRUE))
words_at_1 <- tibble(word = word_list) %>%
    count(word) %>%
    filter(n == 1)
# Get the list of OOV words
OOVwords <- words_at_1$word

# Divide the train set into quadgrams
train_quad <- tibble(text = unlist(tokenize_ngrams(train$text, n = 4L,
                                                   simplify = TRUE)))

# Remove train set, and save test set for later use
saveRDS(test, "data/test")
rm(train, test, words_at_1, word_list)
gc()

library(textrecipes)
# Recipe which will give one hot encoding of these sequences
train_rec <- recipe(~text, data = train_quad) %>%
    step_tokenize(text, options = list(strip_punct = FALSE,
                                       strip_numeric = FALSE)) %>%
    step_sequence_onehot(text, sequence_length = 4)
# Prep the recipe
train_prep <- prep(train_rec)

# Remove train_quad
rm(train_quad)
gc()

# Bake the data
baked_train <- bake(train_prep, new_data = NULL)
names(baked_train) <- c("word1", "word2", "word3", "word4")

# Get the word to integer conversion index
word_index <- tidy(train_prep, 2)

# Save and remove the prep and recipe object for later use
rm(train_rec, train_prep)
gc()

# Vocab size
word_index

# Find vocabulary values for all OOVwords
OOVwordsIndex <- word_index$vocabulary[which(word_index$token %in% OOVwords)]

# # Reduce word_index without these
# word_index_OOVout <- word_index[-OOVwordsIndex, ]

# Add <unk> word token to the vocabulary
unkWord <- tibble(vocabulary = as.integer(nrow(word_index)+1),
                  token = "<unk>")

# Define the final vocabulary
vocab_index <- rbind(word_index[,2:3], unkWord)

# Define a OOVwordsindex dictionary
OOVwords_index <- tibble(vocabulary = OOVwordsIndex,
                         token = OOVwords)

# Replacing each OOVword with this same number
replacement <- as.integer(nrow(word_index)+1)
baked_train_final <- baked_train %>%
    mutate(word1 = ifelse(word1 %in% OOVwords_index$vocabulary,
                          replacement, word1),
           word2 = ifelse(word2 %in% OOVwords_index$vocabulary,
                          replacement, word2),
           word3 = ifelse(word3 %in% OOVwords_index$vocabulary,
                          replacement, word3),
           word4 = ifelse(word4 %in% OOVwords_index$vocabulary,
                          replacement, word4))

# Final quadgrams in the train set one hot encoded for smaller space requirement
baked_train_final
object.size(baked_train_final)/10^6

# Associated vocabulary dictionary
vocab_index
object.size(vocab_index)/10^6

# Let's remove all unnecessary objects
rm(baked_train, unkWord, word_index, OOVwords, OOVwordsIndex)
gc()

# # Divide baked_train_final into 1 analysis and 1 assessment sets to help
# # with determination of discounting values
# set.seed(15823)
# in_analysis <- sample(1:nrow(baked_train_final),
#                       size = floor(0.6*nrow(baked_train_final)))
# analysis_set <- baked_train_final[in_analysis,]
# assessment_set <- baked_train_final[-in_analysis,]

# We will be building a interpolated modified Kneser-Ney model with no
# need of back off due to our introduction of bos1, bos2, bos3
# Term1 = max(count(word1-word4)-D,0)/c(word1-word3)
train_counts <- baked_train_final %>%
    count(word1, word2, word3, word4, name = "raw_count")
summary(train_counts$raw_count)
# Remove baked_train_final, since no longer needed
rm(baked_train_final)
gc()
# Calculate D1, D2 and D3 from the formula
count_of_counts <- train_counts %>%
    count(raw_count) %>%
    filter(raw_count <= 5)
# Y = n1/(n1+2*n2)
Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
# D1 = 1 - 2Y(n2/n1)
D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
# D2 = 2 - 3Y(n3/n2)
D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
# D3 = 3 - 4Y(n4/n3)
D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])
# Getting the modified counts
train_counts1 <- train_counts %>%
    filter(raw_count == 1) %>%
    mutate(mkn_count = max(raw_count - D1, 0))
train_counts2 <- train_counts %>%
    filter(raw_count == 2) %>%
    mutate(mkn_count = max(raw_count - D2, 0))
train_counts3plus <- train_counts %>%
    filter(raw_count >= 3) %>%
    mutate(mkn_count = max(raw_count - D3, 0))
train_counts <- rbind(train_counts1, train_counts2, train_counts3plus)
train_counts
# Getting the denominator and calculating term1
train_counts <- train_counts %>%
    add_count(word1, word2, word3, name = "den") %>%
    mutate(term1 = mkn_count/den) %>%
    select(-mkn_count)
train_counts
# gamma1
train_counts1 <- train_counts %>%
    filter(raw_count == 1) %>%
    mutate(gamma1 = D1*length(unique(word4)), .by = c(word1, word2, word3))
train_counts2 <- train_counts %>%
    filter(raw_count == 2) %>%
    mutate(gamma1 = D2*length(unique(word4)), .by = c(word1, word2, word3))
train_counts3plus <- train_counts %>%
    filter(raw_count >= 3) %>%
    mutate(gamma1 = D3*length(unique(word4)), .by = c(word1, word2, word3))
train_counts <- rbind(train_counts1, train_counts2, train_counts3plus)
train_counts <- train_counts %>%
    mutate(gamma1 = gamma1/den) %>%
    select(-c(raw_count, den))
train_counts

# Moving onto term2 and gamma2
# term2
train_counts <- train_counts %>%
    mutate(kn_count = length(unique(word1)), .by = c(word2, word3, word4))
# Calculate D1, D2, and D3 again
count_of_counts <- train_counts %>%
    count(kn_count) %>%
    filter(kn_count <= 5)
# Y = n1/(n1+2*n2)
Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
# D1, D2, D3
D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])
# Getting the modified counts
train_counts1 <- train_counts %>%
    filter(kn_count == 1) %>%
    mutate(mkn_count = max(kn_count - D1, 0))
train_counts2 <- train_counts %>%
    filter(kn_count == 2) %>%
    mutate(mkn_count = max(kn_count - D2, 0))
train_counts3plus <- train_counts %>%
    filter(kn_count >= 3) %>%
    mutate(mkn_count = max(kn_count - D3, 0))
train_counts <- rbind(train_counts1, train_counts2, train_counts3plus)
train_counts
# Getting the denominator and calculating pkn
train_counts <- train_counts %>%
    mutate(den = sum(kn_count), .by = word4,
           term2 = mkn_count/den) %>%
    select(-mkn_count)
train_counts
# gamma2
train_counts1 <- train_counts %>%
    filter(kn_count == 1) %>%
    mutate(gamma2 = D1*length(unique(word4)), .by = c(word2, word3))
train_counts2 <- train_counts %>%
    filter(kn_count == 2) %>%
    mutate(gamma2 = D2*length(unique(word4)), .by = c(word2, word3))
train_counts3plus <- train_counts %>%
    filter(kn_count >= 3) %>%
    mutate(gamma2 = D3*length(unique(word4)), .by = c(word2, word3))
train_counts <- rbind(train_counts1, train_counts2, train_counts3plus)
train_counts <- train_counts %>%
    mutate(gamma2 = gamma2/den) %>%
    select(-c(kn_count, den))
train_counts

# Term3 and gamma 3
# term3
train_counts <- train_counts %>%
    mutate(kn_count = length(unique(word2)), .by = c(word3, word4))
# Calculate D1, D2, and D3 again
count_of_counts <- train_counts %>%
    count(kn_count) %>%
    filter(kn_count <= 5)
# Y = n1/(n1+2*n2)
Y <- count_of_counts$n[1]/(count_of_counts$n[1] + 2*count_of_counts$n[2])
# D1, D2, and D3
D1 <- 1 - 2*Y*(count_of_counts$n[2]/count_of_counts$n[1])
D2 <- 2 - 3*Y*(count_of_counts$n[3]/count_of_counts$n[2])
D3 <- 3 - 4*Y*(count_of_counts$n[4]/count_of_counts$n[3])
# Getting the modified counts
train_counts1 <- train_counts %>%
    filter(kn_count == 1) %>%
    mutate(mkn_count = max(kn_count - D1, 0))
train_counts2 <- train_counts %>%
    filter(kn_count == 2) %>%
    mutate(mkn_count = max(kn_count - D2, 0))
train_counts3plus <- train_counts %>%
    filter(kn_count >= 3) %>%
    mutate(mkn_count = max(kn_count - D3, 0))
train_counts <- rbind(train_counts1, train_counts2, train_counts3plus)
train_counts
# Getting the denominator and calculating pkn
train_counts <- train_counts %>%
    mutate(den = sum(kn_count), .by = word4,
           term3 = mkn_count/den) %>%
    select(-mkn_count)
train_counts
# gamma3
train_counts1 <- train_counts %>%
    filter(kn_count == 1) %>%
    mutate(gamma3 = D1*length(unique(word4)), .by = word3)
train_counts2 <- train_counts %>%
    filter(kn_count == 2) %>%
    mutate(gamma3 = D2*length(unique(word4)), .by = word3)
train_counts3plus <- train_counts %>%
    filter(kn_count >= 3) %>%
    mutate(gamma3 = D3*length(unique(word4)), .by = word3)
train_counts <- rbind(train_counts1, train_counts2, train_counts3plus)
train_counts <- train_counts %>%
    mutate(gamma3 = gamma3/den) %>%
    select(-c(kn_count, den))
train_counts

# term4
train_counts <- train_counts %>%
    mutate(term4_num = length(unique(word3)), .by = word4)
train_counts
train_counts <- train_counts %>%
    mutate(term4 = term4_num/sum(term4_num), .keep = "unused")
train_counts

# Probability of each word by interpolated modified Kneser-ney model
train_prob <- train_counts %>%
    mutate(prob = term1 + gamma1*(term2 + gamma2*(term3 + gamma3*term4)),
           .keep = "unused")
train_prob
object.size(train_prob)/10^6

# Clean up train_counts
rm(count_of_counts, train_counts, train_counts1, train_counts2,
   train_counts3plus, D1, D2, D3, Y)
gc()

# Save everything
save(train_prob, vocab_index, OOVwords_index, file = "data/mkn_model.RData")

# Defining a function which will calculate cross entropy and perplexity on the
# test set
library(tokenizers)
test <- readRDS("data/test")
test_quad <- tokenize_ngrams(test$text, n = 4L, simplify = TRUE)

prob_of_s <- function(s) {

    require(dplyr)

    quadgrams <- tibble(quadgram = s) %>%
        tidyr::separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

    # Get the vocabulary
    vocab <- vocab_index %>% filter(!(token %in% OOVwords_index$token))

    # Perform one hot encoding based on train one hot index
    input <- tibble(word1 = vocab$vocabulary[match(quadgrams$word1, vocab$token)],
                    word2 = vocab$vocabulary[match(quadgrams$word2, vocab$token)],
                    word3 = vocab$vocabulary[match(quadgrams$word3, vocab$token)],
                    word4 = vocab$vocabulary[match(quadgrams$word4, vocab$token)])

    # Change any NAs to unkown token index
    unk_token_index <- 265532L
    input$word1 <- Hmisc::impute(input$word1, unk_token_index)
    input$word2 <- Hmisc::impute(input$word2, unk_token_index)
    input$word3 <- Hmisc::impute(input$word3, unk_token_index)
    input$word4 <- Hmisc::impute(input$word4, unk_token_index)

    # Get the probabilties of the quadgrams in the sentence
    probs <- vector(mode = "double")
    for(i in 1:nrow(input)) {
        # Filter by word1, if not there, convert to unk_token_index
        if(input$word1[i] %in% train_prob$word1) {
            res <- train_prob %>%
                filter(word1 == input$word1[i])
        } else {
            input$word1[i] <- unk_token_index
            res <- train_prob %>%
                filter(word1 == input$word1[i])
        }

        # Filter by word2, if not there, convert to unk_token_index
        if(input$word2[i] %in% res$word2) {
            res <- res %>%
                filter(word2 == input$word2[i])
        } else {
            input$word2[i] <- unk_token_index
            res <- res %>%
                filter(word2 == input$word2[i])
            if(nrow(res) == 0) {
                input$word1[i] <- unk_token_index
                res <- train_prob %>%
                    filter(word1 == input$word1[i],
                           word2 == input$word2[i])
            }
        }

        # Filter by word3, if not there, convert to unk_token_index
        if(input$word3[i] %in% res$word3) {
            res <- res %>%
                filter(word3 == input$word3[i])
        } else {
            input$word3[i] <- unk_token_index
            res <- res %>%
                filter(word3 == input$word3[i])
            if(nrow(res) == 0) {
                input$word2[i] <- unk_token_index
                res <- train_prob %>%
                    filter(word1 == input$word1[i],
                           word2 == input$word2[i],
                           word3 == input$word3[i])
            }
            if(nrow(res) == 0) {
                input$word1[i] <- unk_token_index
                input$word2[i] <- unk_token_index
                res <- train_prob %>%
                    filter(word1 == input$word1[i],
                           word2 == input$word2[i],
                           word3 == input$word3[i])
            }
        }

        # Filter by word4, if not there, convert to unk_token_index
        if(input$word4[i] %in% res$word4) {
            res <- res %>%
                filter(word4 == input$word4[i])
        } else {
            input$word4[i] <- unk_token_index
            res <- res %>%
                filter(word4 == input$word4[i])
            if(nrow(res) == 0) {
                input$word3[i] <- unk_token_index
                res <- train_prob %>%
                    filter(word1 == input$word1[i],
                           word2 == input$word2[i],
                           word3 == input$word3[i],
                           word4 == input$word4[i])
            }
            if(nrow(res) == 0) {
                input$word2[i] <- unk_token_index
                input$word3[i] <- unk_token_index
                res <- train_prob %>%
                    filter(word1 == input$word1[i],
                           word2 == input$word2[i],
                           word3 == input$word3[i],
                           word4 == input$word4[i])
            }
            if(nrow(res) == 0) {
                input$word1[i] <- unk_token_index
                input$word2[i] <- unk_token_index
                input$word3[i] <- unk_token_index
                res <- train_prob %>%
                    filter(word1 == input$word1[i],
                           word2 == input$word2[i],
                           word3 == input$word3[i],
                           word4 == input$word4[i])
            }
        }

        # Print a tracking number
        print("Probability of quadgram obtained")
        probs[i] <- res$prob
    }

    return(probs)
}

probs_of_s <- lapply(1:length(test_quad), function(j) {
    p <- prob_of_s(test_quad[[j]])
    print(paste0("Sentence done ", j))
    return(p)
})

# Convert to log base 2
log_probs_of_s <- lapply(probs_of_s, log2)
# Calculate the probability of each sentence
test_sentences_probs <- lapply(log_probs_of_s, sum)
# Calculate the probability of the entire test corpus
test_prob <- sum(unlist(test_sentences_probs))

# Calculating cross entropy
Wt <- nrow(vocab_index %>% filter(!(token %in% OOVwords_index$token))) - 3 + 1
cross_entropy <- test_prob/Wt

# Perplexity
perplexity <- 2^cross_entropy

# Saving this result
perplexity_table <- tibble(model = "mkn_model", loss = NA, perplexity = perplexity)
write.csv(perplexity_table, file = "data/loss_perplexity_table.csv", row.names = FALSE)

# Make sure to remove eos and changing all unk word4 to original values
# Defining the prediction function
predict3 <- function(string = "", vocab, prob_table) {

    require(dplyr)

    # Remove any extra white space
    string <- qdapRegex::rm_white(string)

    # Divide the string into words and remove any punctuations and numbers
    string <- unlist(tokenizers::tokenize_words(string, strip_punct = TRUE,
                                                strip_numeric = TRUE))

    # Add bos1, bos2, bos3 to the begining
    string <- c("bos1", "bos2", "bos3", string)

    # Consider only the last 3 words
    string <- string[(length(string)-2):length(string)]

    # Change to vocabulary
    string <- vocab$vocabulary[match(string, vocab$token)]
    input <- string

    # Change any NAs to unkown token index
    unk_token_index <- 265532L
    input <- Hmisc::impute(string, unk_token_index)

    # Find the top 3 next words
    # Filter by word1, if not there, convert to unk_token_index
    if(string[1] %in% prob_table$word1) {
        res <- prob_table %>%
            filter(word1 == string[1])
    } else {
        string[1] <- unk_token_index
        res <- prob_table %>%
            filter(word1 == string[1])
    }

    # Filter by word2, if not there, convert to unk_token_index
    if(string[2] %in% res$word2) {
        res <- res %>%
            filter(word2 == string[2])
    } else {
        string[2] <- unk_token_index
        res <- res %>%
            filter(word2 == string[2])
        # If no matches found, convert word1 to unknown token index
        if(nrow(res) == 0) {
            string[1] <- unk_token_index
            res <- prob_table %>%
                filter(word1 == string[1],
                       word2 == string[2])
        }
    }

    # Filter by word3, if not there, convert to unk_token_index
    if(string[3] %in% res$word3) {
        res <- res %>%
            filter(word3 == string[3])
    } else {
        string[3] <- unk_token_index
        res <- res %>%
            filter(word3 == string[3])
        # If no matches found, convert various combinations of previous words
        # to unknown token
        if(nrow(res) == 0) {
            string[1] <- unk_token_index
            res <- prob_table %>%
                filter(word1 == string[1],
                       word2 == string[2],
                       word3 == string[3])
        }
        if(nrow(res) == 0) {
            string[2] <- unk_token_index
            res <- prob_table %>%
                filter(word1 == string[1],
                       word2 == string[2],
                       word3 == string[3])
        }
        if(nrow(res) == 0) {
            string[1] <- unk_token_index
            string[2] <- unk_token_index
            res <- prob_table %>%
                filter(word1 == string[1],
                       word2 == string[2],
                       word3 == string[3])
        }
    }

    # If less than 3 matches found, use stupid back off (since large train size)
    if(nrow(res < 3)) {

        # Reset the input and only include last 2 words
        string <- input[2:3]

        # Find the top 3 next words
        # Filter word2, if not there, convert to unk_token_index
        if(string[1] %in% prob_table$word2) {
            res <- res %>%
                filter(word2 == string[1])
        } else {
            string[1] <- unk_token_index
            res <- res %>%
                filter(word2 == string[1])
        }

        # Filter word3, if not there, convert to unk_token_index
        if(string[2] %in% res$word3) {
            res <- res %>%
                filter(word3 == string[2])
        } else {
            string[2] <- unk_token_index
            res <- res %>%
                filter(word3 == string[2])
            # If no matches found, convert various combinations of previous words
            # to unknown token
            if(nrow(res) == 0) {
                string[1] <- unk_token_index
                res <- prob_table %>%
                    filter(word2 == string[1],
                           word3 == string[2])
            }
        }
    }

    # If less than 3 matches found, use stupid back off (since large train size)
    if(nrow(res < 3)) {

        # Reset the input and only include last 2 words
        string <- input[3]

        # Find the top 3 next words
        # Filter word3, if not there, convert to unk_token_index
        if(string[1] %in% prob_table$word3) {
            res <- res %>%
                filter(word3 == string[1])
        } else {
            string[1] <- unk_token_index
            res <- res %>%
                filter(word3 == string[1])
        }
    }

    # Arrange by probabilities and get top 3 one hot indices
    res <- res %>%
        arrange(desc(prob)) %>%
        select(word4)

    # Convert back to words
    output <- vocab_index$token[match(res$word4, vocab_index$vocabulary)]

    # Return the top 3 words
    return(output[1:3])
}


# Get the vocabulary
word_to_index <- vocab_index %>% filter(!(token %in% OOVwords_index$token))

# Checking the predict3 function
start <- Sys.time()
x <- predict3("all of his", word_to_index, train_prob)
end <- Sys.time()

# Save the predict3 function, train_prob and word_to_index
save(train_prob, word_to_index, predict3, file = "data/mkn_model_final.RData")

# ---------------------------------------------------------------------------
# Old, very slow model built using kgrams package

# library(kgrams)
# # Defining a preprocess function for our data
# .preprocess <- function(x) {
#     # Remove \r
#     x <- gsub("\r", "", x)
#     # Remove RT
#     x <- gsub("RT", "", x)
#     # Lower-case everything and remove characters which are not alphanumeric
#     # or punctuation
#     x <- kgrams::preprocess(x)
#     # Returning x
#     return(x)
# }
#
# y <- .preprocess(train)
#
# # Defining a function which will convert all text into sentences and delineate
# # <bos> and <eos>
# .tknz_sent <- function(x) {
#     # Collapse everything to a single string
#     x <- paste(x, collapse = " ")
#     # Tokenize sentences
#     x <- kgrams::tknz_sent(x, keep_first = TRUE)
#     # Remove empty sentences
#     x <- x[x != ""]
#     # Returining x
#     return(x)
# }
#
# y <- .tknz_sent(y)
#
# library(tokenizers)
# trigrams <- tokenize_ngrams(y, lowercase = FALSE,
#                           n = 3L, simplify = TRUE) %>%
#     unlist() %>%
#     na.omit()
#
# trigram_count <- tibble(token = trigrams) %>%
#     separate()
#
#
# # Defining a function which will calculate frequencies(counts) up to trigrams
# N <- 3 # Tried various values, N <= 3 is good
# freq <- kgram_freqs(N, .preprocess = .preprocess, .tknz_sent = .tknz_sent)
# summary(freq)
#
# # Applying the function
# process_sentences(text = train, freqs = freq, verbose = FALSE)

# # Trying a add_k model (Katz smoothing)
# modelK <- language_model(freq, smoother = "add_k", k = 5)
# summary(modelK)
#
# # Defining a Absolute Discontinuing model
# modelAbs <- language_model(freq, smoother = "abs", D = 0.5)
# summary(modelAbs)
#
# # Defining a Kneser-Ney Smoothing model
# modelKn <- language_model(freq, smoother = "kn", D = 0.5)
# summary(modelKn)

# # Defining a modified Kneser-Ney Smoothing model
# model_mkn <- language_model(freq, smoother = "mkn", D1 = 0.5, D2 = 0.5, D3 = 0.5)
# summary(model_mkn)

# # Compare all 4 models on the validation set
# model_compare <- tibble(katz = perplexity(text = validation, model = modelK),
#                         abs = perplexity(text = validation, model  = modelAbs),
#                         KN = perplexity(text = validation, model = modelKn),
#                         modKN = perplexity(text = validation, model = model_mkn))
#
# model_compare
#' Katz = 20000
#' Absolute Discounting = 650
#' Kneser-Ney = 501
#' Kneser-Ney = 501
# We can go for either kneser ney or modified kneser ney, both are much better
# and Katz is the worst

# # We will go for modified kneser ney model
# model <- language_model(freq, smoother = "mkn", D1 = 0.5, D2 = 0.5, D3 = 0.5)
# summary(model)

# Tuning parameters are N and D1, D2, and D3
# First we will tune for N
# Literature suggests N = 3 is usually enough
# Pentagram = 501
# param(model, "N") <- N-1
# parameters(model)
# perplexity(validation, model) # Quadgram = 479.7906
# gc()
# param(model, "N") <- N-2
# parameters(model)
# perplexity(validation, model) # Trigram = 434.7916
# gc()
# param(model, "N") <- N-3
# parameters(model)
# perplexity(validation, model) # Bigram = 419.4605
# gc()
# Final decision reserved till we can reduce the sample size

# parameters(model)
# # Defining a tune function
# tune <- function(D1_grid, D2_grid, D3_grid) {
#     res <- list(D1 = 0,  D2 = 0, D3 = 0, perplexity = Inf)
#     for (D1 in D1_grid)
#         for (D2 in D2_grid)
#             for (D3 in D3_grid) {
#                 param(model, "D1") <- D1
#                 param(model, "D2") <- D2
#                 param(model, "D3") <- D3
#                 perplexity <- perplexity(validation_subset, model)
#                 if (perplexity < res$perplexity)
#                     res <- list(D1 = D1,
#                                 D2 = D2,
#                                 D3 = D3,
#                                 perplexity = perplexity)
#             }
#     return(res)
# }
#
# # Defining our grid
# D1_grid <- c(0.8, 0.9); D2_grid <- D3_grid <- 0.99
# set.seed(9923)
# validation_subset <- validation[sample(1:30000, size = 10000)]
# best_vals <- tune(D1_grid, D2_grid, D3_grid)
# gc()
# best_vals
# $D1
# [1] 0.99, 0.9
#
# $D2
# [1] 0.99, 0.99
#
# $D3
# [1] 0.99, 0.99
#
# $perplexity
# [1] 355.0438, 342.1818

#' # Final model
#' model_final <- language_model(freq, smoother = "mkn", D1 = 0.9, D2 = 0.99, D3 = 0.99)
#' model_kn <- language_model(freq, smoother = "kn", D = 0.9)
#' # model_sbo <- language_model(freq, smoother = "sbo", lambda = 0.5)
#' summary(model_kn)
#' summary(model_final)
#' summary(model_sbo)
#'
#' # Quiz 2
#' options = c("insane", "callous", "asleep", "insensitive")
#' tibble(output = options,
#'        prob = probability(options %|% "then you must be", model_kn))
#'
#' #' Model is slow in predicting
#' #'
#' #' Ideas:
#' #'
#' #' 1. Reduce the length of input string
#' #' 2. Reduce the sample size
#' #' 3. Include built in dictionaries to increase the accuracy for small sample sizes
#'
#' # Trying idea 1
#' system.time({
#'     probability(options %|% "If this isn't the cutest thing you've ever seen, then you must be", model_final)
#' }) # user  system elapsed
#' #   62.94  180.55  243.76
#' system.time({
#'     probability(options %|% "you must be", model_final)
#' }) # user  system elapsed
#' #   65.86  178.82  245.02
#' # Does not make a difference
#'
#' # Trying Idea 2
#' # N = 10000
#' perplexity(validation, model_final)
#' # 83.22676
#' system.time({
#'     probability(options %|% "If this isn't the cutest thing you've ever seen, then you must be", model_final)
#' }) # user  system elapsed
#' #   61.23  195.66  257.46
#' system.time({
#'     probability(options %|% "then you must be", model_final)
#' }) # user  system elapsed
#' #   58.88  199.03  258.44
#' system.time({
#'     probability(options %|% "you must be", model_final)
#' }) # user  system elapsed
#' #   59.78  198.26  258.39
