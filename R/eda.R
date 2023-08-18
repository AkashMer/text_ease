#' Load in our tidy data
text_ngrams <- read.csv("data/text_ngrams.csv", header = TRUE)

library(tidymodels)
tidymodels_prefer()

# Convert the source and ngram columns to factor
text_ngrams <- tibble(text_ngrams) %>%
    mutate(source = factor(source),
           ngram = factor(ngram, levels = c("word", "bigram", "trigram",
                                            "quadgram")))

#' Let's split this data into training and test sets
#' strata would be the type of ngram and source type, there by having all types
#' of ngrams and all source types represented equally among both sets

# Getting indices for strata which combine both columns
temp_df <- text_ngrams %>%
    select(source, ngram) %>%
    group_by(source, ngram) %>%
    group_indices() -> indeces

set.seed(536189)
trainSplit <- initial_split(cbind(text_ngrams, indeces),
                            prop = 0.8, strata = indeces)
# Defining the train set
train <- training(trainSplit)
# Defining the test set
test <- testing(trainSplit)

# Saving these data sets to the data folder
write.csv(train, "data/train.csv", row.names = FALSE)
write.csv(test, "data/test.csv", row.names = FALSE)

# Compressing all the data files created for this project into a zip file
library(zip)
filesToZip <- list.files(path = paste0(getwd(), "/data"), pattern = ".csv$")
filesToZip <- paste0("data/", filesToZip)
zipr("data/tidyData.zip", files = filesToZip)

# Deleting the noncompressed files
unlink(filesToZip)

#' Let's explore the unique numbers of each type of ngram and their counts
#' This will inform us on how often to longer ngrams occur

train %>%
    count(ngram, token, sort = TRUE) %>%
    group_by(ngram) %>%
    summarise(length = length(token), max = max(n)) %>%
    ggplot(aes(ngram, length, size = max)) +
    geom_point() +
    labs(title = "Unique n-grams by n-gram order",
         x = NULL, y = "Number of unique n-grams",
         size = "Count of\nmost frequent\nn-gram")

#' This implies that smaller order n-grams have lower number of unique n-grams
#' but their count keep getting smaller and smaller by a large magnitude
#' Thus, **Larger order n-grams might give smaller and smaller values of **
#' **probability of the next word, resulting into unnecessarily complex and **
#' **time-consuming computations**

#' Distribution of all the words in our data set

# Word frequency plots for each ngram
train %>%
    count(ngram, token) %>%
    ggplot(aes(n, fill = ngram)) +
    geom_histogram(bins = 30, show.legend = FALSE) +
    facet_wrap("ngram", nrow = 2) +
    labs(title = "ngram Distribution", x = "Counts of each unique ngram",
         y = "Count")
#' There are a lot of ngrams which occur only once or close to around that and
#' this increased as we go to higher n-gram order.
#' This once again confirms what we saw in *Fig 1*
#' Let's get a closer look at these, understand their nature by finding out
#' the top 10 and bottom 10 for each type of ngram

# Calculating the count of each unique ngram
ngramCount <- train %>%
    count(ngram, token, sort = TRUE) %>%
    pivot_wider(names_from = ngram, values_from = n)

library(cowplot)
# Top 10 and Bottom 10 unigrams
plotA <- ngramCount %>%
    select(token, word) %>%
    na.omit() %>%
    arrange(desc(word)) %>%
    slice(1:10,(length(token)-9):length(token)) %>%
    mutate(token = reorder(token, word)) %>%
    ggplot(aes(word, token)) +
    geom_col(fill = "#F8766D") +
    labs(title = "Words", y = NULL, "Count")

# Top 10 and Bottom 10 bigrams
plotB <- ngramCount %>%
    select(token, bigram) %>%
    na.omit() %>%
    arrange(desc(bigram)) %>%
    slice(1:10,(length(token)-9):length(token)) %>%
    mutate(token = reorder(token, bigram)) %>%
    ggplot(aes(bigram, token)) +
    geom_col(fill = "#00BFC4") +
    labs(title = "Bigrams", y = NULL, x = "Count")

# Top 10 and Bottom 10 trigrams
plotC <- ngramCount %>%
    select(token, trigram) %>%
    na.omit() %>%
    arrange(desc(trigram)) %>%
    slice(1:10,(length(token)-9):length(token)) %>%
    mutate(token = reorder(token, trigram)) %>%
    ggplot(aes(trigram, token)) +
    geom_col(fill = "#7CAE00") +
    labs(title = "Trigrams", y = NULL, x = "Count")

# Top 10 and Bottom 10 quadgrams
plotD <- ngramCount %>%
    select(token, quadgram) %>%
    na.omit() %>%
    arrange(desc(quadgram)) %>%
    slice(1:10,(length(token)-9):length(token)) %>%
    mutate(token = reorder(token, quadgram)) %>%
    ggplot(aes(quadgram, token)) +
    geom_col(fill = "#C77CFF") +
    labs(title = "Quadgrams", y = NULL, x = "Count")


# Top 10 and Bottom 10 n-gram plot
plot_grid(plotA, plotB, plotC, plotD, nrow = 2) +
    labs(title = "Top 10 and Bottom 10 n-grams")

#' Points to note:
#'
#' * Top n-grams contain most commonly used words which are known as stop words
#' * Least frequent n-grams for our data set includes foreign language words
#' * The difference between these is huge but keeps decreasing for higher order
#' n-grams
#' * The top 3 unigrams are `the, to and and`. `to and and` are usually not the
#' words used to start sentences, thus we will have to introduce some kind of
#' place holder to represent the beginning of a sentence to ensure the model
#' predicts even when no text is entered.
#'
#' **we will keep the stop words for now** since our aim is to build a text
#' prediction model and these words will be very useful.
#'
#' **We will remove these foreign language words by stemming our data set**
#' since these words are more individualized and thus come from a different
#' population of text with it's own probabilities. We can revisit during
#' fine tuning of our models
#'
#' **Introduce a place holder at the start of each sentence**

#' Time to revisit **Decision 1** which we made where we combined the text
#' from all source types. Let's check if different sources use different
#' proportion of words. We will remove stop words since these will be the same
#' across all sources, and thus may drive the correlation between sources

library(tidytext)

frequency <- train %>%
    filter(ngram == "word") %>%
    mutate(token = tolower(token)) %>%
    anti_join(stop_words, join_by(token == word)) %>%
    count(source, token) %>%
    group_by(source) %>%
    mutate(proportion = n/sum(n)) %>%
    select(-n) %>%
    pivot_wider(names_from = source, values_from = proportion) %>%
    pivot_longer('blogs':'news',
                 names_to = "source", values_to = "proportion")

library(scales)
# Plot and compare the frequency of these words from each source
ggplot(frequency, aes(x = proportion, y = twitter,
                      color = abs(twitter - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0,0.001),
                         low = "darkslategray4", high = "gray75") +
    facet_wrap(~source, ncol = 2) +
    theme(legend.position = "none") +
    labs(y = "Twitter", x = NULL)

#' Things to notice:
#'
#' * Blogs and Twitter use similar group of words, where as, news is different.
#' Let's confirm this using a correlation test

cor.test(data = frequency[frequency$source == "blogs",],
         ~ proportion + twitter)

cor.test(data = frequency[frequency$source == "news",],
         ~ proportion + twitter)

#' These support our claim that blogs and twitter share a similar language
#' morphology as compared to news and twitter
#' * Numbers and punctuation seem to be included in our tokens. Let's check
#' how much impact do these have on the correlation of language morphology.

# Without punctuation and numbers
library(stringr)
frequency <- train %>%
    filter(ngram == "word") %>%
    mutate(token = tolower(token)) %>%
    anti_join(stop_words, join_by(token == word)) %>%
    mutate(token = str_replace_all(token, "[:digit:]+|[:punct:]+", "")) %>%
    count(source, token) %>%
    group_by(source) %>%
    mutate(proportion = n/sum(n)) %>%
    select(-n) %>%
    pivot_wider(names_from = source, values_from = proportion) %>%
    pivot_longer('blogs':'news',
                 names_to = "source", values_to = "proportion")

# Plot and compare the frequency of these words from each source
ggplot(frequency, aes(x = proportion, y = twitter,
                      color = abs(twitter - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0,0.001),
                         low = "darkslategray4", high = "gray75") +
    facet_wrap(~source, ncol = 2) +
    theme(legend.position = "none") +
    labs(y = "Twitter", x = NULL)

# Let's confirm this using a correlation test
cor.test(data = frequency[frequency$source == "blogs",],
         ~ proportion + twitter)

cor.test(data = frequency[frequency$source == "news",],
         ~ proportion + twitter)

#' Now all the text sources are much closer to each other
#' Thus,
#' This confirms that we should **exclude numbers and punctuation in order to **
#' **build a prediction model which will perform well in all these source types.**
#' The inclusion of data from all sources, instead of just 1 or 2 is favored
#' since we will be able to build a more generalized model instead of
#' overfitting to one type of source.

#' Final pre-processing steps for our data,
#'
#' 1. **Introduce a place holder to indicate start sentences** to
#' preserve natural syntax of the language which will help the model predict
#' appropriate words accordingly
#' 2. **Use bi-grams only** to help reduce computational time. For
#' higher order inputs we will use the intuition behind n-gram modeling and
#' derive the higher order n-gram probabilities from bigrams.
#' 2. **Remove foreign language words** since these words are more
#' individualized and thus come from a different population of text with it's
#' own probabilities.
#' 3. **Remove the numbers** - It is difficult to predict numbers that will be
#' entered by the user, since the purpose of the numbers could be anything, for eg.,
#' time of the day, some sort of measurement, score on a test, value of pi, and
#' thus can take on any number from `-Inf` to `+Inf`. Although this may prevent
#' the model to predict let's say, am and pm, after the user enters let's meet at
#' 5 ... . We will come back to this if the prediction accuracy of our model is
#' low
#' 4. **Remove punctuation** since punctuation represents the grammar of the
#' language and is affected less by the accompanying 3-4 words, but by the
#' entire sentence. This would be out of the scope of our prediction model
#' 5. **Keep the stop words** - Since our purpose is to predict while the user
#' is typing, inclusion of stop words would be very helpful and may even be
#' pleasant for the user since they wont have to focus on these words while
#' typing
#'
#' Although the downside of keeping stop words is it is likely for our model to
#' always predict these over others, thus to avoid this scenario, **we will not**
#' **use count as the statistic, but the probability calculated by the**
#' **following formula**

# Reading in our sample subset
textSentences <- read.csv(unz("data/tidyData.zip", "textSentences.csv"),
                          header = TRUE)

# Removing all numbers and punctuation
# Adding placeholder <s> at start of each sentence
library(stringr)
library(dplyr)
textSentencesTransformed <- tibble(textSentences) %>%
    mutate(sentences = str_replace_all(sentences, "[:digit:]+|[:punct:]+", "")) %>%
    mutate(sentences = paste0("sss ", sentences))

# Getting bigrams, remove punctuation and numbers
library(tidytext)
bigrams <- textSentencesTransformed %>%
    unnest_tokens(output = bigram,
                  input = sentences,
                  token = "ngrams",
                  n = 2)

# Check and remove missing values if present
if(mean(complete.cases(bigrams)) < 1) {
    bigrams <- na.omit(bigrams)
}

# Dividing into train and test sets
library(tidymodels)
tidymodels_prefer()
set.seed(651843)
trainSplit <- initial_split(bigrams, prop = 0.8, strata = source)
train <- training(trainSplit)
test <- testing(trainSplit)

# Calculating the numerator of the formula
bigram_counts_train <- tibble(train) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, name = "num")
# Doing the same for the test set
bigram_counts_test <- tibble(test) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, word2, name = "num")

# Calculating the denominator of the formula
unigram_counts_train <- tibble(train) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, name = "den")
# Doing the same for the test set
unigram_counts_test <- tibble(test) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    count(word1, name = "den")

# Executing our formula
bigram_prob_train <- left_join(bigram_counts_train, unigram_counts_train) %>%
    mutate(prob = num/den) %>%
    select(-c(num,den))
# Doing the same for the test set
bigram_prob_test <- left_join(bigram_counts_test, unigram_counts_test) %>%
    mutate(prob = num/den) %>%
    select(-c(num,den))

