#' Things to note:
#'
#' 1. Each source has a LOT of data
#' 2. Each source has different amount of data
#' 3. Blogs text data, despite having lower number of sources, has one of the
#' longest texts out of all the 3
#'
#' Before we start processing and tidying the data we have a few decisions and
#' plans to make
#'
#' 1. As was seen from Quiz 1 questions, the data is huge
#' 2. Each type of information source contains different sample size
#' 3. Blogs text data, despite getting data from lower number of blogs as
#' compared to twitter or news, it contains one of the most longest data
#' out of the all the data units
#'
#' **Decision 1** to make:
#'
#' *Option A* : We combine all the data into 1 data set
#' Advantages - simple, increases sample size
#' Disadvantages - Will not be able to capture the different language morphology
#' used among these different sources
#'
#' *Option B* : Keep them separate and then conduct different EDA and different
#' models for each
#' Advantages - Preserves the language morphology from each source
#' Disadvantages - Smaller source of information, more computationally heavy
#' and complex
#'
#' For now we will go for **Option A**, since it is much simpler, and may help
#' us capture unique combinations of ngrams which might be more useful
#'
#' **Decision 2** to make:
#' Do we use all the data from each source?
#' Counting the number of different elements in each text file
#' Each line in the files represents 50%-truncated data from one instance of a
#' source type. So it is natural that blog posts and news have longer texts as
#' compared to twitter
#' Let's just get an idea of the number of instances for each source type

# l = 899288  w = 37333958 c = 210160014 data/en_US/en_US.blogs.txt
#
# l = 2360148 w = 30357171 c = 167105338 data/en_US/en_US.twitter.txt
#
# l = 1010242 w = 34365905 c = 205811889 data/en_US/en_US.news.txt

library(R.utils)
twitterN <- countLines(unz("data/Coursera-SwiftKey.zip",
                           "final/en_US/en_US.twitter.txt", open = "rb"))
blogsN <- countLines(unz("data/Coursera-SwiftKey.zip",
                         "final/en_US/en_US.blogs.txt", open = "rb"))
newsN <- countLines(unz("data/Coursera-SwiftKey.zip",
                        "final/en_US/en_US.news.txt", open = "rb"))

cat("Number of instances from each source type:\n
    1. Twitter - ", twitterN/10^6, " million\n
    2. Blogs - ", blogsN/10^6, " million\n
    3. News - ", newsN/10^6, "million")

#' That is a lot of instances, we will randomly sample 50000 instances which
#' should provide a enough information for drawing conclusions on our population
#' as per law of large numbers and CLT

# # Reading in the data and subsetting only random 50000 entries
library(readr)
set.seed(65198)
twitterTextData <- read_lines(unz("data/Coursera-SwiftKey.zip",
                                           "final/en_US/en_US.twitter.txt"),
                              n_max = -1)[sample(1:twitterN, size = 50000)]
set.seed(65198)
blogsTextData <- read_lines(unz("data/Coursera-SwiftKey.zip",
                                  "final/en_US/en_US.blogs.txt"),
                              n_max = -1)[sample(1:blogsN, size = 50000)]
set.seed(65198)
newsTextData <- read_lines(unz("data/Coursera-SwiftKey.zip",
                                 "final/en_US/en_US.news.txt"),
                             n_max = -1)[sample(1:newsN, size = 50000)]

#' **Decision 3** to make:
#' What proportion of each source type should be used?
#' First we will convert all 3 source type texts to sentence tokens, then
#' select equal number of sentences.
#' This will prevent any effect of the amount of data from each source type,
#' and at the same time, this will remove any bias while we try to determine
#' if the language morphology among all 3 sources is different.
#' This is important, since we are trying to predict the next word, based on
#' language syntax learned from this data set, and our expectation is that all 3
#' source types should have different language morphology, hence it should impact
#' our predictions

# Unnesting into sentences
# For now, we will preserve the uppercase characters
library(dplyr)
library(tidytext)
twitterSentences <- tibble(source = "twitter", text = twitterTextData) %>%
    unnest_tokens(output = sentences,
                  input = text,
                  token = "sentences",
                  format = "text", # Confirms usage of tokenizers package
                  to_lower = FALSE)
blogsSentences <- tibble(source = "blogs", text = blogsTextData) %>%
    unnest_tokens(output = sentences,
                  input = text,
                  token = "sentences",
                  format = "text", # Confirms usage of tokenizers package
                  to_lower = FALSE)
newsSentences <- tibble(source = "news", text = newsTextData) %>%
    unnest_tokens(output = sentences,
                  input = text,
                  token = "sentences",
                  format = "text", # Confirms usage of tokenizers package
                  to_lower = FALSE)

#' As can be seen below all sources contain different number of sentences.
#' This justifies this transformation
cat("Minimum and Maximum number of characters in each sample data: \n
    1. Twitter - ", summary(nchar(twitterSentences))[c(1,6)], "\n
    2. Blogs - ", summary(nchar(blogsSentences))[c(1,6)], "\n
    3. News - ", summary(nchar(newsSentences))[c(1,6)])

#' Different sample sizes will be selected from these since twitter language
#' morphology is associated with shorter words
set.seed(75168)
twitterSentences <- twitterSentences[sample(1:nrow(twitterSentences), 80000),]
set.seed(75168)
blogsSentences <- blogsSentences[sample(1:nrow(blogsSentences), 60000),]
set.seed(75168)
newsSentences <- newsSentences[sample(1:nrow(newsSentences), 60000),]

#' This represents the data that we will be using to conduct out analysis and
#' build a prediction model

# Combining all into 1
textSentences <- rbind(twitterSentences, blogsSentences, newsSentences)

# Writing this subset to our data folder so it can be retrieved at any time
write.csv(textSentences, "data/textSentences.csv", row.names = FALSE)
#' This is a tidy form of data, with 1 observation unit(sentence) per row and 1
#' variable per column

#' Next processing step depends on our prediction goal
#'
#' **Prediction Goal** : Give word suggestions to the user depending on previously
#' entered words
#'
#' **Decision 4** to make:
#' How many minimum and maximum words should be entered by the user to trigger
#' predictions?
#' We will limit ourselves to 0-3 words, anything more than this, only the
#' last three words will be taken into consideration
#' *Impact of this decision* - We will unnest our sentences further down to
#' include 4 word ngrams(quadgrams), 3 word ngrams(trigrams), 2 word
#' ngrams(bigrams) and 1 word ngrams(words)
#' Following functions automatically lowercase everything for us
#' For now we will be converting everything to lowercase, we will revisit this
#' decision when we are trying to improve our prediction model

# Tokenizing to 4-word ngrams/quadgrams
quadgrams <- textSentences %>%
    unnest_tokens(output = token,
                  input = sentences,
                  token = "ngrams",
                  n = 4L) %>%
    mutate(ngram = "quadgram")
# Check if any of them are NA
mean(complete.cases(quadgrams))
#' `r round((1-mean(complete.cases(quadgrams)))*100, 2)`% of the data has
#' missing values since the sentences ended before a 4 word ngram could be
#' formed
#' For now we will simply omit these values, and check back again in case of
#' poor model prediction
quadgrams <- na.omit(quadgrams)

# Tokenizing to 3-word ngrams(trigrams)
trigrams <- textSentences %>%
    unnest_tokens(output = token,
                  input = sentences,
                  token = "ngrams",
                  n = 3L) %>%
    mutate(ngram = "trigram")
# Check if any of them are NA
mean(complete.cases(trigrams))
#' `r round((1-mean(complete.cases(trigrams)))*100, 2)`% of the data has
#' missing values since the sentences ended before a 3 word ngram could be
#' formed
#' For now we will simply omit these values, and check back again in case of
#' poor model prediction
trigrams <- na.omit(trigrams)

# Tokenizing to 2-word ngrams(bigrams)
bigrams <- textSentences %>%
    unnest_tokens(output = token,
                  input = sentences,
                  token = "ngrams",
                  n = 2L) %>%
    mutate(ngram = "bigram")
# Check if any of them are NA
mean(complete.cases(bigrams))
#' `r round((1-mean(complete.cases(bigrams)))*100, 2)`% of the data has
#' missing values since the sentences ended before a 3 word ngram could be
#' formed
#' For now we will simply omit these values, and check back again in case of
#' poor model prediction
bigrams <- na.omit(bigrams)

# Tokenizing to 1-word ngrams, which are just words
words <- textSentences %>%
    unnest_tokens(output = token,
                  input = sentences) %>%
    mutate(ngram = "word")
# Check if any of them are NA
mean(complete.cases(words))
#' This contains all the words that we might have omitted in other ngrams, thus
#' we will not be losing a lot of information

# Combining all into 1 data set
text_ngrams <- rbind(words, bigrams, trigrams, quadgrams)

# Writing this out to another file in data folder
write.csv(text_ngrams, "data/text_ngrams.csv", row.names = FALSE)
