---
title: "Word Prediction Milestone Report"
author: "Akash Mer"
date: "2023-08-08"
output: 
  bookdown::html_document2:
        number_sections: FALSE
        keep_md: true
        code_folding: hide
        theme: readable
        highlight: espresso
        toc: yes
        toc_float:
            collapsed: true
bibliography: https://api.citedrive.com/bib/83239535-51df-46be-aa69-24f67a76fc51/references.bib?x=eyJpZCI6ICI4MzIzOTUzNS01MWRmLTQ2YmUtYWE2OS0yNGY2N2E3NmZjNTEiLCAidXNlciI6ICI1MTc1IiwgInNpZ25hdHVyZSI6ICJkN2YzNWNjZDFmNjUyYzFjODRiOWQ5MzM2Y2FmZDQ0ZTA3ZjcyMDk0NTJmZDcwYTRiM2Q5OTg0NDViNTM2OTIzIn0=/bibliography.bib
csl: american-medical-association.csl
link-citations: yes
knit: (function(input, ...){
    rmarkdown::render(input,
        output_dir = "C:/Users/akash/Documents/projects/text_ease/results",
        output_file = file.path("./milestoneReport"))
    })
---

<style type="text/css">
h1 {
    color: darkslateblue
}

h2,h3,h4,h5,h6 {
    color: steelblue
}
</style>



# **INTRODUCTION**
  
Milestone Report for the following project,
  
**Suggest the user 3 words that would follow the words already entered by the user**
  
#### **Description of the model type that will be used to generate the predictions**
  
* To achieve the goal, the text will need to be **tokenized**.
* In tokenization, we take an input (a string) and a token type (a meaningful unit of text, such as a word) and split the input into pieces (tokens) that correspond to the type[@Hvitfeldt_2021;@Manning_2008].
* There are various token types available - *character, word, n-grams, sentences, lines, paragraphs*.
* Since we will use **N-gram modeling** to predict, we will eventually divide the text data into n-grams
  
N-grams are word combinations where `N` represents the number of words used to create the combination. Hence, for eg. `The weather report suggested heavy rainfall`
  
* Unigrams = one word (`the`|`weather`|`report`|`suggested`|`heavy`|`rainfall`)
* Bigrams = two words (`the weather`|`weather report`|`report suggested`|`...`)
* Trigrams = three words (`the weather report`|`weather report suggested`|`...`)
* Quadgrams = four words (`the weather report suggested`|`weather report suggested heavy`|`...`)
* and so on ...
  
<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

This report aims to do the following,
  
* Outline steps taken to prepare the raw data to build a prediction model
* Conduct and visualize the tidy data by using plots
* Refine our data processing pipeline based on the exploratory analysis
* Present a general plan for building a prediction model
  
</div>
  
# **DATA PROCESSING**
  
Data used for this project is downloaded from [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)[@Data_Source].  
  
Things to note about our raw data,
  
1. Raw data contains text in 4 languages - **German, English, Finnish, and Russian**
2. We will only **focus on English** for now
3. English text data has been collected from 3 different sources - **blogs, news,**
**and twitter**
4. Each source has a LOT of data (size of English text data : approx. 556 MB)
5. Each source has different amount of data
6. Blogs text data, despite having lower number of sources, has one of the
longest texts out of all the 3
  
### **Decision 1**
  
*Option A* : We combine all the data into 1 data set  
Advantages - simple, increases sample size  
Disadvantages - Will not be able to capture the different language morphology
used among these different sources  
  
*Option B* : Keep them separate and then conduct different EDA and different
models for each  
Advantages - Preserves the language morphology from each source  
Disadvantages - Smaller source of information, more computationally heavy
and complex  
  
For now we will go for **Option A**, since it is much simpler, and may help us capture unique combinations of words which might be more useful
  
### **Decision 2**

**Do we use all the data from each source?**  
Each line in the files represents 50%-truncated data from one instance of a
source type. So it is natural that blog posts and news have longer texts as
compared to twitter.  
Let's just get an idea of the number of instances(ie. lines) for each source type  

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">


```r
if(system.file(package = "R.utils") == "") install.packages("R.utils")
library(R.utils)
twitterN <- countLines(unz("./data/textData.zip",
                           "final/en_US/en_US.twitter.txt", open = "rb"))
blogsN <- countLines(unz("./data/textData.zip",
                         "final/en_US/en_US.blogs.txt", open = "rb"))
newsN <- countLines(unz("./data/textData.zip",
                        "final/en_US/en_US.news.txt", open = "rb"))

cat("Number of instances(ie. lines) from each source type:\n
    1. Twitter - ", twitterN/10^6, " million\n
    2. Blogs - ", blogsN/10^6, " million\n
    3. News - ", newsN/10^6, "million")
```

```
Number of instances(ie. lines) from each source type:

    1. Twitter -  2.360148  million

    2. Blogs -  0.899288  million

    3. News -  1.010242 million
```

</div>

That is a lot of instances.  
A **random sample of 50000** instances which should provide enough information for drawing conclusions on our population data. (As per Law of Large Numbers which states that the mean of the sample gets closer and closer to the mean of the population as sample size increases, which can be interpreted in our case as the sample will contain words at approximately the same mean number of occurrences as our entire data, and thus we should be able to draw conclusions about our entire data from this sample)
  

```r
if(system.file(package = "readr") == "") install.packages("readr")
library(readr)
set.seed(65198)
twitterTextData <- read_lines(unz("data/textData.zip",
                                           "final/en_US/en_US.twitter.txt"),
                              n_max = -1)[sample(1:twitterN, size = 50000)]
set.seed(65198)
blogsTextData <- read_lines(unz("data/textData.zip",
                                  "final/en_US/en_US.blogs.txt"),
                              n_max = -1)[sample(1:blogsN, size = 50000)]
set.seed(65198)
newsTextData <- read_lines(unz("data/textData.zip",
                                 "final/en_US/en_US.news.txt"),
                             n_max = -1)[sample(1:newsN, size = 50000)]
```

### **Decision 3**
  
**What proportion of each source type should be used?**
  
All 3 source type texts were **broken down into sentences**, then **equal number of sentences were selected**. This will prevent any effect of the amount of data from each source type, and at the same time, this will remove any bias while we try to determine if the language morphology among all 3 sources is different.  
This is important, since we are trying to predict the next word, based on language syntax learned from this data set, and our expectation is that all 3 source types should have different language morphology, hence it should impact our predictions.
  
<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

```r
if(system.file(package = "dplyr") == "") install.packages("dplyr")
if(system.file(package = "tidytext") == "") install.packages("tidytext")
library(dplyr)
library(tidytext)

# For now, we will preserve the uppercase characters
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

# Displaying number of sentences by source type
cat("Minimum and Maximum number of characters in each sample data(in million characters): \n
    1. Twitter - ", summary(nchar(twitterSentences))[c(1,6)]/10^6, "\n
    2. Blogs - ", summary(nchar(blogsSentences))[c(1,6)]/10^6, "\n
    3. News - ", summary(nchar(newsSentences))[c(1,6)]/10^6)
```

```
Minimum and Maximum number of characters in each sample data(in million characters): 

    1. Twitter -  0.88336 3.750952 

    2. Blogs -  1.197509 12.08569 

    3. News -  0.804137 10.50334
```
</div>
All sources contain different number of characters as well. Thus further sampling of these sentences was done to enforce approximately equal contribution of each source for our data. The ratio of sampling was adjusted to achieve approximately equal representation of each source type and was done in the following manner,
  
<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">
  
* **Twitter text data - 80000 sentences** (contains shorter sentences)
* **Blogs text data - 60000 sentences** (contains longer sentences)
* **News text data - 60000 sentences**
  
</div>
  
These sentences were randomly sampled with `seed set at 75168`

```r
set.seed(75168)
twitterSentences <- twitterSentences[sample(1:nrow(twitterSentences), 80000),]
set.seed(75168)
blogsSentences <- blogsSentences[sample(1:nrow(blogsSentences), 60000),]
set.seed(75168)
newsSentences <- newsSentences[sample(1:nrow(newsSentences), 60000),]
```
  
### **Combine data from all 3 sources**
  
Data was combined from all 3 sources and the information on source type was preserved in the sample set

```r
textSentences <- rbind(twitterSentences, blogsSentences, newsSentences)
textSentences
```

```
# A tibble: 200,000 × 2
   source  sentences                                                  
   <chr>   <chr>                                                      
 1 twitter Follow up, did you ever find that baby deer?               
 2 twitter Easy A+. c:                                                
 3 twitter haha                                                       
 4 twitter If you could write a book, it would be about ____________ ?
 5 twitter Les Créations de NARISAWA in Japan - reservations made     
 6 twitter Patrick’s Day!                                             
 7 twitter The 48 wins in Darlington.                                 
 8 twitter My apologize for the last post.                            
 9 twitter followed by much destruction of dining furniture.          
10 twitter Saved the American auto industry                           
# ℹ 199,990 more rows
```

### **Decision 4**
  
**How many minimum and maximum words should be entered by the user to trigger predictions?**
  
We will limit ourselves to 0-3 words, anything more than this, only the last three words will be taken into consideration.  
*Impact of this decision* - We will unnest our sentences further down to include 4 word tokens(quadgrams), 3 word tokens(trigrams), 2 word tokens(bigrams) and 1 word tokens(words).  
Following transformation will automatically lowercase everything for us. For now we will be converting everything to lowercase, we will revisit this decision when we are trying to improve our prediction model.
  

```r
# Tokenizing to 4-word ngrams/quadgrams
quadgrams <- textSentences %>%
    unnest_tokens(output = token,
                  input = sentences,
                  token = "ngrams",
                  n = 4L) %>%
    mutate(ngram = "quadgram")

# Tokenizing to 3-word ngrams(trigrams)
trigrams <- textSentences %>%
    unnest_tokens(output = token,
                  input = sentences,
                  token = "ngrams",
                  n = 3L) %>%
    mutate(ngram = "trigram")

# Tokenizing to 2-word ngrams(bigrams)
bigrams <- textSentences %>%
    unnest_tokens(output = token,
                  input = sentences,
                  token = "ngrams",
                  n = 2L) %>%
    mutate(ngram = "bigram")

# Tokenizing to 1-word ngrams, which are just words
words <- textSentences %>%
    unnest_tokens(output = token,
                  input = sentences) %>%
    mutate(ngram = "word")
```
  
These tokens contained some missing values, since there is a chance that the sentence would end before an n-gram could be formed

```r
cat("Missing values for each type of n-gram:\n
    1. Words - ", sum(is.na(words$token)), "out of ", nrow(words), "\n
    2. Bigrams - ", sum(is.na(bigrams$token)), "out of ", nrow(bigrams), "\n
    3. Trigrams - ", sum(is.na(trigrams$token)), "out of ", nrow(trigrams), "\n
    4. Quadgrams - ", sum(is.na(quadgrams$token)), "out of ", nrow(quadgrams))
```

```
Missing values for each type of n-gram:

    1. Words -  0 out of  2620178 

    2. Bigrams -  9317 out of  2430431 

    3. Trigrams -  18623 out of  2249054 

    4. Quadgrams -  28864 out of  2077918
```
  
Since these missing values are so small compared to the amount of our data and they have a known pattern, they will be removed.


```r
bigrams <- na.omit(bigrams)
trigrams <- na.omit(trigrams)
quadgrams <- na.omit(quadgrams)
```
  
### **Combining all n-grams into one data frame**
  
All types of n-grams are combined into 1 data set.   
The source and n-gram type were given their own columns and converted to a factor which will be useful during exploratory data analysis.

```r
text_ngrams <- rbind(words, bigrams, trigrams, quadgrams)
text_ngrams <- text_ngrams %>%
    mutate(source = factor(source),
           ngram = factor(ngram, levels = c("word", "bigram", "trigram",
                                            "quadgram")))
text_ngrams
```

```
# A tibble: 9,320,777 × 3
   source  token  ngram
   <fct>   <chr>  <fct>
 1 twitter follow word 
 2 twitter up     word 
 3 twitter did    word 
 4 twitter you    word 
 5 twitter ever   word 
 6 twitter find   word 
 7 twitter that   word 
 8 twitter baby   word 
 9 twitter deer   word 
10 twitter easy   word 
# ℹ 9,320,767 more rows
```

# **EXPLORATORY DATA ANALYSIS**
  
The number of lines in each text files and number of characters in our sample was already shown during data processing.  
  
### **Data Splitting**
  
To conduct a more unbiased analysis during the later stages of model building, the above mentioned data set was divided into train (80%) and test (20%) sets  
This splitting was done to ensure both sets have the same proportion of data from each source as well as same proportion of each n-gram type as the un-split data set.

```r
if(system.file(package = "tidymodels") == "") install.packages("tidymodels")
library(tidymodels)
tidymodels_prefer()

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
```

All our exploratory analysis will be done on the `train` split of the data set
  
### **Number of unique N-grams**
  

```r
if(system.file(package = "ggplot2") == "") install.packages("ggplot2")
library(ggplot2)

# Calculate count of each unique token
train_count <- train %>%
    count(ngram, token, sort = TRUE)

# Fig1 on relationship between type of n-gram and number of unique tokens
train_count %>%
    group_by(ngram) %>%
    summarise(length = length(token), max = max(n)) %>%
    ggplot(aes(ngram, length, size = max, color = ngram)) +
    geom_point() +
    scale_color_brewer(palette = "Dark2", guide = "none") +
    scale_size_continuous(range = c(10,30)) +
    labs(x = NULL, y = "Number of unique n-grams",
         size = "Count of\nmost frequent\nn-gram") +
    theme_bw(base_size = 70)
```

<div class="figure">
<img src="C:/Users/akash/Documents/projects/text_ease/results/milestoneReport_files/figure-html/fig1-1.png" alt="Unique n-grams by n-gram order"  />
<p class="caption">(\#fig:fig1)Unique n-grams by n-gram order</p>
</div>

**Fig. \@ref(fig:fig1)** shows the relationship between the number of unique tokens by N-gram type and number of occurrences of the most frequent token(counts) represented by the size of the point.  

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

**Conclusions** : Higher order n-grams have large number of unique n-grams but their count keeps getting smaller and smaller by a large magnitude.  
Thus, **Larger order n-grams might give smaller and smaller values of counts and thus the probability(calculated from counts) of the next word might also keep approaching zero, resulting into unnecessarily complex and time-consuming computations**

</div>
  
### **Distribution of counts of each N-gram type**
  

```r
library(ggplot2)

train_count %>%
    ggplot(aes(n, fill = ngram)) +
    geom_histogram(bins = 30) +
    facet_wrap("ngram", nrow = 2) +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    labs(x = "Counts of each unique ngram",
         y = "Count") +
    theme_bw(base_size = 70) +
    theme(plot.caption = element_text(face = "italic", hjust = 0))
```

<div class="figure">
<img src="C:/Users/akash/Documents/projects/text_ease/results/milestoneReport_files/figure-html/fig2-1.png" alt="Unique N-gram Count Distribution"  />
<p class="caption">(\#fig:fig2)Unique N-gram Count Distribution</p>
</div>

**Fig. \@ref(fig:fig2)** shows the distribution of counts of each unique n-gram by n-gram type.  

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

**Conclusion:** There are a lot of n-grams which occur only once or close to around that and these increased as we go to higher order n-grams.  
This once again **confirms what we saw in Fig \@ref(fig:fig1)**

</div>
  
Let's get a closer look at these, to understand their nature by finding out the most frequent(10) and least frequent(10) tokens for each type of n-gram
  
### **Most and Least Frequent N-grams**
  

```r
if(system.file(package = "cowplot") == "") install.packages("cowplot")
library(ggplot2)
library(cowplot)

train_count_wide <- train_count %>%
    pivot_wider(names_from = ngram, values_from = n)

# Getting custom colors
library(RColorBrewer)
cols <- brewer.pal(4, "Dark2")

# Top 10 and Bottom 10 unigrams
plotA <- train_count_wide %>%
    select(token, word) %>%
    na.omit() %>%
    arrange(desc(word)) %>%
    slice(1:10,(length(token)-9):length(token)) %>%
    mutate(token = reorder(token, word)) %>%
    ggplot(aes(word, token)) +
    geom_col(fill = cols[1]) +
    labs(title = "Words", y = NULL, "Count") +
    theme_bw(base_size = 70)

# Top 10 and Bottom 10 bigrams
plotB <- train_count_wide %>%
    select(token, bigram) %>%
    na.omit() %>%
    arrange(desc(bigram)) %>%
    slice(1:10,(length(token)-9):length(token)) %>%
    mutate(token = reorder(token, bigram)) %>%
    ggplot(aes(bigram, token)) +
    geom_col(fill = cols[2]) +
    labs(title = "Bigrams", y = NULL, x = "Count") +
    theme_bw(base_size = 70)

# Top 10 and Bottom 10 trigrams
plotC <- train_count_wide %>%
    select(token, trigram) %>%
    na.omit() %>%
    arrange(desc(trigram)) %>%
    slice(1:10,(length(token)-9):length(token)) %>%
    mutate(token = reorder(token, trigram)) %>%
    ggplot(aes(trigram, token)) +
    geom_col(fill = cols[3]) +
    labs(title = "Trigrams", y = NULL, x = "Count") +
    theme_bw(base_size = 70)

# Top 10 and Bottom 10 quadgrams
plotD <- train_count_wide %>%
    select(token, quadgram) %>%
    na.omit() %>%
    arrange(desc(quadgram)) %>%
    slice(1:10,(length(token)-9):length(token)) %>%
    mutate(token = reorder(token, quadgram)) %>%
    ggplot(aes(quadgram, token)) +
    geom_col(fill = cols[4]) +
    labs(title = "Quadgrams", y = NULL, x = "Count") +
    theme_bw(base_size = 70)


# Top 10 and Bottom 10 n-gram plot
plot_grid(plotA, plotB, plotC, plotD, nrow = 2, labels = c("A", "B", "C", "D"),
          label_size = 40)
```

<div class="figure">
<img src="C:/Users/akash/Documents/projects/text_ease/results/milestoneReport_files/figure-html/fig3-1.png" alt="10 Most and 10 Least Frequent N-grams"  />
<p class="caption">(\#fig:fig3)10 Most and 10 Least Frequent N-grams</p>
</div>
  
**Fig. \@ref(fig:fig3)** shows 10 Most and Least Frequent tokens in each n-gram type  

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">
  
**Conclusion:**
  
* Top n-grams contain most commonly used words in the English language which are known as `stop words`
* Least frequent n-grams for our data set includes foreign language words
* The difference between these is huge but keeps decreasing for higher order n-grams
* The top 3 unigrams are `the, to and and`. `to and and` are usually not the words used to start sentences, thus we will have to introduce some kind of place holder to represent the beginning of a sentence to ensure the model predicts even when no text is entered.

</div>
  
**Decisions based on the above Figures:**
  
1. **Introduce a place holder at the start of each sentence** - `sss`
2. **Keep the stop words for now** since our aim is to build a text prediction model and these words will be very useful
3. **Do not remove the foreign language words for now since they are in the form of embedded text**. But the following should be kept in mind, these words are more individualized and thus come from a different population of text with it's own probabilities. We can revisit this decision during fine tuning of our models.
  
### **Impact of Source of the text**
  
Time to revisit **Decision 1** which we made where we combined the text from all source types.   
Let's check if different sources use different proportion of words[@Silge_2017]. Only the 1-word n-grams are used since our aim is to check if different sources use different words and in turn different language morphology.  
The stop words will be removed since these will be same across the sources and thus may drive the correlation test we will perform.


```r
if(system.file(package = "scales") == "") install.packages("scales")
library(tidytext)

# Get frequency of each word by source
frequency <- train %>%
    filter(ngram == "word") %>%
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
    geom_abline(color = "gray40", lty = 2, lwd = 3) +
    geom_jitter(alpha = 0.1, size = 15, width = 0.3, height = 0.3) +
    geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5, size = 25) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0,0.001),
                         low = "darkslategray4", high = "gray75") +
    facet_wrap(~source, ncol = 2,
               labeller = labeller(source = c(blogs = "Blogs", news = "News"))) +
    labs(y = "Twitter", x = NULL) +
    theme_bw(base_size = 90) +
    theme(legend.position = "none")
```

<div class="figure">
<img src="C:/Users/akash/Documents/projects/text_ease/results/milestoneReport_files/figure-html/fig4-1.png" alt="Correlation of unique words used in all 3 sources with numbers and punctuation"  />
<p class="caption">(\#fig:fig4)Correlation of unique words used in all 3 sources with numbers and punctuation</p>
</div>
  
**Fig. \@ref(fig:fig4)** shows the comparison of the frequency(count of each unique words/total number of words from the source) of words from each source. More close the points are to the `gray dashed line` passing through the center, the more likely it is for the word to appear in both sources
  
**Things to note:**
  
* Blogs and Twitter use similar group of words, where as, news is different from twitter. Let's confirm this using a correlation test

```r
# Correlation between twitter and blogs
corr1 <- cor.test(data = frequency[frequency$source == "blogs",],
         ~ proportion + twitter)

# Correlation between twitter and news
corr2 <- cor.test(data = frequency[frequency$source == "news",],
         ~ proportion + twitter)

cat("The correlation between words used on twitter and \n
    1. words used on Blogs = ", round(corr1$estimate, 3),
    "(", round(corr1$conf.int, 3), ")\n
    2. words used on News = ", round(corr2$estimate, 3),
    "(", round(corr2$conf.int, 3), ")\n
    which implies that words from twitter and blogs are 0.3 times more similar 
    to each other as compared to words from twitter and news")
```

```
The correlation between words used on twitter and 

    1. words used on Blogs =  0.713 ( 0.705 0.72 )

    2. words used on News =  0.555 ( 0.545 0.565 )

    which implies that words from twitter and blogs are 0.3 times more similar 
    to each other as compared to words from twitter and news
```
* Numbers and punctuation seem to be included in our tokens.
  
To check the impact of numbers and punctuation, we will remove them and repeat the correlation tests done above.
  

```r
if(system.file(package = "stringr") == "") install.packages("stringr")
# Get frequency of each word by source with number and punctuations removed
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
    geom_abline(color = "gray40", lty = 2, lwd = 3) +
    geom_jitter(alpha = 0.1, size = 15, width = 0.3, height = 0.3) +
    geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5, size = 25) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0,0.001),
                         low = "darkslategray4", high = "gray75") +
    facet_wrap(~source, ncol = 2,
               labeller = labeller(source = c(blogs = "Blogs", news = "News"))) +
    labs(y = "Twitter", x = NULL) +
    theme_bw(base_size = 90) +
    theme(legend.position = "none")
```

<div class="figure">
<img src="C:/Users/akash/Documents/projects/text_ease/results/milestoneReport_files/figure-html/fig5-1.png" alt="Correlation of unique words used in all 3 sources without numbers and punctuation"  />
<p class="caption">(\#fig:fig5)Correlation of unique words used in all 3 sources without numbers and punctuation</p>
</div>


```r
# Correlation between twitter and blogs
corr1 <- cor.test(data = frequency[frequency$source == "blogs",],
         ~ proportion + twitter)

# Correlation between twitter and news
corr2 <- cor.test(data = frequency[frequency$source == "news",],
         ~ proportion + twitter)

cat("The correlation between words used on twitter and \n
    1. words used on Blogs = ", round(corr1$estimate, 3),
    "(", round(corr1$conf.int, 3), ")\n
    2. words used on News = ", round(corr2$estimate, 3),
    "(", round(corr2$conf.int, 3), ")\n
    which implies that once numbers and pucntation are removed, words from all 
    3 sources are almost the same, with only slight difference(0.05 times)")
```

```
The correlation between words used on twitter and 

    1. words used on Blogs =  0.909 ( 0.906 0.911 )

    2. words used on News =  0.868 ( 0.864 0.871 )

    which implies that once numbers and pucntation are removed, words from all 
    3 sources are almost the same, with only slight difference(0.05 times)
```
  
<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

**Conclusions:** *Fig. \@ref(fig:fig4) & \@ref(fig:fig5)* and the associated correlation tests confirm that **numbers and punctuation should be excluded to build a text prediction model which will perform well in all these source types**  
The inclusion of data from all sources, instead of just 1 or 2 is favored since we will be able to build a more generalized model instead of overfitting to one type of source.

</div>
  
# **FINAL DATA PROCESSING PIPELINE**
  
1. **Randomly sample 50000 instances(lines) from each of the 3 sources provided in the data** - Randomness of the sampling eliminates any selection bias.
2. **Divide the sample into individual sentences** - Helps to balance the length of text from each source since twitter sources will have shorter texts as compared to blogs and news.
3. **Randomly sample sentences** - Randomness of the sampling again eliminates any selection bias. Different amount of sentences will be taken from each source, since twitter sentences are usually shorter due to the limited input size of 240 characters, and thus this adjustment will assure equal representation of each source. (Twitter - 80k sentences, Blogs - 60k sentences, and News - 60k sentences)
4. **Remove the numbers** - It is difficult to predict numbers that will be entered by the user, since the purpose of the numbers could be anything, for eg. time of the day, some sort of measurement, score on a test, value of pi, and thus can take on any number from `-Inf` to `+Inf`. Although this may prevent the model to predict let's say, am and pm, after the user enters let's meet at 5 ... . We will come back to this if the prediction accuracy of our model is low.
5. **Remove punctuation** - Since punctuation represents the grammar of the language and is affected less by the accompanying 3-4 words, but by the entire sentence. This would be out of the scope of our prediction model. Both steps 4 and 5, will also reduce the difference contributed by the inclusion of different sources, thereby making our model more parsimonious.
6. **Introduce a place holder(`sss`) to indicate start sentences** - to preserve natural syntax of the language which will help the model predict appropriate words accordingly.
7. **Divide the sentences into bi-grams only** - to help reduce computational time. For higher order inputs we will use the intuition behind n-gram modeling - higher order n-grams have close to zero probabilities.
8. **Check for any missing values** - and remove them since they will only contribute to a small amount in our large data set
9. **Keep the stop words** - Since our purpose is to predict while the user is typing, inclusion of stop words would be very helpful and may even be pleasant for the user since they wont have to focus on these words while typing. Although the downside of keeping stop words is it is likely for our model to always predict these over others, thus to avoid this scenario, **we will not use count as the statistic, but the probability calculated by the following formula**
<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">
$$
Prob(word2|word1) = {Count(word2|word1)\over Count(word1)} (\#eq:model-equation)
$$
</div>
which can be read as the probability of `word2` following `word1` is equal to the number of times `word2` appears after `word1` divided by the number of times `word1` appears.
10. **Keep the embedded foreign words** - since this will require complex transformations
11. **Divide into train(80%) and test(20%) sets** - Helps to check the accuracy of our model after tuning has been done and informs us on issues with overfitting of our model. The data will be divided to make sure the same proportion of all 3 sources is there in each split.
12. **Separate bi-grams into `word1` and `word2`** - helps with calculating the above formula. 
13. **Calculate the probability** of each `word2` by using the above equation \@ref(eq:model-equation).
14. **Apply the transformations 12 and 13 to the test set as well**



## **Visualization of the processed train data set**
  

```r
if(system.file(package = "igraph") == "") install.packages("igraph")
if(system.file(package = "ggraph") == "") install.packages("ggraph")
if(system.file(package = "grid") == "") install.packages("grid")
# Bigram visualization
library(igraph)
bigram_graph <- bigram_counts_train %>%
    filter(num > 500) %>%
    graph_from_data_frame()

# Plotting the igraph
library(ggraph)
set.seed(2023)
a <- grid::arrow(type = "closed", length = unit(.50, "inches"))
ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = num), show.legend = FALSE,
                   arrow = a, end_cap = circle(.20, 'inches')) +
    geom_node_point(color = "lightblue", size = 20) +
    geom_node_text(aes(label = name), vjust = 1.4, hjust = 0.8, size = 25) +
    scale_edge_alpha_continuous(range = c(0.6,1)) +
    labs(caption = "Fig. 6") +
    theme_void() +
    theme(plot.caption = element_text(face = "italic", hjust = 0))
```

<div class="figure">
<img src="C:/Users/akash/Documents/projects/text_ease/results/milestoneReport_files/figure-html/fig6-1.png" alt="Markov chains of bi-grams from train data set"  />
<p class="caption">(\#fig:fig6)Markov chains of bi-grams from train data set</p>
</div>

**Fig. \@ref(fig:fig6)**[@Silge_2017] visualizes the relationship between the first word and the second word of our bi-grams. The point of the arrow indicates the `word2` and the starting of the arrow indicates `word1` and the visibility of the arrows represent the strength of the relationship. Only bi-grams occurring more than 500 times are included in the graph to avoid overcrowding.  
The paths laid out are known as **Markov chains**  
For eg. one can follow along this way,
  
* more -> than (*bottom left*)
* has -> been (*bottom right*)
* sss(sentence start placeholder) -> i, we, it and so on and so forth (*right center*)
  
<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

## **Basic Idea behind the model**
  
The model will utilize these Markov chains laid out in *Fig. \@ref(fig:fig6)* when given `word1` and trace along all the paths leading away from it and sort them by decreasing order of probabilities calculated from the equation \@ref(eq:model-equation).  
Then the model will return the top 3 `word2`.  
Hence, this idea could easily be extended to include higher order n-grams and still maintain a simple structure as shown in *Fig. \@ref(fig:fig6)*

</div>
  
# **TUNING POINTS FOR THE MODEL**
  
In order to increase the accuracy of model, following entry points should be noted,
  
* Inclusion of a larger sample size
* Inclusion of higher order n-grams
* Creating a specific function which will clean up the words even further, like the word `ive` in *Fig. \@ref(fig:fig6)* can be dealt with specifically by converting to `I have`
* Inclusion of certain numbers and punctuation
* Exclusion of embedded foreign text by including text which only appears in some sort of official collection of words like a dictionary. `Stemming` of the text data is one way to do this, where words are included only if they are part of the words included in a data set specifically built for this purpose.
  
Even after tuning for these variables, if the model accuracy remains low, another model type will be chosen, most likely involving some sort of deep learning model.
  
# **APPENDIX**

### **References**
  
<div id="refs"></div>
  
### **R markdown details**
  
Written in **Rmarkdown file in R version 4.3.1 (2023-06-16 ucrt) using RStudio IDE**  
**Packages** used for this report,  
  
* **bookdown** : *Version 0.34.2*
* **R.utils** : *Version 2.12.2*
* **readr** : *Version 2.1.4*
* **dplyr** : *Version 1.1.2*
* **tidytext** : *Version 0.4.1*
* **tidymodels** : *Version 1.1.0*
* **ggplot2** : *Version 3.4.2*  
* **cowplot** : *Version 1.1.1*  
* **scales** : *Version 1.2.1*  
* **stringr** : *Version 1.5.0*  
* **igraph** : *Version 1.5.0.1*  
* **ggraph** : *Version 2.1.0*  
* **grid** : *Version 4.3.1*
  
