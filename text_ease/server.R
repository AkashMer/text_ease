# Load the data from the data folder
load("data/model.RData")

# Load the required library
library(shiny)
library(shinydashboard)
library(tokenizers)
library(dplyr)
library(stringr)

# Defining the server logic which will suggest the next 3 words
function(input, output, session) {

    # Get the predictions
    prediction <- reactive({

        # Tokenize into sentences
        inp <- tokenize_regex(input$input, pattern = "\\.+|\\?+|\\!+",
                              simplify = TRUE)

        # Add bos1, bos2, and bos3
        inp <- paste0("bos1 bos2 bos3 ", inp[length(inp)])

        # Divide into words
        inp <- tokenize_ngrams(inp, n = 1L, simplify = TRUE)

        # Consider only the last 3 words
        inp <- inp[(length(inp) - 2):length(inp)]

        # Convert to one hot encodes from the vocabulary
        inp <- vocab$vocabulary[match(inp, vocab$token)]

        # Replace any NAs with unk token index
        unk_vocab <- vocab$vocabulary[match("<unk>", vocab$token)]
        inp[which(is.na(inp))] <- unk_vocab

        # Keep only matching n-grams from the corpus
        pred1 <- top3_quadgram %>%
            filter(word1 == inp[1], word2 == inp[2], word3 == inp[3]) %>%
            select(output) %>%
            unlist()

        # Back off to trigrams
        pred2 <- top3_trigram %>%
            filter(word2 == inp[2], word3 == inp[3]) %>%
            select(output) %>%
            unlist()

        # Back off to bigrams
        pred3 <- top3_bigram %>%
            filter(word3 == inp[3]) %>%
            select(output) %>%
            unlist()

        # Back off to unigrams with word1, word2 context
        pred4 <- top3_unigram12 %>%
            filter(word1 == inp[1], word2 == inp[2]) %>%
            select(output) %>%
            unlist()

        # Back off to unigrams with word2 context
        pred5 <- top3_unigram2 %>%
            filter(word2 == inp[2]) %>%
            select(output) %>%
            unlist()

        # Back off to unigrams with word1 context
        pred6 <- top3_unigram1 %>%
            filter(word1 == inp[1]) %>%
            select(output) %>%
            unlist()

        # Back off to unigrams with no context
        pred7 <- top3_unigram$output

        # Combine all
        top3 <- unique(c(pred1, pred2, pred3, pred4, pred5, pred6, pred7))
        # Select the top3
        top3 <- top3[1:3]

        # Change back to words
        top3 <- vocab$token[match(top3, vocab$vocabulary)]

        # Change to uppercase in case of it is the first letter of a sentence
        if(inp[1] == 23809L) top3 <- str_to_title(top3)

        # Return the top3
        return(top3)

    }) # Closed prediction reactive

    # Display the result into the 3 output boxes
    output$prediction1 <- renderValueBox({

        valueBox(
            value = prediction()[1],
            subtitle = "",
            color = "light-blue",
            icon = icon(NULL)
        ) # Closed valueBox
    }) # Closed renderValueBox

    output$prediction2 <- renderValueBox({

        valueBox(
            value = prediction()[2],
            subtitle = "",
            color = "light-blue",
            icon = icon(NULL)
        ) # Closed valueBox
    }) # Closed renderValueBox

    output$prediction3 <- renderValueBox({

        valueBox(
            value = prediction()[3],
            subtitle = "",
            color = "light-blue",
            icon = icon(NULL)
        ) # Closed valueBox
    }) # Closed renderValueBox

} # Closed serve function
