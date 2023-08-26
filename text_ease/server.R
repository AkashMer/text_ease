# Load the data from the data folder
load("data/model.RData")

# Load the required library
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyhelper)
library(tokenizers)
library(dplyr)
library(stringr)

# Defining the server logic which will suggest the next 3 words
function(input, output, session) {

    # Observing the question mark helpers
    observe_helpers(withMathJax = TRUE)

    # Observe for click event on the radio buttons
    observeEvent(input$output, {

        new_input <- paste(input$input, input$output, collapse = " ")
        updateTextInput(
            session = session,
            "input",
            value = new_input
        ) # Closed update Text Input
    }) # Closed obser event

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

    # Render and display the predictions
    output$outputBox <- renderUI(

        radioGroupButtons(
            "output",
            choices = prediction(),
            selected = character(0),
            size = "lg",
            individual = TRUE,
            status = "success"
        ) # Closed radio group buttons

    ) # Closed render UI

    # Update the radio button
    observeEvent(input$input, {
        updateRadioGroupButtons(
            session = session,
            "output",
            choices = prediction(),
            selected = character(0),
            size = "lg",
            status = "success"
        ) # Closed updateRadiogroupbutton
    }) # Closed observe

} # Closed serve function
