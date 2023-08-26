# Load required library
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyhelper)
library(dplyr)

dashboardPage(

    # Adding a skin
    skin = "green",

    # Adding the app title
    dashboardHeader(title = "Text-Ease"),

    # Disable the side bar
    dashboardSidebar(disable = TRUE),

    # Defining the body of the app
    dashboardBody(

        # Adding a custom tag for the app name
        tags$head(
            tags$style(
                HTML('.main-header .logo {
                     font-family: "Helvetica Neue", "Comic Sans MS", Times, serif;
                     font-weight: bold;
                     font-size: 30px;
                     }'))),

        # First row
        fluidRow(align = "center",

            # Output
            box(
                background = "navy",
                width = 12,
                uiOutput("outputBox", fill = TRUE)
            ) # Closed output box

        ), # Closed first row

        # Define second row for input
        fluidRow(

            # Input box
            box(
                background = "black",
                width = 12,
                textInput(
                    "input",
                    label = "",
                    value = "",
                    placeholder = "Enter text here"
                ) %>% # Closed textInput
                    helper(type = "markdown", colour = "red",
                           content = "info"), # Added a help file
            ) # Closed input box

        ), # Closed 2nd row

        # Define third row for appendix
        fluidRow(

            # Input box
            box(
                width = 12,
                title = markdown("**APPENDIX:**"),
                markdown("*<span style=color:red;>Kindy click the red
                         question mark if you have any
                         questions on how to use the app</span>*"),
                markdown("**Slide-Deck:**"),
                markdown("More information on the app can be found
                         [here](https://rpubs.com/akashmer/text_ease_slide_deck)"),
                markdown("**Github Repository:**"),
                markdown("Code for this app and model development can be
                         found [here](https://github.com/AkashMer/text_ease)"),
                markdown("**License Information:**"),
                markdown("Code behind the model and app is licensed under
                         [MIT License](https://github.com/AkashMer/text_ease/blob/main/LICENSE.md)")

            ) # Closed input box

        ) # Closed 3nd row

    ) # Closed dashboard body

) # Closed dashboard page
