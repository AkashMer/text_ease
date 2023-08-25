# Load required library
library(shiny)
library(shinydashboard)

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
                ".main-header .logo{font-weight: bold;
                font-size: 30px;
                }"
            ) # Closed tags style
        ), # Closed tags head

        # First row
        fluidRow(

            # Box 1 for output
            valueBoxOutput("prediction1", width = 4),

            # Box 2 for output
            valueBoxOutput("prediction2", width = 4),

            # Box 3 for output
            valueBoxOutput("prediction3", width = 4)

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
                ) # Closed textInput
            ) # Closed input box

        ) # Closed 2nd row

    ) # Closed dashboard body

) # Closed dashboard page
