#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = bslib::bs_theme(bootswatch = "flatly"),

    # Application title
    "Dataverse synthesizer",
    
    tabPanel(
        "Create data",
        verticalLayout(
            h4("Dataverse page"),
            textInput("url", placeholder = "Dataverse URL", label = "Input the URL to a dataverse dataset page below. You will then be able to download a synthetic version of the dataset.", width = "100%"),
            actionButton("click", "Create data"),
            hr(),
            h4("Data preview:"),
            DT::dataTableOutput("syn_dat"),
            br(),
            downloadButton("dwnload", "Download synthetic dataset as csv"),
            hr()
        )
    ),
    
    tabPanel(
        "Info",
        verticalLayout(
            h4("Information"),
            p("The dataverse synthesizer is a prototype. For more information, contact Erik-Jan van Kesteren (",
              a("erikjanvankesteren.nl", href = "https://erikjanvankesteren.nl"), ").")
        )
    ),
    
    tags$head(tags$style(HTML("
      .tab-content {
        margin-left: auto;
        margin-right: auto;
        display: block;
        max-width: 65rem;
      }
    ")))
))
