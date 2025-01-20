#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Equipment Valuation"),

    # ---- 1) FILE INPUT & DOWNLOAD BUTTONS ----
    fluidRow(
      column(4, fileInput('file1', 'Choose File to Load Data')),
      column(4, downloadButton("downloadData", "Save Data"))
    ),
    
    # ---- 2) URL INPUT ----
    fluidRow(
      column(8, textInput("url", "URL"))
      ),
    
    # ---- 3) MACHINE / EQUIPMENT DETAILS ----
    fluidRow(
      column(2, numericInput("lot", "Lot or Unite Number", value = 0)),
      column(3, textInput("description", "Description")),
      column(3, textInput("model", "Model")),
      column(2, numericInput("year", "Year", value = 0)),
      column(2, numericInput("hours", "Hours", value = 0))
    ),
    
    # ---- 4) VALUATION TYPE & EQUIPMENT CONDITION ----
    fluidRow(
      column(2, selectInput("valuationType", "Valuation Type", choices = c("Auction", "Retail"))),
      column(2, selectInput("condition", "Equipment Condition", choices = conditions_Defaults))
    ),
    
    # ---- 5) ANALYSIS SECTION ----
    fluidRow(
      tags$h1("Anyalsis"),
      fluidRow(
        column(1, ""),
        
        # Checkbox group to select which valuation type(s) should be used in the analysis
        column(2, checkboxGroupInput("valuationType_box", 
                                     "Valuation Type to use in Anyalsis:",
                                     c("Auction" = "Auction", "Retail" = "Retail"),
                                     selected = "Auction")),
        
        # Dynamically generated checkbox(es) or other UI (defined in server) for extended analysis options
        column(2, uiOutput("dynamicCheckbox"))
        ),
      
      # Additional UI output for displaying plots, tables, or other analysis (defined in the server)
      uiOutput('ex1')
    ),
    
    # ---- 6) DATA TABLE DISPLAY ----
    fluidRow(
      tags$h1("Data Table"),
      rHandsontableOutput("HistTable")
    )
)
