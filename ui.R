# Load the required libraries
library(rhandsontable)
library(shiny)
library(shinyFeedback)
library(bslib)
library(DT)

# Sidebar -----------------------------------------------------------------

sidebar <- sidebar(
  accordion(
    accordion_panel(
      title = "File Handling",
      fileInput('file1', 'Choose File to Load Data'),
      
    ),
    accordion_panel(
      title = "Equipment Details",
      selectInput("unites", "Unite Number", choices = NULL),    # Dropdown for units
      textInput("description", "Description"),
      #textInput("model", "Model"),
      selectInput("model", "Model", choices = NULL),  # Dropdown for Models in our Hist Data
      numericInput("year", "Year", value = 0),
      numericInput("hours", "Hours", value = 0),
      selectInput("condition", "Equipment Condition",
                  choices = conditions_Defaults)
    ),
    accordion_panel(
      title = "Valuation Type",
      selectInput("valuationType", "Valuation Type",
                  choices = c("Auction", "Retail"), 
                  multiple = TRUE)
    )
  )
)



# Summary -----------------------------------------------------------------

summary_panel <- nav_panel(
  title = "Summary",
  "SUMMARY CONTENT",
  card(
    DTOutput("dt_summary")
    )
)




# Details -----------------------------------------------------------------

details_panel <- nav_panel(
  title = "Details",
  card(
    min_height = "400px",
    card_header(
      tags$h2("Price Comparison")
    ),
    layout_column_wrap(
      width = 1/2,
      value_box(
        "Predictive Price",
        textOutput("price_pred")
      ),
      value_box(
        "Retail Price",
        textOutput("price_retail")
      ),
      value_box(
        "Average Price",
        textOutput("price_average")
      ),
      value_box(
        "Assigned Valuation",
        numericInput("valuation", "Valuation to use", value = NULL)
      )
    )
  ),
  div(
    style = "overflow-x: auto; white-space: nowrap;",
    uiOutput("dynamicCheckbox"),      # Dynamic checkbox (server-side)
    accordion(
      accordion_panel(
        title = "Table",
        tags$div(
          rHandsontableOutput("HistTable")
        )
      )
    ),
    uiOutput('ex1')
  )
)



# Main UI -----------------------------------------------------------------

page_navbar(
  title = "Equipment Valuation Tool",
  selected = "Summary",
  sidebar = sidebar,
  header = div(
    useBusyIndicators(),
    useShinyFeedback()
  ),
  summary_panel,
  details_panel
)
