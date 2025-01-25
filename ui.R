# Load the required libraries
library(rhandsontable)
library(shiny)
library(bslib)


# Sidebar -----------------------------------------------------------------

sidebar <- sidebar(
  fileInput('file1', 'Choose File to Load Data'),
  accordion(
    accordion_panel(
      title = "Equipment Details",
      numericInput("lot", "Lot or Unit Number", value = 0),
      textInput("description", "Description"),
      textInput("model", "Model"),
      numericInput("year", "Year", value = 0),
      numericInput("hours", "Hours", value = 0)
    ),
    accordion_panel(
      title = "Valuation Type & Equipment Condition",
      selectInput("valuationType", "Valuation Type", 
                  choices = c("Auction", "Retail")),
      selectInput("condition", "Equipment Condition", 
                  choices = conditions_Defaults)
    )
  )
)



# Summary -----------------------------------------------------------------

summary_panel <- nav_panel(
  title = "Summary",
  "SUMMARY CONTENT"
)




# Details -----------------------------------------------------------------

details_panel <- nav_panel(
  title = "Details",
  div(
    style = "overflow-x: auto; white-space: nowrap;",
    rHandsontableOutput("HistTable", width = "100%")
  )
)



# Main UI -----------------------------------------------------------------

page_navbar(
  title = "Equipment Valuation Tool",
  selected = "Summary",
  sidebar = sidebar,
  summary_panel,
  details_panel
)
