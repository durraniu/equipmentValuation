# Load the required libraries
library(rhandsontable)
library(shiny)
library(shinyFeedback)
library(bslib)
library(DT)

# Sidebar -----------------------------------------------------------------

sidebar <- sidebar(
  fileInput('file1', 'Choose File to Load Data'),
  accordion(
    accordion_panel(
      title = "File Handling",
      open = TRUE,
      uiOutput('master_button'),
      fileInput('file1', 'Choose File to Load Data'),
      downloadButton("downloadData", "Save Data")
    ),
    accordion_panel(
      title = "Adding New Units and Models",
      open = FALSE,
      actionButton("new_catagorie", HTML("Add a New <br>Model Catagorie")),
      actionButton("new_model", HTML("Add a New <br>Model History")),
      actionButton("new_unit", "Add a New Unit"),
    ),
    accordion_panel(
      title = "Equipment Details",
      selectInput("unites", "Unite Number", choices = NULL),    # Dropdown for units
      textInput("description", "Description"),
      textInput("model", "Model"),
      numericInput("year", "Year", value = 0),
      numericInput("hours", "Hours", value = 0)
    ),
    accordion_panel(
      title = "Valuation Type & Equipment Condition",
      selectInput("valuationType", "Valuation Type",
                  choices = c("Auction", "Retail"), 
                  multiple = TRUE),
      selectInput("condition", "Equipment Condition",
                  choices = conditions_Defaults),
      actionButton("save_details", HTML("Update and Save <br>Equipment Details"))
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
    min_height = "200px",
    card_header(
      tags$h2("Price Comparison")
    ),
    layout_column_wrap(
      width = 1/3,
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
