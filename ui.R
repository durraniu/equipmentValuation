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
      open = FALSE,
      selectInput("unites", "Unite Number", choices = NULL),    # Dropdown for units
      selectInput("categorie", "Categorie", choices = NULL),     # Dropdown for Categories in our Hist Data
      textInput("description", "Description"),
      #textInput("model", "Model"),
      selectInput("model", "Model", choices = NULL),  # Dropdown for Models in our Hist Data
      numericInput("year", "Year", value = 0),
      numericInput("hours", "Hours", value = 0),
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
        numericInput("valuation", label = NULL, value = NULL),
        actionButton("assign_valuation", "Assign Valuation")
      )
    )
  ),
  div(
    style = "overflow-x: auto; white-space: nowrap;",
    uiOutput("dynamicCheckbox"),      # Dynamic checkbox (server-side)
    accordion(
      accordion_panel(
        title = "History Table",
        tags$div(
          actionButton("save_HistTable", "Save Changes to History Table"),
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
