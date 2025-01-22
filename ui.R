# Load the required libraries
library(rhandsontable)
library(shiny)
library(bslib)

# Create the main navigation bar page
page_navbar(
  title = "Equipment Valuation Tool",  # The title for the entire application

  # Wrap everything in a card that itself contains another navbar
  card(
    page_navbar(

      #---- "Summary" tab panel ----
      nav_panel(
        "Summary",
        "Summary content here"  # Simple text for the "About" tab
      ),

      #---- "Tool" tab panel ----
      nav_panel(
        "Details",

        # Outer card to contain the tool’s UI elements
        card(
          fill = TRUE,               # Allow the card to fill available space
          card_header("Details and Entry"), # Section header label
          full_screen = TRUE,        # Enable fullscreen option

          ##---- Sidebar layout for input widgets ----
          layout_sidebar(
            sidebar = sidebar(
              fileInput('file1', 'Choose File to Load Data'),  # File input for data
              downloadButton("downloadData", "Save Data"),     # Download button to save data

              numericInput("lot", "Unite Number", value = 0),  # Numeric input for "Unite Number"
              textInput("url", "URL"),                         # Text input for a URL
              textInput("description", "Description"),         # Text input for a description
              textInput("model", "Model"),                     # Text input for model name
              numericInput("year", "Year", value = 0),         # Numeric input for year
              numericInput("hours", "Hours", value = 0),       # Numeric input for hours of use

              selectInput(
                "valuationType",
                "Valuation Type",
                choices = c("Auction", "Retail")               # Dropdown to select a valuation type
              ),

              selectInput(
                "condition",
                "Equipment Condition",
                choices = conditions_Defaults                   # Dropdown for equipment condition
              ),

              checkboxGroupInput(
                "valuationType_box",
                "Valuation Type to use in Anyalsis:",
                c("Auction" = "Auction", "Retail" = "Retail"),  # Checkboxes for valuation types
                selected = "Auction"
              )
            ),

            ##---- Card holding the main outputs and plots ----
            card(
              ###---- Dynamic title using a server-side text output ----
              card_header(textOutput("TitleCard")),

              # Wrap columns for results and plots side by side (each takes half width)
              layout_column_wrap(
                width = 1/2,

                ###---- Card for various price considerations ----
                card(
                  card_header("Prices to Consider"),            # Header label for price info
                  value_box(
                    title = "Predictive Price",
                    value = textOutput("predictivePrice")       # Shows the predicted price
                  ),
                  value_box(
                    title = "Comparison Retail Price",
                    value = textOutput("retailPrice")           # Shows a comparison retail price
                  ),
                  value_box(
                    title = "Average Price",
                    value = textOutput("averagePrice")          # Displays the average price
                  )
                ),

                ###---- Card containing all plot outputs ----
                card(
                  card_header("Plots"),                         # Header label for plot section
                  navset_card_tab(
                    nav_panel("$ vs Year", plotlyOutput("plot1")),     # Plotly plot: price vs year
                    nav_panel("$ vs Hours", plotlyOutput("plot2")),    # Plotly plot: price vs hours
                    nav_panel("$ vs Condition", plotlyOutput("plot3")) # Plotly plot: price vs condition
                  )
                )
              ),

              ###---- Card for formulas and method ----
              card(
                card_header("Formulas used"),
                uiOutput('ex1')  # Any dynamic UI generated on the server
              ),

              ###---- Card for displaying the data table and related controls ----
              card(
                card_header("Data Table"),        # Header for data table section
                uiOutput("dynamicCheckbox"),      # Dynamic checkbox (server-side)
                rHandsontableOutput("HistTable")  # Handsontable (editable table) output
              )
            )
          )
        )
      )
    )
  )
)
