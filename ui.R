library(rhandsontable)
library(shiny)
library(bslib)

page_navbar(
  title = "Equipment Valuation Tool",
  nav_panel("About", "About content here"),
  nav_panel("Tool",
            card(
              card(
                card_header("Details and Entry"),
                full_screen = TRUE,
                layout_sidebar(sidebar =  sidebar(
                  fileInput('file1', 'Choose File to Load Data'),
                  downloadButton("downloadData", "Save Data"),
                  numericInput("lot", "Unite Number", value = 0),
                  textInput("url", "URL"),
                  textInput("description", "Description"),
                  textInput("model", "Model"),
                  numericInput("year", "Year", value = 0),
                  numericInput("hours", "Hours", value = 0),
                  selectInput("valuationType", "Valuation Type", choices = c("Auction", "Retail")),
                  selectInput("condition", "Equipment Condition", choices = conditions_Defaults),
                 checkboxGroupInput("valuationType_box",
                                    "Valuation Type to use in Anyalsis:",
                                    c("Auction" = "Auction", "Retail" = "Retail"),
                                    selected = "Auction")
                  ),
                  card(
                    card(
                      textOutput("TitleCard")
                    ),
                    layout_column_wrap(
                      width = 1/2,
                      card(
                        card_header("Prices to Consider"),
                        value_box(
                          title = "Predictive Price",
                          value = textOutput("predictivePrice")
                          ),
                        value_box(
                          title = "Comparison Retail Price",
                          value = textOutput("retailPrice")
                          ),
                        value_box(
                          title = "Average Price",
                          value = textOutput("averagePrice")
                          )
                        ),
                      card(
                        card_header("Plots"),
                        navset_card_tab(
                          nav_panel("$ vs Year", plotlyOutput("plot1")),
                          nav_panel("$ vs Hours", plotlyOutput("plot2")),
                          nav_panel("$ vs Condition", plotlyOutput("plot3"))
                          )
                        )
                      ),
                    card(
                        uiOutput('ex1')
                    ),
                    card(
                      card_header("Data Table"),
                      uiOutput("dynamicCheckbox"),
                      rHandsontableOutput("HistTable")
                      )
                    )
                  )
                )
              )
            )
  )

