library(shiny)
library(dplyr)
library(rhandsontable)

function(input, output, session) {
  ## ---- 1) REACTIVE EXPRESSION: LOADING / PREPARING DATA ----
  
  Hist_Table <- reactive({
    inFile <- input$file1

    # checks if there is an uploaded file, if there is one, it loads the file.
    if (!is.null(inFile)) {
      load(inFile$datapath)
    }

    if (is.null(inFile)) {
      HistTable
    } else {
      Dets <- hot_to_r(inpput_values$HistTable)

      if (!"Include" %in% colnames(Dets)) {
        Dets <- Dets %>%
          mutate(Include = NA) %>%
          select(Include, everything())
      }
      Dets
    }
  })
  
  
  ##---- 3) RHANDSONTABLE: RENDERING AND FORMATTING ----
  
  # table to collect basic details of the test
  output$HistTable <- renderRHandsontable({
    
    Hist_Table <- Hist_Table()
    
    if (!is.null(Hist_Table)) {
      # Create and configure the Handsontable
      rhandsontable(Hist_Table, stretchH = "all") %>%
        hot_col(col = "Include", type = 'checkbox', halign = "htCenter") %>%
        hot_col(col = "year", format = "0", halign = "htCenter") %>%
        hot_col(col = "hours", format = "0") %>%
        hot_col(col = "price", format = "$0,0.00") %>%
        hot_col(col = "auction_year", format = "0", halign = "htCenter")%>%
        hot_cols(columnSorting = TRUE) %>%
        # make colums that are drop downs that match the inputes valuationType, workEnvironment, geographics, and warranty
        hot_col(col = "valuationType", type = "dropdown", source = c("Auction", "Retail"), strict = TRUE, halign = "htCenter") %>%
        hot_col(col = "condition", type = "dropdown", source =  c("Salvage/Scrap", "Poor", "Below Average", "Good/Average", "Exceptionally Good", "Excellent"), strict = TRUE, halign = "htCenter")
    } 
    
  })
  
  
}
