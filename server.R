
library(shiny)


function(input, output, session) {
  
  # Store reactive values (data, etc.) across user sessions
  vals <- reactiveValues(data = NULL,
                         master_list = NULL,
                         summary_list = NULL,
                         cat_names = as.character())
  
  ##---- 1) REACTIVE EXPRESSION: LOADING / PREPARING DATA ----
  observeEvent(input$pause, {
    browser()
    
    if (FALSE){
      list <- vals$master_list
    }
  })
  
  observeEvent(input$file1, {
    req(input$file1)
    
    inFile <- input$file1
    file_ext <- tools::file_ext(inFile$name)
    
    # if (is.null(inFile)) {
    #   return()
    # }
    
    if (file_ext != "RData") {
      showToast("error", "Invalid file type! Please upload a .RData file.")
      return()
    }
    
    load(inFile$datapath)
    vals$master_list <- master_list
    updateSelectInput(session, "unites", choices = names(master_list$Equip_List))
    
  })
  
  Hist_Table <- eventReactive(input$file1, {
    
    inFile <- input$file1
    unite <- input$unites
    
    if (unite == ""){
      return()
    }
    
    if (is.null(inFile)){
      
      return(HistTable)
      
    } else {
      
      model <- vals$master_list$Equip_List[[unite]]$model
      test <- vals$master_list$Market_Hist
      Dets <- as_tibble(vals$master_list$Market_Hist[[model]])
      
      if (!"Include" %in% colnames(Dets)) {
        Dets <- Dets %>% 
          mutate(Include = NA) %>% 
          select(Include, everything())
      }
      
      # Update various input fields with loaded data
      updateTextInput(session, "description", value = vals$master_list$Equip_List[[unite]]$description)
      updateSelectInput(session, "model", 
                        selected = vals$master_list$Equip_List[[unite]]$model, 
                        choices = names(vals$master_list$Market_Hist))
      
      unique_categories <- unique(unlist(lapply(vals$master_list$Equip_List, function(item) item$categorie)))
      
      updateSelectInput(session, "categorie", 
                        selected = vals$master_list$Equip_List[[unite]]$categorie, 
                        choices = unique_categories)
      
      updateNumericInput(session, "year", value = vals$master_list$Equip_List[[unite]]$year)
      updateNumericInput(session, "hours", value = vals$master_list$Equip_List[[unite]]$hours)
      updateSelectInput(session, "valuationType", selected = vals$master_list$Equip_List[[unite]]$valuationType)
      updateSelectInput(session, "condition", selected = vals$master_list$Equip_List[[unite]]$condition)
      updateSelectInput(session, "valuation", selected = vals$master_list$Equip_List[[unite]]$valuation)
      
      vals$cat_names <- unique(unlist(lapply(vals$master_list$Equip_List, function(item) item$categorie)))
      
      Dets
      
    }
    
  })
  
  observeEvent(input$unites, {
    
    inFile <- input$file1
    unite <- input$unites
    
    if (unite == ""){
      return()
    }
    
    if (is.null(inFile)){
      
      return(HistTable)
      
    } else {
      
      model <- vals$master_list$Equip_List[[unite]]$model
      Dets <- as_tibble(vals$master_list$Market_Hist[[model]])
      
      if (!"Include" %in% colnames(Dets)) {
        Dets <- Dets %>% 
          mutate(Include = NA) %>% 
          select(Include, everything())
      }
      
      # Update various input fields with loaded data
      updateTextInput(session, "description", value = vals$master_list$Equip_List[[unite]]$description)
      updateSelectInput(session, "model", 
                        selected = vals$master_list$Equip_List[[unite]]$model, 
                        choices = names(vals$master_list$Market_Hist))
      
      unique_categories <- unique(unlist(lapply(vals$master_list$Equip_List, function(item) item$categorie)))
      
      updateSelectInput(session, "categorie", 
                        selected = vals$master_list$Equip_List[[unite]]$categorie, 
                        choices = unique_categories)
      
      updateNumericInput(session, "year", value = vals$master_list$Equip_List[[unite]]$year)
      updateNumericInput(session, "hours", value = vals$master_list$Equip_List[[unite]]$hours)
      updateSelectInput(session, "valuationType", selected = vals$master_list$Equip_List[[unite]]$valuationType)
      updateSelectInput(session, "condition", selected = vals$master_list$Equip_List[[unite]]$condition)
      updateSelectInput(session, "valuation", selected = vals$master_list$Equip_List[[unite]]$valuation)
      
      vals$cat_names <- unique(unlist(lapply(vals$master_list$Equip_List, function(item) item$categorie)))
      
      Dets
      
    }
    
  })
  
  Hist_Table <- reactive({
    unite <- input$unites
    model <- vals$master_list$Equip_List[[unite]]$model
    Dets <- as_tibble(vals$master_list$Market_Hist[[model]])
    Dets
  })
  
  ##---- 2) DOWNLOAD HANDLER: SAVING DATA ----
  
  output$downloadData <- downloadHandler(
    # Generate a filename based on the current date and user input
    filename = function() {
      
      savedate <- format(Sys.time(), "%Y-%m-%d")
      
      if (!is.null(vals$master_list)) {
        paste0(savedate," - master_list.RData")
      } else {
        "NOT ENOUGH INFO TO SAVE.RData"
      }
    },
    # The actual process of saving the reactiveValues or user inputs
    content = function(file) {
      
      master_list <- isolate(vals$master_list)
      
      if (!is.null(master_list)) {
        save(master_list, file = file)
      } else {
        stop("No data available to save.")
      }
    }
  )
  
  ##---- 3) RHANDSONTABLE: RENDERING AND FORMATTING ----
  
  # table to collect basic details of the test
  output$HistTable <- renderRHandsontable({
    
    Hist_Table <- Hist_Table()
    
    if (!is.null(Hist_Table)) {
      # Create and configure the Handsontable
      #rhandsontable(Hist_Table, stretchH = "all", overflow = "visible") %>%
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
  
  ##---- 4) DYNAMIC CHECKBOX INPUT FOR SOURCES ----
  
  # Creates a UI element for the user to select the source of Auction or Retail
  output$dynamicCheckbox <- renderUI({
    
    if(!is.null(input$HistTable)){
      # Identify unique "source" values for the selected valuation types
      source_choices <- unique((hot_to_r(input$HistTable)%>%
                                  filter(valuationType %in% input$valuationType)
      )$source)
      
      # Check if "Ritchie Bros" is in the list, if not, add it
      if(!"Ritchie Bros" %in% source_choices) {
        source_choices <- c("Ritchie Bros", source_choices)
      }
      
      # Render a checkbox group with these source choices
      
      value_selected <- c("Auction", "Retail")
      
      layout_column_wrap(
        width = 1/2,
        card(
          title = "Valuation Type",
          open = FALSE,
          checkboxGroupInput("valuationType", "Valuation Type",
                             choices = c("Auction", "Retail"),
                             if (is.null(input$source_box)) value_selected else input$valuationType)
        ),
        card(
          checkboxGroupInput("source_box", "Source of Auction or Retail:",
                             choices = source_choices,
                             if (is.null(input$source_box)) source_choices else input$source_box)
        )
      )
      
      
    }
    
  })
  
  ##---- 5) FITTING A MODEL & DATA ANALYSIS REACTIVES ----
  
  # 'fit' reactive: builds a linear model based on the available data
  fit <- reactive({
    
    if (is.null(hot_to_r(input$HistTable))){
      n <- 0
    } else {
      
      df <- reused_hot_to_r(input)
      
      n <- nrow(df)
    }
    
    # If enough rows exist, build a linear model
    if (n > 2){
      
      fit_histtable(df, input$valuationType)
      
    }
    
  })
  
  ##---- 6) REACTIVE PLOTS (PLOT1, PLOT2, PLOT3) ----
  # These plots rely on the linear model and user input to show predicted vs. actual data
  
  ####---- PLOT 1: Price vs Year ----
  output$plot1 <- renderPlotly({
    
    if (is.null(hot_to_r(input$HistTable))){
      return()
    } 
    
    df <- reused_hot_to_r(input)
    if (nrow(df) <= 2) {
      return(ggplot())
    }
    
    fit <- fit()
    pred_year <- input$year
    new_data <- given_details()
    pred_price <- price_predictor(fit, new_data)
    pred_hours <-input$hours
    comp_price <- comp_price()
    average_price <- average_price()
    
    # Base ggplot with points and a linear fit
    p <- df %>%
      ggplot(aes(x = year, y = price)) +
      geom_point(aes(color =  valuationType), size = 2) +
      geom_smooth(method = "lm") +
      geom_point(x = pred_year, y = pred_price, size = 4, color = "Red", shape = 4) +
      geom_point(x = pred_year, y = comp_price, size = 4, color = "Blue", shape = 4) +
      geom_point(x = pred_year, y = average_price, size = 4, color = "Green", shape = 4)+
      theme(legend.position="none")
    
    if (!is.na(input$valuation)){
      valuation_y <-  input$valuation
      p <- p + geom_point(x = pred_year, y = valuation_y, size = 4, color = "Orange", shape = 13)
    }
    
    p
    
  })
  
  ####---- PLOT 2: Price vs Hours ----
  output$plot2 <- renderPlotly({
    
    if (is.null(hot_to_r(input$HistTable))){
      return()
    } 
    
    df <- reused_hot_to_r(input)
    if (nrow(df) <= 2) {
      return(ggplot())
    }
    
    fit <- fit()
    new_data <- given_details()
    pred_year <- input$year
    pred_price <- price_predictor(fit, new_data)
    pred_hours <-input$hours
    comp_price <- comp_price()
    average_price <- average_price()
    
    p2 <- df %>%
      ggplot(aes(x = hours, y = price)) +
      geom_point(aes(color =  valuationType), size = 2) +
      geom_smooth(method = "lm") +
      geom_point(x = pred_hours, y = pred_price, size = 4, color = "Red", shape = 4) +
      geom_point(x = pred_hours, y = comp_price, size = 4, color = "Blue", shape = 4) +
      geom_point(x = pred_hours, y = average_price, size = 4, color = "Green", shape = 4)+
      theme(legend.position="none")
    
    if (!is.na(input$valuation)){
      valuation_y <-  input$valuation
      p2 <- p2 + geom_point(x = pred_hours, y = valuation_y, size = 4, color = "Orange", shape = 13)
    }
    
    p2
    
  })
  
  ####---- PLOT 3: Price vs Condition Index ----
  output$plot3<- renderPlotly({
    
    if (is.null(hot_to_r(input$HistTable))){
      return()
    } 
    
    df <- reused_hot_to_r(input)
    if (nrow(df) <= 2) {
      return(ggplot())
    }
    
    fit <- fit()
    new_data <- given_details()
    pred_year <- input$year
    pred_price <- price_predictor(fit, new_data)
    pred_hours <-input$hours
    pred_condition_index <- input$condition %>%
      factor(., levels = conditions_Defaults) %>%
      as.numeric(.)
    comp_price <- comp_price()
    average_price <- average_price()
    
    p3 <- df %>%
      ggplot(aes(x = condition_index, y = price)) +
      geom_point(aes(color =  valuationType), size = 2) +
      geom_smooth(method = "lm") +
      geom_point(x = pred_condition_index, y = pred_price, size = 4, color = "Red", shape = 4) +
      geom_point(x = pred_condition_index, y = comp_price, size = 4, color = "Blue", shape = 4) +
      geom_point(x = pred_condition_index, y = average_price, size = 4, color = "Green", shape = 4) +
      theme(legend.position="none")
    
    if (!is.na(input$valuation)){
      valuation_y <-  input$valuation
      p3 <- p3 + geom_point(x = pred_condition_index, y = valuation_y, size = 4, color = "Orange", shape = 13)
    }
    
    p3
    
  })
  
  ##---- 7) COMPARISON TABLE & PRICE COMPUTATIONS ----
  
  # comp_Table: builds a summary comparison of Auction vs Retail data
  comp_Table <- reactive({
    req(input$HistTable)
    df <-  hot_to_r(input$HistTable) %>%
      filter(Include == TRUE)
    
    # If both Auction and Retail data exist, create a summary table
    if (all(unique(sort(df$valuationType)) == sort(c("Auction", "Retail")))) {
      #print("Both")
      
      compTable <- df%>%
        group_by(valuationType) %>%
        summarise(n = n(),
                  highest = max(price),
                  lowest = min(price),
                  average = mean(price)) %>%
        # Reshape the data into a wide format
        pivot_longer(-valuationType) %>%
        pivot_wider(names_from = valuationType, values_from = c(value)) %>%
        mutate(diff = if_else(name == "n",NA,1 - Auction/Retail))
    } else {
      # If only one type is present, create partial summary
      #print("Just One")
      
      compTable <- df %>%
        group_by(valuationType)  %>%
        summarise(n = n(),
                  highest = max(price),
                  lowest = min(price),
                  average = mean(price)) %>%
        add_row(valuationType = "Retail", n = 0, highest = max(df$price), lowest = max(df$price), average = max(df$price)) %>%
        pivot_longer(-valuationType) %>%
        pivot_wider(names_from = valuationType, values_from = c(value)) %>%
        mutate(diff = if_else(name == "n",NA,1 - Auction/Retail))
      
    }
    
    
    compTable
    
  })
  
  # comp_price: calculates a "comparison" retail price based on the summarized table
  comp_price <- reactive({
    
    compTable <- comp_Table()
    
    comp_price <- compTable$Retail[2] * compTable$diff[4]
    
    comp_price
    
  })
  
  output$price_retail <- renderText({
    req(comp_price())
    comp_price()
  })
  
  # average_price: calculates the midpoint of the predicted price and the comparison price
  average_price <- reactive({
    req(input$HistTable)
    
    new_data <- given_details()
    
    df <-  hot_to_r(input$HistTable)
    fit <- fit()
    
    pred_price <- price_predictor(fit, new_data)
    
    comp_price <- comp_price()
    
    average_price <- (pred_price+comp_price)/2
    
    average_price
    
  })
  
  output$price_average <- renderText({
    if (identical(average_price(), 0)) {
      return(0)
    }
    req(average_price())
    average_price()
  })
  
  # given_details: constructs a new data frame for prediction based on user inputs
  given_details <- reactive({
    per_year <- input$year
    per_hours <- input$hours
    per_source <- input$valuationType
    
    per_condition <- input$condition %>%
      factor(., levels = conditions_Defaults) %>%
      as.numeric(.)
    
    df <- hot_to_r(input$HistTable)
    
    # Identify relevant variables to include in the new data row
    variable_list <- make_variable_list(df, input)
    new_data <- setNames(data.frame(matrix(ncol = length(variable_list), nrow = 0)), variable_list)
    new_data [ nrow(new_data) + 1 , ] <- NA
    
    # Create a tibble with the relevant variables (year, hours, etc.)
    new_data <- tibble(year = per_year,
                       hours = per_hours,
                       #valuationType = per_source,
                       condition_index = per_condition)
    
  })
  
  ##---- 8) DISPLAY COMPARISON TABLE ----
  output$compTable <- renderTable({
    
    compTable <- comp_Table()
    
    compTable
    
  })
  
  ##---- 9) FORMULA OUTPUT (ex1) and VALUE BOX OUTPUT ----
  
  output$TitleCard <- renderText({
    paste0("Unit ", input$lot, " - ", input$description)
  })
  
  output$predictivePrice <- renderText({
    
    if (is.null(hot_to_r(input$HistTable))){
      n <- 0
    } else {
      fit <- fit()
      df <- hot_to_r(input$HistTable)
      n <- nrow(df)
      new_data <- given_details()
    }
    
    if (n > 2){
      #params <- summary(fit)
      pred_price <- price_predictor(fit, new_data)
      paste0("$", format(round(pred_price, 2), big.mark = ",", nsmall = 2))
    } else {
      "No Data Yet"
    }
  })
  
  output$retailPrice <- renderText({
    comp_price <- comp_price()
    
    if (comp_price > 0){
      paste0("$", format(round(comp_price, 2), big.mark = ",", nsmall = 2))
    } else {
      "No Data Yet"
    }
    
  })
  
  output$averagePrice <- renderText({
    
    
    if (is.null(hot_to_r(input$HistTable))){
      n <- 0
    } else {
      fit <- fit()
      df <- hot_to_r(input$HistTable)
      n <- nrow(df)
      new_data <- given_details()
    }
    
    if (n > 2){
      average_price <- average_price()
      paste0("$", format(round(average_price, 2), big.mark = ",", nsmall = 2))
    } else {
      "No Data Yet"
    }
    
  })
  
  output$plotPriceYear <- renderPlot({
    # Example plot
    plot(1:10, 1:10, type = "l")
  })
  
  predictive_price <- reactiveVal()
  
  output$price_pred <- renderText({
    req(predictive_price())
    predictive_price()
  })
  
  # Renders a UI with text, summary statistics, and plots
  output$ex1 <- renderUI({
    
    if (is.null(hot_to_r(input$HistTable))){
      n <- 0
    } else {
      fit <- fit()
      df <- hot_to_r(input$HistTable)
      n <- nrow(df)
      new_data <- given_details()
    }
    
    if (n > 2){
      params <- summary(fit)
      pred_price <- price_predictor(fit, new_data)
      predictive_price(pred_price)
      comp_price <- comp_price()
      average_price <- average_price()
      
      # Browser variables extracted for clarity
      RetailMax <- comp_Table()$Retail[2]
      RetailAverage <- comp_Table()$Retail[4]
      AuctionAverage <- comp_Table()$Auction[4]
      
      tagList(
        # Two-plot layout for Price vs Year and Price vs Hours
        card(
          card_header(
            tags$h2("Plots")
          ),
          layout_column_wrap(
            width = 1/3,
            plotlyOutput("plot1"),
            plotlyOutput("plot2"),
            plotlyOutput("plot3")
          )
        ),
        card(
          card_header(
            tags$h2("Auction vs Retail Comparison Table"),
          ),
          tableOutput("compTable")
        )
        
      )
      
    } else {
      "Waiting for Data"
    }
    
  })
  
  ##---- 10) Summary Page ----
  
  output$dt_summary <- renderDT({
    
    # Build Sumamry Table
    master_list <- vals$master_list
    if (!is.null(master_list)){
      
      unit_list <- names(master_list$Equip_List)
      
      summary_table <- NULL
      
      for (i in unit_list) {
        
        market_data <- master_list$Market_Hist[[master_list$Equip_List[[i]]$model]]
        
        fit_liquidation <- fit_histtable(market_data, "Auction")
        fit_market <- fit_histtable(market_data, "Retail")
        
        i_data <- tibble(year =  master_list$Equip_List[[i]]$year,
                         hours = master_list$Equip_List[[i]]$hours,
                         condition = master_list$Equip_List[[i]]$condition) %>%
          mutate(condition = factor(condition, levels = conditions_Defaults)) %>%
          mutate(condition_index = as.numeric(condition))
        
        value_liquidation <- price_predictor(fit_liquidation, i_data)
        value_market <- price_predictor(fit_market, i_data)
        
        if (is.null(master_list$Equip_List[[i]]$valuation)) {
          valueation_to_use <- 0
        } else {
          valueation_to_use <- master_list$Equip_List[[i]]$valuation
        }
        
        summary_table <- rbind(summary_table, tibble(Unit = i,
                                                     categorie = master_list$Equip_List[[i]]$categorie,
                                                     Liquidation = value_liquidation,
                                                     Market = value_market,
                                                     valuation = valueation_to_use))
      }
      
      if (is.null(summary_table)) {
        return()
      }
      
      summary_output <- summary_table %>%
        group_by(categorie) %>%
        summarize(Liquidation = sum(Liquidation),
                  Market = sum(Market),
                  Valuation = sum(valuation))
      
      
      datatable(
        summary_output,
        rownames = FALSE,
        selection = "single",   # So we can detect which row was clicked
        options = list(
          # Minimal styling
          dom = 't',    # show just the table, no search box or pagination
          paging = FALSE,
          ordering = FALSE
        ),
        class = "compact stripe cell-border"
      ) %>%
        formatCurrency(
          columns = c("Liquidation", "Market", "Valuation"),
          currency = "$",
          digits = 0,       # Number of decimal places
          interval = 3,     # Helps place commas (1,000 vs 1000)
          mark = ","        # Thousands separator
        )
    }
    
  })
  
  #--- Observe which row is selected in summaryTable
  observeEvent(input$dt_summary_rows_selected, {
    #print(input$dt_summary_rows_selected)
    
    master_list <- vals$master_list
    
    unit_list <- names(master_list$Equip_List)
    
    summary_table <- NULL
    
    for (i in unit_list) {
      #print(i)
      
      market_data <- master_list$Market_Hist[[master_list$Equip_List[[i]]$model]]
      
      fit_liquidation <- fit_histtable(market_data, "Auction")
      fit_market <- fit_histtable(market_data, "Retail")
      
      i_data <- tibble(year =  master_list$Equip_List[[i]]$year,
                       hours = master_list$Equip_List[[i]]$hours,
                       condition = master_list$Equip_List[[i]]$condition) %>%
        mutate(condition = factor(condition, levels = conditions_Defaults)) %>%
        mutate(condition_index = as.numeric(condition))
      
      value_liquidation <- price_predictor(fit_liquidation, i_data)
      value_market <- price_predictor(fit_market, i_data)
      
      if (is.null(master_list$Equip_List[[i]]$valuation)) {
        valueation_to_use <- 0
      } else {
        valueation_to_use <- master_list$Equip_List[[i]]$valuation
      }
      
      summary_table <- rbind(summary_table, tibble(Unit = i,
                                                   categorie = master_list$Equip_List[[i]]$categorie,
                                                   Liquidation = value_liquidation,
                                                   Market = value_market,
                                                   valuation = valueation_to_use))
    }
    
    summary_output <- summary_table %>%
      group_by(categorie) %>%
      summarize(Liquidation = sum(Liquidation),
                Market = sum(Market),
                Valuation = sum(valuation))
    
    #row_selected <- summary_output$categorie[input$dt_summary_rows_selected]
    
    if(TRUE) {
      # Get the category from the summary table
      cat_selected <- summary_output$categorie[input$dt_summary_rows_selected]
      
      # Filter the original raw data for that category
      detail_data <- summary_table %>% filter(categorie == cat_selected)
      
      # Show a modal with a second DT (or any UI) to display the detail
      #browser()
      showModal(
        modalDialog(
          title = paste("Details for category:", cat_selected),
          renderDT({
            datatable(
              detail_data,
              rownames = FALSE,
              options = list(dom = 't', paging = FALSE)
            ) %>%
              formatCurrency(
                columns = c("Liquidation","Market","valuation"),
                currency = "$",
                digits = 0,
                mark = ","
              )
          }),
          easyClose = TRUE,
          size = "l"
        )
      )
    }
  })
  
  ##---- 11) Add New Units ----
  observeEvent(input$new_unit, {
    showModal(modalDialog(
      title = "Adding a New Unit",
      "This is an important message!",
      textInput("add_new_unites", "New Unite Number"),
      textInput("description", "Description"),
      selectInput("categorie", "Categorie", vals$cat_names),
      selectInput("model", "Model", choices = names(vals$master_list$Market_Hist)),  # Dropdown for Models in our Hist Data
      numericInput("year", "Year", value = 0),
      numericInput("hours", "Hours", value = 0),
      selectInput("condition", "Equipment Condition",
                  choices = conditions_Defaults),
      easyClose = FALSE,
      footer = tagList(actionButton("confirmCreate_unit", "Create"),
                       modalButton("Cancel"))
    ))
  })
  
  observeEvent(input$confirmCreate_unit, {
    
    new_name <- input$add_new_unites
    new_item <- list(description = input$description,
                     categorie = input$categorie,
                     model = input$model,
                     year = input$year,
                     hours = input$hours,
                     condition = input$condition,
                     valuationType = NULL,
                     valuation = NULL
    )
    
    vals$master_list$Equip_List[[new_name]] <- new_item
    
    sorted_names <- sort(names(vals$master_list$Equip_List))
    vals$master_list$Equip_List <- vals$master_list$Equip_List[sorted_names]
    
    updateSelectInput(session, "unites", choices = sorted_names)
    
    removeModal()
    
  })
  
  
  ##---- 12) Add New Models ----
  observeEvent(input$new_model, {
    showModal(modalDialog(
      title = "Adding a New Model",
      "Create a new empty History Table for a new Model",
      textInput("add_new_models", "New Model"),
      easyClose = FALSE,
      footer = tagList(actionButton("confirmCreat_emodel", "Create"),
                       modalButton("Cancel"))
    ))
  })
  
  observeEvent(input$confirmCreat_emodel, {
    
    new_model <- input$add_new_models
    new_model_hist <- tribble(
      ~Include, ~Description,	~Model,	~year,	~hours,	~price,	~valuationType,	~source, ~auction_year, ~condition,
      TRUE, paste0("Example: 2022 ", input$add_new_models),	input$add_new_models,	2022,	7500,	100000,	"Auction",	"Ritchie Bros", 2023, "Below Average",
      TRUE, paste0("Example: 2023 ", input$add_new_models),	input$add_new_models,	2023,	5500,	200000,	"Auction",	"Ritchie Bros", 2023, "Good/Average",
      TRUE, paste0("Example: 2024 ", input$add_new_models),	input$add_new_models,	2024,	2500,	300000,	"Auction",	"Ritchie Bros", 2023, "Excellent",
      TRUE, paste0("Example: 2022 ", input$add_new_models),	input$add_new_models,	2022,	7500,	200000,	"Retail",	"Iron Planet", 2023, "Below Average",
      TRUE, paste0("Example: 2023 ", input$add_new_models),	input$add_new_models,	2023,	5500,	300000,	"Retail",	"Iron Planet", 2023, "Good/Average",
      TRUE, paste0("Example: 2024 ", input$add_new_models),	input$add_new_models,	2024,	2500,	400000,	"Retail",	"Iron Planet", 2023, "Excellent"
    )
    
    vals$master_list$Market_Hist[[new_model]] <- new_model_hist
    
    new_model_names <- c(names(vals$master_list$Market_Hist), new_model)
    
    updateSelectInput(session, "model", choices = new_model_names)
    
    removeModal()
    
  })
  
  ##---- 16) Add New Category Button ----
  observeEvent(input$new_catagorie, {
    showModal(modalDialog(
      title = "Adding a Catagorie",
      "Create a new Catagorie for the Categorie dropdown",
      textInput("add_new_categorie", "Categorie"),
      easyClose = FALSE,
      footer = tagList(actionButton("confirmCreat_cat", "Create"),
                       modalButton("Cancel"))
    ))
  })
  
  observeEvent(input$confirmCreat_cat, {
    
    vals$cat_names <- c(vals$cat_names, input$add_new_categorie)
    
    updateSelectInput(session, "categorie", choices = vals$cat_names)
    
    removeModal()
    
  })
  
  ##---- 13) Update Valuation ----
  observeEvent(input$assign_valuation, {
    
    vals$master_list$Equip_List[[input$unites]]$valuation <- input$valuation
    
  })
  
  ##---- 14) New blank Master Button ----
  
  # A button that is only visible when no file is loaded and no blank master has been made
  output$master_button <- renderUI({
    
    #hide button if a file has been imported
    if (!is.null(input$file1)){
      return()
    }
    
    # hide button if a master list already exists
    if (!is.null(vals$master_list)){
      return()
    }
    
    #button
    actionButton("new_blank_master", HTML("Make New <br>Blank dataset"))
    
  })
  
  # creates an empty list of lists with the expected format of an improted master_list.RData
  observeEvent(input$new_blank_master, {
    
    vals$master_list <- list(Equip_List = list(),
                             Market_Hist = list())
    
  })
  
  ##---- 15) Save HistTable Button ----
  
  observeEvent(input$save_HistTable, {
    
    # id the currently selected model to overwrite the correct HistTable
    model <- input$model
    
    # Save the current HistTable to a reactiveValues object
    vals$master_list$Market_Hist[[model]] <- hot_to_r(input$HistTable)
    
  })
  
  ##---- 16) Save Unit Details if they have been changed Button ----
  observeEvent(input$save_details, {
    
    unite <- input$unites
    
    if (input$unites == ""){
      return()
    }
    
    if (is.null(input$valuationType)){
      return()
    }
    
    updated_item <- list(unite = unite,
                         year = input$year,
                         hours = input$hours,
                         description = input$description,
                         model = input$model,
                         valuationType = input$valuationType,
                         condition = input$condition,
                         valuation = input$valuation,
                         categorie = input$categorie
    )
    
    vals$master_list$Equip_List[[unite]] <- updated_item
    
  })
}