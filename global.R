library(tidyverse)
library(scales)
library(rhandsontable)
library(plotly)

## Get rid of scientific notation ----
options(scipen = 999)

## Base Data & Constants ----

# Initial example data table for historical comparisons
HistTable <- tribble(
  ~Include, ~Description,	~Model,	~year,	~hours,	~price,	~valuationType,	~source, ~auction_year, ~condition,
  TRUE, "Example: 2014 Cat 336EL",	"336EL",	2014,	2500,	200000,	"Auction",	"Ritchie Bros", 2023, "Below Average",
  NA , NA,	NA,	NA,	NA,	NA,	NA,	NA, NA,	NA
)

# Standard condition levels for dropdowns & RHandsontable
conditions_Defaults <- c("Excellent", "Exceptionally Good", "Good/Average", "Below Average", "Poor", "Salvage/Scrap")

#### make_variable_list ----
# This function creates a list of variables (column names) for modeling
# based on whether there's sufficient variability and whether user-selected
# inputs match the data in each variable.
make_variable_list <- function(df, input){

  input_list <-   c(year = input$year,
                    hours = input$hours,
                    workEnvironment = input$workEnvironment,
                    geographics = input$geographics,
                    warranty = input$warranty,
                    valuationType = input$valuationType,
                    condition = input$condition)

  # Start with a broader list of potential variables
  variable_list <- c("year", "hours", "workEnvironment", "geographics", "warranty", "valuationType")

  # Loop through each potential variable to ensure at least 2 unique data values
  # and that the user's selected value is actually present in the data.
  for(i in variable_list){
    # check if the variable matching i has more than 1 unique value, if it doesn't remove it from variable_list
    if (length(unique(df[[i]])) < 2) {
      variable_list <- variable_list[variable_list != i]
    } else if (!any(unique(df[[i]]) %in%  input_list[i])) {
      variable_list <- variable_list[variable_list != i]
    }
  }

  variable_list <- unique(append(variable_list, c("year", "hours", "condition")))

  return(variable_list)

}

#### price_predictor ----
# Given a fitted linear model (fit) and a new_data row,
# predicts the price using predict() from base R.
price_predictor <- function(fit, new_data){

  if (identical(fit, 0)) {
    return(0)
  }
  pred_price <- predict(fit, newdata = new_data)
  return(pred_price)

}

#### reused_hot_to_r ----
# Converts the Handsontable input data to an R data frame, then filters it
# based on the user's selected valuation types, source, and whether items
# are included. Adds a numeric condition_index field for modeling.
reused_hot_to_r <- function(input_data){
  
  output <- hot_to_r(input_data$HistTable) %>% 
    filter(valuationType %in% input_data$valuationType) %>%
    filter(Include == TRUE) %>%
    mutate(condition = factor(condition, levels = conditions_Defaults)) %>%
    mutate(condition_index = as.numeric(condition))
  
  if (!is.null(input_data$source_box)) {
    output %>%
      filter(source %in% input_data$source_box)
  } else {
    output
  }
}


#### Fit Model ----
fit_histtable <- function(df, value_input) {
  tryCatch({
    df <- df %>% 
      filter(valuationType %in% value_input) %>%
      filter(Include == TRUE) %>%
      mutate(condition = factor(condition, levels = conditions_Defaults)) %>%
      mutate(condition_index = as.numeric(condition))
    
    if (length(unique(df$condition)) == 1) {
      variable_list <- c("year", "hours")
    } else {
      variable_list <- c("year", "hours", "condition_index")
    }
    
    # Construct a formula dynamically for the chosen variables
    formula <- reformulate(termlabels = variable_list, response = 'price')
    
    # Fit the linear model
    fit <- lm(formula, data = df)
    
    return(fit)
    
  }, error = function(e) {
    message("Error in fit_histtable: ", e$message)
    return(0)
  })
}

