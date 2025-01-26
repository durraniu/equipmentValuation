
library(tidyverse)

## Function to build Dummy Dataset ----
## form a folder that contains some number of Rdata files
load_old_data <- function(master_list, categories) {

  # Load the file
  #datapath <- file.choose()
  datapath <- choose.dir()

  files <- list.files(datapath)

  # Ensure master_list exists
  if (is.null(master_list)) {
    master_list <- list(Equip_List = list(), Market_Hist = list())
  }

  for (i in files) {

    load(paste0(datapath,"\\",i))  # This loads variables into the current environment

    lot_cat <- floor(inpput_values$lot / 1000) * 1000 
    
    categorie_to_use <- categories %>%
      filter(Category == lot_cat) %>%
      pull(Description)
    
    # Define new equipment data
    unit_data <- list(
      unit = inpput_values$lot,
      year = inpput_values$year,
      hours = inpput_values$hours,
      description = inpput_values$description,
      model = inpput_values$model,
      valuationType = inpput_values$valuationType,
      condition = inpput_values$condition,
      valuation = inpput_values$valuation,
      categorie = categorie_to_use
    )


    # Define new market history data
    hist_data <- rhandsontable::hot_to_r(inpput_values$HistTable)

    # Append or update the entries in master_list
    master_list$Equip_List[[paste0(unit_data$unit)]] <- unit_data
    master_list$Market_Hist[[paste0(inpput_values$model)]] <- hist_data
  }



  return(master_list)
}

## open EMCM.Rds from a different project ----
file <- choose.files()
categories <- readRDS(file)

categories <- categories %>%
  select(Category, Description)

## Make the Dummy Dataset ----
# Initialize master_list
master_list <- NULL

# Call function
master_list <- load_old_data(master_list, categories)

## Save the Dummy Dataset ----
save(master_list, file = "master_list.RData")





