
library(tidyverse)

load_old_data <- function(master_list) {

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

    # Define new equipment data
    unit_data <- list(
      unit = inpput_values$lot,
      year = inpput_values$year,
      hours = inpput_values$hours,
      description = inpput_values$description,
      model = inpput_values$model,
      valuationType = inpput_values$valuationType,
      condition = inpput_values$condition,
      valuation = inpput_values$valuation
    )


    # Define new market history data
    hist_data <- rhandsontable::hot_to_r(inpput_values$HistTable)

    # Append or update the entries in master_list
    master_list$Equip_List[[paste0(unit_data$unit)]] <- unit_data
    master_list$Market_Hist[[paste0(inpput_values$model)]] <- hist_data
  }



  return(master_list)
}

# Initialize master_list
master_list <- NULL

# Call function
master_list <- load_old_data(master_list)


# Save the master_list
save(master_list, file = "master_list.RData")
