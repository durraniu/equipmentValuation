
library(tidyverse)


load_old_data <- function(UnitNo, Model, master_list) {

  # Load the file
  datapath <- file.choose()
  load(datapath)  # This loads variables into the current environment

  # Ensure master_list exists
  if (is.null(master_list)) {
    master_list <- list(Equip_List = list(), Market_Hist = list())
  }

  # Create formatted names
  equip_name <- UnitNo

  # Define new equipment data
  unit_data <- list(
    unit = inpput_values$lot,
    year = inpput_values$year,
    hours = inpput_values$hours,
    description = inpput_values$description,
    model = inpput_values$model,
    valuationType = inpput_values$valuationType,
    condition = inpput_values$condition
  )

  # Define new market history data
  hist_data <- rhandsontable::hot_to_r(inpput_values$HistTable)

  # Append or update the entries in master_list
  master_list$Equip_List[[equip_name]] <- unit_data
  master_list$Market_Hist[[Model]] <- hist_data

  return(master_list)
}

# Initialize master_list
master_list <- NULL

# Call function iteratively

master_list <- load_old_data("14807", "CAT374", master_list)
master_list <- load_old_data("40709", "GD655", master_list)

# Print results
print(master_list)

