rm(list = ls())          # clear all objects in memory
#dev.off()                  # reload graphic device
cat("\014")                # clear console
options(max.print = 5000)  # expand display
options(scipen=10)
# Load packages
if (!require("pacman")) install.packages("pacman")  # load packages

pacman::p_load (dplyr
                , haven
                , ggplot2
                , ggpubr
                , ggthemes  ## si quieren mas themes
                , readstata13
                , readxl
                , sf
                , tidyverse
                , tidyr
                , units
                , viridis ## paleta de colores Viridis
                , wesanderson## p/usar paleta de colores de Wes Anderson
                , stringr
                , RColorBrewer
                , patchwork
                , Rmisc
                , lfe
                , stargazer
                , AER
                , haven
                , skimr
                , modelsummary
                , terra
                , fixest
                , vtable
                , did
                , cowplot
                , grid
                , psych
                , pander
                , knitr
                , survey
                , car
                , xlsx)

####Append process####

# Set your working directory
setwd("~/Downloads/Data2Analyse/080807_WearMillingY_50_")

# List all folders that contain "Level" in their name
folders <- list.dirs(full.names = TRUE, recursive = FALSE)
folders <- folders[grepl("Level", folders)]  # Filter folders that contain "Level"

# Create an empty dataframe to store the results
all_data <- data.frame()

# Loop through each folder
for (folder in folders) {
  # Get the folder name
  folder_name <- basename(folder)
  
  # List all CSV files in the folder
  files <- list.files(path = folder, pattern = "*.csv", full.names = TRUE)
  
  # Check if there are any files in the folder
  if (length(files) == 0) {
    message(paste("No CSV files found in folder:", folder_name))
    next  # Skip to the next folder if no files are found
  }
  
  # Loop through each file
  for (file in files) {
    # Get the file name
    file_name <- basename(file)
    
    # Try reading the CSV file, adding an error handler in case the file is not read correctly
    message(paste("Reading file:", file_name, "from folder:", folder_name))
    tryCatch({
      data <- read.csv(file)
      
      # Add the folder name and file name as new columns
      data$Folder <- folder_name
      data$File <- file_name
      
      # Append the data to the main dataframe
      all_data <- rbind(all_data, data)
      
      message(paste("Successfully appended data from:", file_name))
      
    }, error = function(e) {
      message(paste("Error reading file:", file_name, "in folder:", folder_name))
      message(e)
    })
  }
}

# Check if any data was appended
if (nrow(all_data) > 0) {
  # View the first few rows of the combined dataframe
  print(head(all_data))
  
  # Save the final dataframe to a CSV file
  write.csv(all_data, "~/Downloads/combined_data.csv", row.names = FALSE)
  message("Data successfully saved to combined_data.csv")
} else {
  message("No data was appended to the dataframe.")
}

all_data <- all_data %>% 
  dplyr::rename(
    "torque_x" = "X..Nck..SD.nckServoDataActCurr32..u1..1.",  # X..Nck..SD.nckServoDataActCurr32..u1..1.
    "torque_y" = "X..Nck..SD.nckServoDataActCurr32..u1..2.",  # X..Nck..SD.nckServoDataActCurr32..u1..2.
    "torque_spindle" = "X..Nck..SD.nckServoDataActCurr32..u1..4.",  
    "position_2" = "X..Nck..SD.nckServoDataActPos1stEnc32..u1..2.",
    "position_3" = "X..Nck..SD.nckServoDataActPos1stEnc32..u1..3.",
    "position_4" = "X..Nck..SD.nckServoDataActPos1stEnc32..u1..4."
  )

data.table::fwrite(all_data, "all_data_WearMillingY_50_L1_L108.csv")

####New Data####
all_data <- data.table::fread("all_data_WearMillingY_50_L1_L108.csv")
# Check the new column names
table(all_data$Folder)

summary(all_data$time)

# Create a new adjusted time column
all_data <- all_data %>%
  dplyr::group_by(Folder) %>%
  dplyr::mutate(adjusted_time = time + lag(cumsum(max(time)), default = 0)) %>% 
  dplyr::ungroup()

# Define the time window size (e.g., 1 second)
time_window <- 1  # You can change this value to define the window size

# Group the data by Folder and time window, 
#then calculate the mean torque and position for each window, i scale all the variables due to the
# different magnitudes
aggregated_data <- all_data %>%
  dplyr::mutate(time_bin = floor(adjusted_time / time_window) * time_window) %>%
  dplyr::group_by(Folder, time_bin) %>%
  dplyr::summarise(
    mean_torque_x = mean(torque_x, na.rm = TRUE),
    mean_torque_y = mean(torque_y, na.rm = TRUE),
    mean_torque_spindle = mean(torque_spindle, na.rm = TRUE),
    mean_pos_2 = mean(position_2, na.rm = TRUE),
    mean_pos_3 = mean(position_3, na.rm = TRUE),
    mean_pos_4 = mean(position_4, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    mean_torque_x_scale = scale(mean_torque_x),
    mean_torque_y_scale = scale(mean_torque_y),
    mean_torque_spindle_scale = scale(mean_torque_spindle),
    mean_pos_2_scale = scale(mean_pos_2),
    mean_pos_3_scale = scale(mean_pos_3),
    mean_pos_4_scale = scale(mean_pos_4))

# Plot mean torque over time for different levels
ggplot(aggregated_data, aes(x = time_bin)) +
  geom_line(aes(y = mean_torque_x_scale, color = "Torque X")) +
  geom_line(aes(y = mean_torque_y_scale, color = "Torque Y")) +
  geom_line(aes(y = mean_torque_spindle_scale, color = "Torque Spindle")) +
  facet_wrap(~ Folder, scales = "free_y") +  # Compare across different levels
  labs(title = "Mean Torque Over Time by Folder (Level)", 
       x = "Time (s)", y = "Mean Torque (A)") +
  theme_minimal()


# Position over adjusted time
ggplot(aggregated_data, aes(x = time_bin)) +
  geom_line(aes(y = mean_pos_2_scale, color = "Position 2")) +
  geom_line(aes(y = mean_pos_3_scale, color = "Position 3")) +
  geom_line(aes(y = mean_pos_4_scale, color = "Position 4")) +
  facet_wrap(~ Folder, scales = "free_y") +  # Compare across different levels
  labs(title = "Mean Position Over Time by Folder (Level)", 
       x = "Time (s)", y = "Mean Position (mm)") +
  theme_minimal()





