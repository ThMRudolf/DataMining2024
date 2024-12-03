# Clear console and expand display options
cat("\014")
options(max.print = 5000, scipen = 10)

# Load required packages using pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, haven, ggplot2, ggpubr, ggthemes, readstata13, readxl, sf, tidyverse,
  tidyr, units, viridis, wesanderson, stringr, RColorBrewer, patchwork, Rmisc,
  lfe, stargazer, AER, skimr, modelsummary, terra, fixest, vtable, did, cowplot,
  grid, psych, pander, knitr, survey, car, xlsx, zoo, plotly, GGally, patchwork,
  randomForest, doParallel, factoextra)

# Set working directory
setwd("~/Downloads/Data2Analyse/080807_WearMillingY_50_")

##### List folders containing "Level" in their name####
folders <- list.dirs(full.names = TRUE, recursive = FALSE) %>%
  .[grepl("Level", .)]

# Create an empty list to store dataframes
all_data_list <- list()

# Loop through each folder and file, reading and appending them
for (folder in folders) {
  folder_name <- basename(folder)
  files <- list.files(path = folder, pattern = "*.csv", full.names = TRUE)
  
  if (length(files) == 0) {
    message(paste("No CSV files found in folder:", folder_name))
    next
  }
  
  for (file in files) {
    file_name <- basename(file)
    message(paste("Reading file:", file_name, "from folder:", folder_name))
    
    tryCatch({
      data <- read.csv(file)
      
      # Add folder and file name columns
      data <- data %>%
        mutate(Folder = folder_name, File = file_name)
      
      # Store in the list
      all_data_list[[length(all_data_list) + 1]] <- data
      
      message(paste("Successfully appended data from:", file_name))
    }, error = function(e) {
      message(paste("Error reading file:", file_name, "in folder:", folder_name))
      message(e)
    })
  }
}

# Combine all dataframes using bind_rows to handle different columns
all_data <- bind_rows(all_data_list, .id = "source")

names(all_data)

# Rename columns as specified
all_data <- all_data %>%
  dplyr::select(-("X..Channel..RP.rpa..u1..15.")) %>% 
  dplyr::rename(
    torque_x = "X..Nck..SD.nckServoDataActCurr32..u1..1.",
    torque_y = "X..Nck..SD.nckServoDataActCurr32..u1..2.",
    torque_z = "X..Nck..SD.nckServoDataActCurr32..u1..3.",
    torque_spindle = "X..Nck..SD.nckServoDataActCurr32..u1..4.",
    position_1 = "X..Nck..SD.nckServoDataActPos1stEnc32..u1..1.",
    position_2 = "X..Nck..SD.nckServoDataActPos1stEnc32..u1..2.",
    position_3 = "X..Nck..SD.nckServoDataActPos1stEnc32..u1..3.",
    position_4 = "X..Nck..SD.nckServoDataActPos1stEnc32..u1..4."
  )

# Save the final combined dataframe
if (nrow(all_data) > 0) {
  data.table::fwrite(all_data, "~/Downloads/combined_data.csv", row.names = FALSE)
  message("Data successfully saved to combined_data.csv")
} else {
  message("No data was appended to the dataframe.")
}

#### EDA ####

all_data <- data.table::fread("combined_data.csv")

summary(all_data)

all_data <- as.data.frame(all_data)

# Create a new adjusted time column
all_data <- all_data %>%
  dplyr::arrange(Folder, File, time) 

all_data <- all_data %>%
  dplyr::group_by(Folder) %>%
  dplyr::mutate(adjusted_time = time + dplyr::lag(cumsum(time), default = 0)) %>%
  dplyr::ungroup()

# Define the time window size (e.g., every 2 seconds!!!)
time_window <- 15000  # You can change this value to define the window size

# Group the data by Folder and time window, 
#then calculate the mean torque and position for each window, i scale all the variables due to the
# different magnitudes
names(all_data)
all_data <- all_data %>%
  dplyr::mutate(time_bin = floor(adjusted_time / time_window)*.5)

table(all_data$time_bin)

Grouped_data <- all_data %>%
  dplyr::group_by(Folder, time_bin) %>%
  dplyr::summarise(
    mean_torque_x = mean(torque_x, na.rm = TRUE),
    mean_torque_y = mean(torque_y, na.rm = TRUE),
    mean_torque_z = mean(torque_z, na.rm = TRUE),
    mean_torque_spindle = mean(torque_spindle, na.rm = TRUE),
    mean_pos_1 = mean(position_1, na.rm = TRUE),
    mean_pos_2 = mean(position_2, na.rm = TRUE),
    mean_pos_3 = mean(position_3, na.rm = TRUE),
    mean_pos_4 = mean(position_4, na.rm = TRUE))


Grouped_data <- Grouped_data %>%
  dplyr::mutate(
    mean_torque_x_scale = scale(mean_torque_x),
    mean_torque_y_scale = scale(mean_torque_y),
    mean_torque_z_scale = scale(mean_torque_z),
    mean_torque_spindle_scale = scale(mean_torque_spindle),
    mean_pos_1_scale = mean(mean_pos_1, na.rm = TRUE),
    mean_pos_2_scale = scale(mean_pos_2),
    mean_pos_3_scale = scale(mean_pos_3),
    mean_pos_4_scale = scale(mean_pos_4))

# Plot mean torque over time for different levels
ggplot(Grouped_data, aes(x = time_bin)) +
  geom_line(aes(y = mean_torque_x_scale, color = "Torque x")) +
  geom_line(aes(y = mean_torque_y_scale, color = "Torque y")) +
  geom_line(aes(y = mean_torque_z_scale, color = "Torque z")) +
  geom_line(aes(y = mean_torque_spindle_scale, color = "Torque Cabezal")) +
  facet_wrap(~ Folder, scales = "free_y", ncol = 12, nrow = 10) +  # Adjust ncol and nrow as needed
  labs(
    title = "Mean Torque Over Time by Folder (Per Minute)", 
    x = "Time (Minutes)", 
    y = "Mean Torque Scaled",
    color = "Torque Variables"
  ) +
  scale_x_continuous(
    breaks = seq(0, 12, by = 2),  # Breaks every 0.5 minutes
    limits = c(0, 12)               # Set y-axis limits
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",    # Position legend at the bottom
    legend.title = element_blank() # Remove legend title for clarity
  )

# Plot mean position over time for different levels
ggplot(Grouped_data, aes(x = time_bin)) +
  geom_line(aes(y = mean_pos_1_scale, color = "Position 1")) +
  geom_line(aes(y = mean_pos_2_scale, color = "Position 2")) +
  geom_line(aes(y = mean_pos_3_scale, color = "Position 3")) +
  geom_line(aes(y = mean_pos_4_scale, color = "Position 4")) +
  facet_wrap(~ Folder, scales = "free_y")  +
  facet_wrap(~ Folder, scales = "free_y", ncol = 12, nrow = 10) +  # Adjust ncol and nrow as needed
  labs(
    title = "Mean position Over Time by Folder (Per Minute)", 
    x = "Time (Minutes)", 
    y = "Mean Position scaled",
    color = "Torque Variables"
  ) +
  scale_x_continuous(
    breaks = seq(0, 12, by = 2),  # Breaks every 0.5 minutes
    limits = c(0, 12)               # Set y-axis limits
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",    # Position legend at the bottom
    legend.title = element_blank() # Remove legend title for clarity
  )


# Create a new adjusted time column other perspective
all_data <- all_data %>%
  dplyr::group_by(Folder) %>%
  dplyr::mutate(adjusted_time_2 = time + lag(cumsum(max(time)), default = 0)) %>% 
  dplyr::ungroup()

# Define the time window size (e.g., 1 second)
time_window <- 1  # You can change this value to define the window size

# Group the data by Folder and time window, 
#then calculate the mean torque and position for each window, i scale all the variables due to the
# different magnitudes
aggregated_data <- all_data %>%
  dplyr::mutate(time_bin = floor(adjusted_time_2 / time_window) * time_window) %>%
  dplyr::group_by(Folder, time_bin) %>%
  dplyr::summarise(
    mean_torque_x = mean(torque_x, na.rm = TRUE),
    mean_torque_y = mean(torque_y, na.rm = TRUE),
    mean_torque_z = mean(torque_z, na.rm = TRUE),
    mean_torque_spindle = mean(torque_spindle, na.rm = TRUE),
    mean_pos_1 = mean(position_1, na.rm = TRUE),
    mean_pos_2 = mean(position_2, na.rm = TRUE),
    mean_pos_3 = mean(position_3, na.rm = TRUE),
    mean_pos_4 = mean(position_4, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    mean_torque_x_scale = scale(mean_torque_x),
    mean_torque_y_scale = scale(mean_torque_y),
    mean_torque_z_scale = scale(mean_torque_z),
    mean_torque_spindle_scale = scale(mean_torque_spindle),
    mean_pos_1_scale = mean(mean_pos_1, na.rm = TRUE),
    mean_pos_2_scale = scale(mean_pos_2),
    mean_pos_3_scale = scale(mean_pos_3),
    mean_pos_4_scale = scale(mean_pos_4))

# Plot mean torque over time for different levels
ggplot(aggregated_data, aes(x = time_bin)) +
  geom_line(aes(y = mean_torque_x_scale, color = "Torque x")) +
  geom_line(aes(y = mean_torque_y_scale, color = "Torque y")) +
  geom_line(aes(y = mean_torque_z_scale, color = "Torque z")) +
  geom_line(aes(y = mean_torque_spindle_scale, color = "Torque Cabezal")) +
  facet_wrap(~ Folder, scales = "free_y") +  # Compare across different levels
  labs(title = "Mean Torque Over Time across Folder (Level)", 
       x = "Time (s)", y = "Mean Torque (A)") +
  theme_minimal()


# Position over adjusted time
ggplot(aggregated_data, aes(x = time_bin)) +
  geom_line(aes(y = mean_pos_1_scale, color = "Position 1")) +
  geom_line(aes(y = mean_pos_2_scale, color = "Position 2")) +
  geom_line(aes(y = mean_pos_3_scale, color = "Position 3")) +
  geom_line(aes(y = mean_pos_4_scale, color = "Position 4")) +
  facet_wrap(~ Folder, scales = "free_y") +  # Compare across different levels
  labs(title = "Mean Position Over Time across Folder (Level)", 
       x = "Time (s)", y = "Mean Position (mm)") +
  theme_minimal()



# 1. Filter rows with available data for torque and position (non-missing values)
torque_data <- all_data %>%
  dplyr::select(torque_x, torque_y, torque_z, torque_spindle) %>%
  na.omit()

position_data <- all_data %>%
  dplyr::select(position_1, position_2, position_3, position_4) %>%
  na.omit()

# 2. Compute correlation matrices
cor_torque <- cor(torque_data)
cor_position <- cor(position_data)

# 3. Visualize the Correlation Heatmaps

# Torque Correlation Heatmap
ggcorrplot::ggcorrplot(
  cor_torque, lab = TRUE, 
  title = "Correlation Matrix of Torque Variables"
)

# Position Correlation Heatmap
ggcorrplot::ggcorrplot(
  cor_position, lab = TRUE, 
  title = "Correlation Matrix of Position Variables"
)

# density of spindle torque
dens_spindel <- ggplot(all_data, aes(x = torque_spindle)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Spindle Torque", x = "Spindle Torque") +
  theme_minimal()

# density of X
dens_x <-ggplot(all_data, aes(x = torque_x)) +
  geom_density(fill = "red", alpha = 0.7) +
  labs(title = "Distribution of X Torque", x = "X Torque") +
  theme_minimal()

# density of y
dens_y <-ggplot(all_data, aes(x = torque_y)) +
  geom_density(fill = "yellow", alpha = 0.7) +
  labs(title = "Distribution of Y Torque", x = "Y Torque") +
  theme_minimal()

# density of z
dens_z <-ggplot(all_data, aes(x = torque_z)) +
  geom_density(fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Z Torque", x = "Z Torque") +
  theme_minimal()

dens_spindel + dens_x +dens_y + dens_z

# Create three subsets based on time multiples (handled per file)

subset_008 <- all_data %>%
  filter(time %% 0.008 == 0)

subset_016 <- all_data %>%
  filter(time %% 0.016 == 0)

# Check the size of each subset to ensure correct filtering
cat("Subset sizes: \n")
cat("0.002s:", nrow(all_data), "\n")
cat("0.008s:", nrow(subset_008), "\n")
cat("0.016s:", nrow(subset_016), "\n")

# Function to perform PCA
perform_pca <- function(data) {
  pca_features <- data %>%
    select(torque_x, torque_y, torque_spindle) %>%
    na.omit()
  prcomp(pca_features, scale. = TRUE)
}

# Apply PCA to each subset
pca_all_data <- perform_pca(all_data)
pca_008 <- perform_pca(subset_008)
pca_016 <- perform_pca(subset_016)

# Loading plot for PCA on all_data
loading_all_data <- fviz_pca_var(pca_all_data,
                                 col.var = "contrib", # Color by contributions
                                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                 repel = TRUE) +  # Avoid label overlapping
  ggtitle("PCA Variables: 0.002s Granularity")

# Loading plot for PCA on subset_008
loading_008 <- fviz_pca_var(pca_008,
                            col.var = "contrib",
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE) +
  ggtitle("PCA Variables: 0.008s Granularity")

# Loading plot for PCA on subset_016
loading_016 <- fviz_pca_var(pca_016,
                            col.var = "contrib",
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE) +
  ggtitle("PCA Variables: 0.016s Granularity")

# Combine loading plots vertically
combined_loading_plots <- loading_all_data / loading_008 / loading_016

# Display the combined loading plots
print(combined_loading_plots)

# Contributions of variables to PC1
fviz_contrib(pca_all_data, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC1
fviz_contrib(pca_008, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC1
fviz_contrib(pca_016, choice = "var", axes = 1, top = 10)

# Function to calculate rolling variance within each file
calculate_rolling_variance <- function(data) {
  data %>%
    dplyr::group_by(Folder,File) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(rolling_var_spindle = zoo::rollapply(torque_spindle, 12, var, fill = NA)) %>%
    dplyr::ungroup()
}

# Apply rolling variance calculation for each subset
all_data <- calculate_rolling_variance(all_data)
subset_008 <- calculate_rolling_variance(subset_008)
subset_016 <- calculate_rolling_variance(subset_016)

# Function to plot anomalies in a given dataset
plot_anomalies <- function(data, title) {
  # Identify anomalies (top 5% rolling variance)
  anomalies <- data %>%
    dplyr::filter(rolling_var_spindle > quantile(rolling_var_spindle, 0.95, na.rm = TRUE))
  
  # Plot spindle torque with anomalies highlighted
  ggplot(data, aes(x = time, y = torque_spindle)) +
    geom_line(color = "black", alpha = 0.7) +
    geom_point(data = anomalies, aes(x = time, y = torque_spindle), color = "red", size = 1.5) +
    labs(title = title, x = "Time (s)", y = "Spindle Torque") +
    theme_minimal()
}

# Plot anomalies for each granularity
p1 <- plot_anomalies(all_data, "Anomaly Detection: 0.002s Granularity")
p2 <- plot_anomalies(subset_008, "Anomaly Detection: 0.008s Granularity")
p3 <- plot_anomalies(subset_016, "Anomaly Detection: 0.016s Granularity")

# Combine the three plots for comparison
p1 / p2 / p3

# Function to summarize anomalies
summarize_anomalies <- function(anomaly_data, granularity) {
  total <- nrow(anomaly_data)
  anomalies <- sum(anomaly_data$is_anomaly, na.rm = TRUE)
  proportion <- anomalies / total * 100  # Percentage
  
  data.frame(
    Granularity = granularity,
    Total_Observations = total,
    Number_of_Anomalies = anomalies,
    Proportion_of_Anomalies = proportion
  )
}

# Function to calculate anomalies
calculate_anomalies <- function(data) {
  data %>%
    dplyr::filter(!is.na(rolling_var_spindle)) %>%  # Ensure rolling_var_spindle is not NA
    dplyr::mutate(
      is_anomaly = rolling_var_spindle > quantile(rolling_var_spindle, 0.95, na.rm = TRUE)
    )
}

# Apply anomaly detection
all_data_anomalies <- calculate_anomalies(all_data)
subset_008_anomalies <- calculate_anomalies(subset_008)
subset_016_anomalies <- calculate_anomalies(subset_016)

# Summarize for each subset
summary_all_data <- summarize_anomalies(all_data_anomalies, "0.002s")
summary_008 <- summarize_anomalies(subset_008_anomalies, "0.008s")
summary_016 <- summarize_anomalies(subset_016_anomalies, "0.016s")

# Combine summaries into one data frame
anomaly_summary <- bind_rows(summary_all_data, summary_008, summary_016)
names(anomaly_summary)
# Plotting the proportion of anomalies
ggplot(anomaly_summary, aes(x = Granularity, y = Proportion_of_Anomalies, fill = Granularity)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Proportion_of_Anomalies, 1), "%")),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Proportion of Anomalies Across Different Time Granularities",
    x = "Time Granularity",
    y = "Proportion of Anomalies (%)"
  ) +
  ylim(0, max(anomaly_summary$Proportion_of_Anomalies) + .5) +  # Add some space above bars
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))

# 1. INTERSECT ANOMALIES

# Function to calculate rolling variance within each file
calculate_rolling_variance <- function(data, window_size = 12) {
  data %>%
    dplyr::group_by(Folder, File) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      rolling_var_spindle = rollapply(torque_spindle, width = window_size, FUN = var, fill = NA, align = "right")
    ) %>%
    dplyr::ungroup()
}

# Function to identify anomalies (top 5% rolling variance)
identify_anomalies <- function(data) {
  threshold <- quantile(data$rolling_var_spindle, 0.95, na.rm = TRUE)
  data %>%
    dplyr::filter(rolling_var_spindle > threshold) %>%
    dplyr::select(Folder, File, time)
}

# Function to calculate anomaly proportions
calculate_anomaly_proportion <- function(original_data, anomaly_data, granularity) {
  total <- nrow(original_data)
  anomalies <- nrow(anomaly_data)
  proportion <- (anomalies / total) * 100
  
  data.frame(
    Granularity = granularity,
    Total_Observations = total,
    Number_of_Anomalies = anomalies,
    Proportion_of_Anomalies = round(proportion, 2)
  )
}

# 2. Calculate rolling variance
all_data <- calculate_rolling_variance(all_data)
subset_008 <- calculate_rolling_variance(subset_008)
subset_016 <- calculate_rolling_variance(subset_016)

# 3. Identify anomalies
anomalies_all <- identify_anomalies(all_data)
anomalies_008 <- identify_anomalies(subset_008)
anomalies_016 <- identify_anomalies(subset_016)

# 4. Remove duplicates
anomalies_all_unique <- anomalies_all %>%
  dplyr::distinct(Folder, File, time)

anomalies_008_unique <- anomalies_008 %>%
  dplyr::distinct(Folder, File, time)

anomalies_016_unique <- anomalies_016 %>%
  dplyr::distinct(Folder, File, time)

# 5. Summarize anomalies
summary_all_data <- calculate_anomaly_proportion(all_data, anomalies_all_unique, "0.002s")
summary_008 <- calculate_anomaly_proportion(subset_008, anomalies_008_unique, "0.008s")
summary_016 <- calculate_anomaly_proportion(subset_016, anomalies_016_unique, "0.016s")

# Combine summaries
anomaly_summary <- bind_rows(summary_all_data, summary_008, summary_016)

# 6. Plot proportion of anomalies
proportion_plot <- ggplot(anomaly_summary, aes(x = Granularity, y = Proportion_of_Anomalies, fill = Granularity)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Proportion_of_Anomalies, "%")),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Proportion of Anomalies Across Different Time Granularities",
    x = "Time Granularity",
    y = "Proportion of Anomalies (%)"
  ) +
  ylim(0, max(anomaly_summary$Proportion_of_Anomalies) + 5) +  # Add space above bars
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# 7. Find common anomalies

# Common between All Data and Subset 008
common_all_008 <- inner_join(anomalies_all_unique, anomalies_008_unique,
                             by = c("Folder", "File", "time"))

# Common between All Data and Subset 016
common_all_016 <- inner_join(anomalies_all_unique, anomalies_016_unique,
                             by = c("Folder", "File", "time"))

# Common between Subset 008 and Subset 016
common_008_016 <- inner_join(anomalies_008_unique, anomalies_016_unique,
                             by = c("Folder", "File", "time"))

# Common across all three subsets
common_all_three <- inner_join(common_all_008, anomalies_016_unique,
                               by = c("Folder", "File", "time"))

# 8. Create overlap summary
overlap_summary <- data.frame(
  Comparison = c("All Data & Subset 008", "All Data & Subset 016", "Subset 008 & Subset 016", "All Three"),
  Number_of_Common_Anomalies = c(
    nrow(common_all_008),
    nrow(common_all_016),
    nrow(common_008_016),
    nrow(common_all_three)
  )
)

print(overlap_summary)

# Install and load VennDiagram package if not already installed
install.packages("VennDiagram")
library(VennDiagram)

# Define sets as unique identifiers (concatenated Folder_File_time)
set_all <- paste(anomalies_all_unique$Folder, anomalies_all_unique$File, anomalies_all_unique$time, sep = "_")
set_008 <- paste(anomalies_008_unique$Folder, anomalies_008_unique$File, anomalies_008_unique$time, sep = "_")
set_016 <- paste(anomalies_016_unique$Folder, anomalies_016_unique$File, anomalies_016_unique$time, sep = "_")

# Create Venn Diagram
venn.plot <- draw.triple.venn(
  area1 = length(set_all),
  area2 = length(set_008),
  area3 = length(set_016),
  n12 = length(intersect(set_all, set_008)),
  n13 = length(intersect(set_all, set_016)),
  n23 = length(intersect(set_008, set_016)),
  n123 = length(intersect(intersect(set_all, set_008), set_016)),
  category = c("All Data (0.002s)", "Subset 008 (0.008s)", "Subset 016 (0.016s)"),
  fill = c("skyblue", "pink1", "mediumorchid"),
  lty = "blank",
  cex = 2,
  cat.cex = 2,
  cat.pos = 0,
  cat.dist = 0.05
)
grid.draw(venn.plot)

# Install and load UpSetR package if not already installed
install.packages("UpSetR")
library(UpSetR)

# Prepare the list of sets
upset_data <- list(
  "All Data (0.002s)" = set_all,
  "Subset 008 (0.008s)" = set_008,
  "Subset 016 (0.016s)" = set_016
)

# Create UpSet plot
upset(fromList(upset_data), 
      order.by = "freq",
      mainbar.y.label = "Intersection Size",
      sets.x.label = "Set Size",
      keep.order = TRUE,
      sets.bar.color = "lightblue",
      main.bar.color = "steelblue",
      matrix.color = "steelblue",
      text.scale = c(2, 2, 2, 1.5, 2, 1.5))

summary(all_data)
summary(subset_008)
summary(subset_016)

# Load required libraries
library(ggplot2)
library(dplyr)
library(signal)
library(zoo)

# STEP 1: Adjust time at the folder level
# Ensure time is cumulative at the folder level
all_data <- all_data %>%
  arrange(Folder, File, time) %>% 
  group_by(Folder) %>%
  mutate(adjusted_time = time + lag(cumsum(time), default = 0)) %>%
  ungroup()

# Extract original torque spindle signal and adjusted time
original_signal <- all_data$torque_spindle[!is.na(all_data$torque_spindle)]  # Remove NA values
original_time <- all_data$adjusted_time[!is.na(all_data$torque_spindle)]
original_sampling_rate <- 1 / mean(diff(original_time))  # Original sampling rate in Hz

# STEP 2: Analyze frequency content of the original signal
# Perform FFT on the original signal
fft_original <- fft(original_signal)
n_original <- length(original_signal)
freqs_original <- seq(0, original_sampling_rate / 2, length.out = n_original / 2)
magnitude_original <- Mod(fft_original[1:(n_original / 2 + 1)])  # Magnitude of FFT

# Plot the frequency spectrum
ggplot(data.frame(Frequency = freqs_original, Magnitude = magnitude_original), aes(x = Frequency, y = Magnitude)) +
  geom_line(color = "blue") +
  labs(title = "Frequency Spectrum of Original Signal", x = "Frequency (Hz)", y = "Magnitude") +
  theme_minimal()

# Nyquist frequency calculation
nyquist_008 <- 1 / 0.008 / 2  # Nyquist frequency for .008s
nyquist_016 <- 1 / 0.016 / 2  # Nyquist frequency for .016s
cat("Nyquist Frequency for .008s:", nyquist_008, "Hz\n")
cat("Nyquist Frequency for .016s:", nyquist_016, "Hz\n")

# STEP 3: Prepare subsets with folder-level time
# Adjust subsets' time using cumulative time in each folder
subset_008 <- subset_008 %>%
  arrange(Folder, File, time) %>%
  group_by(Folder) %>%
  mutate(adjusted_time = time + lag(cumsum(time), default = 0)) %>%
  ungroup()

subset_016 <- subset_016 %>%
  arrange(Folder, File, time) %>%
  group_by(Folder) %>%
  mutate(adjusted_time = time + lag(cumsum(time), default = 0)) %>%
  ungroup()

# Extract subset signals and adjusted times
subset_008_signal <- subset_008$torque_spindle[!is.na(subset_008$torque_spindle)]
subset_008_time <- subset_008$adjusted_time[!is.na(subset_008$torque_spindle)]

subset_016_signal <- subset_016$torque_spindle[!is.na(subset_016$torque_spindle)]
subset_016_time <- subset_016$adjusted_time[!is.na(subset_016$torque_spindle)]

# STEP 4: Reconstruct the signal with respect to folder-level time
reconstruct_signal_with_time <- function(subset_signal, subset_time, original_time) {
  # Interpolate the subset signal onto the original time
  interpolated_signal <- approx(x = subset_time, y = subset_signal, xout = original_time, rule = 2)$y
  return(interpolated_signal)
}

# Reconstruct signals onto the original timeframe
reconstructed_008_signal <- reconstruct_signal_with_time(subset_008_signal, subset_008_time, original_time)
reconstructed_016_signal <- reconstruct_signal_with_time(subset_016_signal, subset_016_time, original_time)

# STEP 5: Validation

# Plot original vs reconstructed signals
plot(original_time, original_signal, type = "l", col = "red", xlab = "Time", ylab = "Torque Spindle",
     main = "Original vs Reconstructed Signals at Folder Level")
lines(original_time, reconstructed_008_signal, col = "blue")
lines(original_time, reconstructed_016_signal, col = "green")
legend("topright", legend = c("Original", "Reconstructed .008s", "Reconstructed .016s"),
       col = c("red", "blue", "green"), lty = 1)

# Zoomed plot for detailed validation
zoom_start <- 0
zoom_end <- 2
idx <- which(original_time >= zoom_start & original_time <= zoom_end)
plot(original_time[idx], original_signal[idx], type = "l", col = "red", xlab = "Time (Zoomed)", ylab = "Torque Spindle",
     main = "Zoomed Comparison at Folder Level")
lines(original_time[idx], reconstructed_008_signal[idx], col = "blue")
lines(original_time[idx], reconstructed_016_signal[idx], col = "green")

# Calculate RMSE for reconstructions
rmse <- function(original_signal, reconstructed_signal) {
  sqrt(mean((original_signal - reconstructed_signal)^2))
}
rmse_008 <- rmse(original_signal, reconstructed_008_signal)
rmse_016 <- rmse(original_signal, reconstructed_016_signal)

cat("RMSE for .008s Subset Reconstruction:", rmse_008, "\n")
cat("RMSE for .016s Subset Reconstruction:", rmse_016, "\n")

# STEP 6: Frequency domain validation
fft_reconstructed_008 <- fft(reconstructed_008_signal)
fft_reconstructed_016 <- fft(reconstructed_016_signal)

# Compute magnitudes
magnitude_reconstructed_008 <- Mod(fft_reconstructed_008[1:(n_original / 2 + 1)])
magnitude_reconstructed_016 <- Mod(fft_reconstructed_016[1:(n_original / 2 + 1)])

# Plot frequency spectrum comparison
ggplot() +
  geom_line(data = data.frame(Frequency = freqs_original, Magnitude = magnitude_original), aes(x = Frequency, y = Magnitude), color = "red") +
  geom_line(data = data.frame(Frequency = freqs_original, Magnitude = magnitude_reconstructed_008), aes(x = Frequency, y = Magnitude), color = "blue") +
  geom_line(data = data.frame(Frequency = freqs_original, Magnitude = magnitude_reconstructed_016), aes(x = Frequency, y = Magnitude), color = "green") +
  labs(title = "Frequency Spectrum Comparison at Folder Level", x = "Frequency (Hz)", y = "Magnitude") +
  theme_minimal()

# Correlation between original and reconstructed spectra
correlation_008 <- cor(magnitude_original, magnitude_reconstructed_008, use = "complete.obs")
correlation_016 <- cor(magnitude_original, magnitude_reconstructed_016, use = "complete.obs")

cat("Frequency Correlation for .008s Subset Reconstruction:", correlation_008, "\n")
cat("Frequency Correlation for .016s Subset Reconstruction:", correlation_016, "\n")

# Define the cutoff frequency in Hz
cutoff_freq <- 31.25  # Hz

# Calculate the Nyquist frequency
original_sampling_rate <- 1 / mean(diff(original_time))  # Ensure this is correct
nyquist_frequency <- original_sampling_rate / 2
cat("Nyquist Frequency:", nyquist_frequency, "Hz\n")

# Adjust the cutoff frequency if it exceeds Nyquist
if (cutoff_freq >= nyquist_frequency) {
  cutoff_freq <- 0.9 * nyquist_frequency  # Reduce cutoff to 90% of Nyquist frequency
  cat("Adjusted Cutoff Frequency:", cutoff_freq, "Hz\n")
}

# Normalize the cutoff frequency
normalized_cutoff <- cutoff_freq / nyquist_frequency
cat("Normalized Cutoff Frequency:", normalized_cutoff, "\n")

# Ensure the normalized cutoff is in range (0, 1)
if (normalized_cutoff <= 0 || normalized_cutoff >= 1) {
  stop("Normalized cutoff frequency must be in the range (0, 1).")
}

# Create a Butterworth filter
butter_filter <- butter(4, normalized_cutoff)

# Apply the filter to the original signal
filtered_signal <- filtfilt(butter_filter, original_signal)

# Apply the filter to the subsets
filtered_subset_008 <- filtfilt(butter_filter, subset_008_signal)
filtered_subset_016 <- filtfilt(butter_filter, subset_016_signal)

# Print confirmation
cat("Filtering complete for all signals.\n")


# Validate the filtered signals with updated reconstruction

zoom_start <- 5  # Example
zoom_end <- 6
idx <- which(original_time >= zoom_start & original_time <= zoom_end)
plot(original_time[idx], original_signal[idx], type = "l", col = "red", xlab = "Time", ylab = "Torque Spindle",
     main = "Zoomed Comparison: Critical Time Window")
lines(original_time[idx], reconstructed_008_signal[idx], col = "blue")
lines(original_time[idx], reconstructed_016_signal[idx], col = "green")

residual_008 <- original_signal - reconstructed_008_signal
residual_016 <- original_signal - reconstructed_016_signal
hist(residual_008, main = "Residuals for .008s Reconstruction", xlab = "Error", col = "blue")
hist(residual_016, main = "Residuals for .016s Reconstruction", xlab = "Error", col = "green")
plot(original_time, residual_008, type = "l", col = "blue", main = "Residuals over Time", ylab = "Residuals")
lines(original_time, residual_016, col = "green")
legend("topright", legend = c(".008s", ".016s"), col = c("blue", "green"), lty = 1)

mae <- function(original, reconstructed) mean(abs(original - reconstructed))
mae_008 <- mae(original_signal, reconstructed_008_signal)
mae_016 <- mae(original_signal, reconstructed_016_signal)
cat("MAE for .008s Subset Reconstruction:", mae_008, "\n")
cat("MAE for .016s Subset Reconstruction:", mae_016, "\n")


psnr <- function(original, reconstructed) {
  mse <- mean((original - reconstructed)^2)
  max_val <- max(original)
  10 * log10(max_val^2 / mse)
}
psnr_008 <- psnr(original_signal, reconstructed_008_signal)
psnr_016 <- psnr(original_signal, reconstructed_016_signal)
cat("PSNR for .008s Subset Reconstruction:", psnr_008, "\n")
cat("PSNR for .016s Subset Reconstruction:", psnr_016, "\n")







