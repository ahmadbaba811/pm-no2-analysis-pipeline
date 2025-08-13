# read library
library(readxl)
library(dplyr)
library(writexl)
library(lubridate)
library(ggplot2)
library(stringr)
library(openxlsx)
library(reshape2)
library(scales)
library(tidyr)
library(missForest)
library(patchwork)
library(purrr)
library(patchwork)
library(gridExtra)



# import excel files
file_path <- "C:/Cranfield Environmental Engineering 2023-2024/Thesis/Green Infrastructure/Sampling Data/ground0/"
working_file <- paste0(file_path, "Analysis-summer.R")
setwd(file_path)
pm_back_folder <- paste0(file_path, "PM/Back/")
pm_front_folder <- paste0(file_path, "PM/Front/")
no2_front_folder <- paste0(file_path, "NO2/Front/")
no2_back_folder <- paste0(file_path, "NO2/Back/")


# load all the files in the two folders
pm_back_files <- list.files(pm_back_folder, full.names = TRUE)
pm_front_files <- list.files(pm_front_folder, full.names = TRUE)
no2_front_files <- list.files(no2_front_folder, full.names = TRUE)
no2_back_files <- list.files(no2_back_folder, full.names = TRUE)

# empty list to save all combined data
df_front_and_back <- list();
df_no2_front_back <- list();
start_time <- "10:52:00"

loc_avg_Arr <- list();
## ==================================================================== ##
# CLEAN PM DATA
for (pm_back_file in pm_back_files) {
  # Extract the file name from folder 1
  pm_back_file_name <- basename(pm_back_file)
  
  # construct the corresponding file name in folder 2
  pm_front_file <- file.path(pm_front_folder, pm_back_file_name)
  
  # Check if the file exists in folder 2
  if (file.exists(pm_front_file)) {
    # Format the back measurement file
    df_pm_back <- readLines(pm_back_file)
    pm_back_start_time <- start_time; #strsplit(df_pm_back[7], ",")[[1]][2]
    pm_back_meas_date <- strsplit(df_pm_back[8], ",")[[1]][2]
    meas_date_time <- paste0(pm_back_meas_date, " ", pm_back_start_time)
    
    df_pm_back <- df_pm_back[37:length(df_pm_back)]
    writeLines(df_pm_back, "df_pm_back.csv")
    df_pm_back <- read.csv("df_pm_back.csv", header = TRUE)
    df_pm_back = df_pm_back %>% select(-Alarms, -Errors)
    df_pm_back$date_time <- meas_date_time
    file.remove("df_pm_back.csv")
    df_pm_back_column_names <- c("time", "pm1_b", "pm2.5_b", "pm4_b", "pm10_b", "pmtot_b", "date_time_b")
    colnames(df_pm_back) <- df_pm_back_column_names
    df_pm_back$date_time_b <- mdy_hms(df_pm_back$date_time_b) + seconds(df_pm_back$time)
    tb <- df_pm_back$date_time_b
    df_pm_back$time_b <- sprintf("%02d:%02d", hour(tb), minute(tb))
    df_pm_back$date_b <- as.Date(df_pm_back$date_time_b)
    df_pm_back$time_hour_b <- sprintf("%02d", hour(tb))
    df_pm_back$pm_coarse_b <- df_pm_back$pm10_b - df_pm_back$pm2.5_b
    
    
    
    # Format the front measurement file
    df_pm_front <- readLines(pm_front_file)
    df_pm_front <- df_pm_front[37:length(df_pm_front)]
    writeLines(df_pm_front, "df_pm_front.csv")
    df_pm_front <- read.csv("df_pm_front.csv", header = TRUE)
    df_pm_front = df_pm_front %>% select(-Alarms, -Errors)
    df_pm_front$date_time <- meas_date_time
    file.remove("df_pm_front.csv")
    df_pm_front_column_names <- c("time", "pm1_f", "pm2.5_f", "pm4_f", "pm10_f", "pmtot_f", "date_time_f")
    colnames(df_pm_front) <- df_pm_front_column_names
    df_pm_front$date_time_f <- mdy_hms(df_pm_front$date_time_f) + seconds(df_pm_front$time)
    tf <- df_pm_front$date_time_f
    df_pm_front$time_f <- sprintf("%02d:%02d", hour(tf), minute(tf))
    df_pm_front$date_f <- as.Date(df_pm_front$date_time_f)
    df_pm_front$time_hour_f <- sprintf("%02d", hour(tf))
    df_pm_front$pm_coarse_f <- df_pm_front$pm10_f - df_pm_front$pm2.5_f
    
    # Determine which data frame to subset based on the number of rows
    if (nrow(df_pm_back) > nrow(df_pm_front)) {
      df_pm_back_subset <- df_pm_back[1:nrow(df_pm_front), ]
      df_pm_back <- df_pm_back_subset
    } else {
      df_pm_front_subset <- df_pm_front[1:nrow(df_pm_back), ]
      df_pm_front <- df_pm_front_subset
    }
    ## Location averages
    b_pm2.5_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pm2.5_b")
    f_pm2.5_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pm2.5_f")
    b_coa_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pmcoa_b")
    f_coa_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pmcoa_f")
    
    b_pm1_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pm1_b")
    f_pm1_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pm1_f")
    b_pm10_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pm10_b")
    f_pm10_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pm10_f")
    b_pmtot_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pmtot_b")
    f_pmtot_col_name <- paste0(unlist(strsplit(as.character(pm_back_file_name), ".csv")), "_pmtot_f")
    
    
    df_loc_avg_b <- df_pm_back %>% select(time, pm2.5_b, pm_coarse_b, pm1_b, pm10_b, pmtot_b)
    df_loc_avg_f <- df_pm_front %>% select(time, pm2.5_f, pm_coarse_f, pm1_f, pm10_f, pmtot_f)
    df_loc_avg_b_cols <- c("time", b_pm2.5_col_name, b_coa_col_name, b_pm1_col_name, b_pm10_col_name, b_pmtot_col_name)
    df_loc_avg_f_cols <- c("time", f_pm2.5_col_name, f_coa_col_name, f_pm1_col_name, f_pm10_col_name, f_pmtot_col_name)
    colnames(df_loc_avg_b) <- df_loc_avg_b_cols
    colnames(df_loc_avg_f) <- df_loc_avg_f_cols
    df_loc_avg_merge <- inner_join(df_loc_avg_b, df_loc_avg_f, by = "time", relationship = "many-to-many")
    loc_avg_Arr[[pm_back_file]] <- df_loc_avg_merge
    
    # now merge df_pm_back and df_pm_front based on the date time column
    merged_df <- inner_join(df_pm_back, df_pm_front, by = "time", relationship = "many-to-many")
    merged_df$meas_location <- gsub(".csv", "", pm_back_file_name)
    
    # push merged file to the general list
    df_front_and_back[[pm_back_file]] <- merged_df
    
    # save merged excel file to folder
    output_file <- paste0("PM/", "merged_file_", basename(gsub(".csv", "", pm_back_file)), "_", basename(gsub(".csv", "", pm_front_file)), ".xlsx")
    write_xlsx(merged_df, output_file)
    
  }else{
    print(paste0("File ", pm_back_file_name, " does not exist in folder 2. Skipping.\n"))
  }
}



## ===================================================================== ##
## MERGE THE PM DATA
combined_df <- do.call(rbind, df_front_and_back)
combined_df <- combined_df %>% select(-date_time_b, -date_b, -time_b, -time_hour_b)
combined_df <- combined_df %>% rename(date_time = date_time_f, meas_date = date_f, meas_time = time_f, time_hour = time_hour_f )
combined_df$pmtot_per_ch <- round(((combined_df$pmtot_b - combined_df$pmtot_f)/combined_df$pmtot_f)*100, 2)

# convert values to milligram/cubic m
combined_df <- combined_df %>% mutate(across(contains("pm") & !contains("per_ch"), ~ .x * 1000))
## ======================================================================== ##



## CLEANING NO2 DATA
no2_files <- c(no2_front_files, no2_back_files)
no2_data_fb <- list()
for (f in no2_files) {
  no2_data <- readLines(f)
  writeLines(no2_data, "no2_data.csv")
  no2_data <- read.csv("no2_data.csv", header = TRUE)
  file.remove("no2_data.csv")
  no2_data$Date.Time <- as.POSIXct(no2_data$Date.Time, format="%d %b %Y %H:%M")
  no2_col_names <- c("meas_date_time", "monitor_id", "location_id", "no2")
  colnames(no2_data) <- no2_col_names
  tf <- no2_data$meas_date
  no2_data$meas_time <- sprintf("%02d:%02d", hour(tf), minute(tf))
  no2_data$meas_date <- as.Date(no2_data$meas_date)
  no2_data$time_hour <- sprintf("%02d", hour(tf))
  no2_data_fb[[f]] <- no2_data
}

distinct_date <- unique(combined_df$meas_date)
no2_front_data <- no2_data_fb[[1]]
no2_back_data <- no2_data_fb[[2]]


for (i in seq_along(distinct_date)) {
  d_date <- distinct_date[i]
  # filter the data frames based on the date
  meas_location <- combined_df %>% filter(meas_date == d_date)
  meas_location <- meas_location[1, 18]
  
  f_df_front <- no2_front_data %>% filter(meas_date == d_date)
  f_df_back <- no2_back_data %>% filter(meas_date == d_date)
  
  # Determine which data frame to subset based on the number of rows
  if (nrow(f_df_back) > nrow(f_df_front)) {
    f_df_back <- f_df_back[1:nrow(f_df_front), ]
  } else {
    f_df_front <- f_df_front[1:nrow(f_df_back), ]
  }
  f_df_back <- f_df_back %>% select(-location_id, -monitor_id)
  f_df_front <- f_df_front %>% select(-location_id, -monitor_id)
  
  colnames(f_df_back) <- c("date_time_b", "no2_b", "time", "meas_date_b", "time_hr_b")
  colnames(f_df_front) <- c("date_time_f", "no2_f", "time", "meas_date_f", "time_hr_f")
  
  # now merge front and back
  merged_no2_df <- inner_join(f_df_back, f_df_front, by = "time", relationship = "many-to-many")
  merged_no2_df$meas_location <- gsub(".csv", "", meas_location)

   date_corr <- as.POSIXct(paste0(d_date, "10:53:00"))
   merged_no2_df <- merged_no2_df %>% mutate(
                    date_time_b = date_corr + minutes(row_number() - 1),
                    date_time_f = date_corr + minutes(row_number() - 1)
                    )
   
   time_corr <- merged_no2_df$date_time_b
   merged_no2_df$time <- sprintf("%02d:%02d", hour(time_corr), minute(time_corr))
   merged_no2_df <- merged_no2_df %>%
     mutate(across(contains("no2_"), ~ .x *  1000 * 1.9125))
  
  # push merged file to the general list
  df_no2_front_back[[i]] <- merged_no2_df
  
  # save merged excel file to folder
  output_file <- paste0("NO2/", "merged_file_", meas_location, ".xlsx")
  write_xlsx(merged_no2_df, output_file)
}


### =====================================================================####
# FUNCTIONS


# merge files
merge_files <- function(combined_df, combined_no2_df){
  
  # daily average
  daily_avg <- daily_averages(combined_df, combined_no2_df)
  pm_daily_avg <- daily_avg[[1]]
  no2_daily_avg <- daily_avg[[2]]
  
  # hourly average
  hourly_average <- hourly_average(combined_df, combined_no2_df)
  pm_hourly_average <- hourly_average[[1]]
  no2_hourly_average <- hourly_average[[2]]
  
  ## save to excel file
  wb <- createWorkbook()
  
  # Add data frames to different sheets
  pm_combined_df <- combined_df %>% select(-meas_date, -time_hour)
  addWorksheet(wb, sheetName = "PM Combined")
  writeData(wb, sheet = "PM Combined", pm_combined_df, startCol = 1, startRow = 1)
  
  no2_combined_df <- combined_no2_df %>% select(-time_hr_b, -time_hr_f, -date_time_f, -meas_date_f)
  no2_combined_df_col <- c("date_time", "no2_b", "time", "meas_date", "no2_f", "meas_location")
  colnames(no2_combined_df) <- no2_combined_df_col
  addWorksheet(wb, sheetName = "NO2 Combined")
  writeData(wb, sheet = "NO2 Combined", no2_combined_df, startCol = 1, startRow = 1)
  
  addWorksheet(wb, sheetName = "PM daily_avg")
  writeData(wb, sheet = "PM daily_avg", pm_daily_avg, startCol = 1, startRow = 1)
  
  addWorksheet(wb, sheetName = "NO2 daily_avg")
  writeData(wb, sheet = "NO2 daily_avg", no2_daily_avg, startCol = 1, startRow = 1)
  
  addWorksheet(wb, sheetName = "PM hourly_average")
  writeData(wb, sheet = "PM hourly_average", pm_hourly_average, startCol = 1, startRow = 1)
  
  addWorksheet(wb, sheetName = "NO2 hourly_average")
  writeData(wb, sheet = "NO2 hourly_average", no2_hourly_average, startCol = 1, startRow = 1)
  
  # Save the workbook to a file
  saveWorkbook(wb, file = "summary_file.xlsx", overwrite = TRUE)
  print("file merge and data summary successful")
}

daily_averages <- function(comb_pm, comb_no2) {
  # Calculate daily average for each column

  pm_daily_avg_df <- comb_pm %>% 
    group_by(meas_date, meas_location) %>%
    summarize(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                             sd = ~sd(.x, na.rm = TRUE)), 
                     .names = "{.col}_{.fn}"))
  
  pm_daily_avg_df <- pm_daily_avg_df %>%
    mutate(across(contains("pm"), ~ round(.x, 2)))
  # 
  pm_daily_avg_df <- pm_daily_avg_df %>% select(-time_mean, -time_sd, -pmtot_per_ch_mean, -pmtot_per_ch_sd)
  # 
  pm_daily_avg_df$pmtot_per_ch <- ((pm_daily_avg_df$pmtot_b_mean - pm_daily_avg_df$pmtot_f_mean)/pm_daily_avg_df$pmtot_f_mean)*100
  pm_daily_avg_df$pmtot_per_ch <- round(pm_daily_avg_df$pmtot_per_ch, 2)
  
  no2_daily_avg_df <- comb_no2 %>% 
    group_by(meas_date_b, meas_location) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE))

  print("daily average computed")
  return(list(pm_daily_avg_df, no2_daily_avg_df))
}

hourly_average <- function(comb_pm, comb_no2) {
  pm_hourly_avg_df <- comb_pm %>% 
    group_by(meas_date, time_hour, meas_location) %>%
    summarize(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                             sd = ~sd(.x, na.rm = TRUE)), .names = "{.col}_{.fn}"))
  
  pm_hourly_avg_df <- pm_hourly_avg_df %>%
    mutate(across(contains("pm"), ~ round(.x, 2)))
  
  # pm_hourly_avg_df <- pm_hourly_avg_df %>% select(-time)
  pm_hourly_avg_df <- pm_hourly_avg_df %>% select(-time_mean, -time_sd, -pmtot_per_ch_mean, -pmtot_per_ch_sd)
  
  pm_hourly_avg_df$pmtot_per_ch <- ((pm_hourly_avg_df$pmtot_b_mean - pm_hourly_avg_df$pmtot_f_mean)/pm_hourly_avg_df$pmtot_f_mean)*100
  pm_hourly_avg_df$pmtot_per_ch <- round(pm_hourly_avg_df$pmtot_per_ch, 2)
  
  no2_hourly_avg_df <- comb_no2 %>% 
    group_by(meas_date_b, time_hr_b, meas_location) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE))
  
  print("hourly average computed")
  return(list(pm_hourly_avg_df, no2_hourly_avg_df))
}


### =============================================================== ###

# PROGRAM
combined_no2_df <- do.call(rbind, df_no2_front_back)
merge_files(combined_df, combined_no2_df);

## =======================================================================##

## PLOT GRAPHS

## PLOT PM ONLY
for (dd in df_front_and_back) {
  measurement_date <- dd$date_b[1]
  measurement_location <- dd$meas_location[1]
  
  dd <- dd %>%
    mutate(across(contains("pm"), ~ .x * 1000))
  
  # calculate % change in PM total and ratio
  dd$perc_change <- round(((dd$pmtot_b - dd$pmtot_f) / dd$pmtot_f) * 100, 2)
  dd$ratio <- round(dd$pmtot_f / dd$pmtot_b, 2)

    plot_pm_total <- ggplot(dd, aes(x = date_time_b)) +
    geom_line(aes(y = pmtot_b, color = "PMtotal(Back)")) +
    geom_line(aes(y = pmtot_f, color = "PMtotal(Front)")) +
    geom_line(aes(y= perc_change, color="PMtotal(% change)"))+
    labs(title = paste0("Time Series of PMtotal (Front vs Back)_", 
                        measurement_date, "_", measurement_location), 
         x = "Time(m)", y = "PM total (ug/m3)") +
    scale_color_manual(name = "Series", values = c("PMtotal(Back)" = "#1fc6cb", 
                                                   "PMtotal(Front)" = "#fa6b63", 
                                                   "PMtotal(% change)" = "grey"
    )) +
    theme_minimal()
    plot_pm_total
  ggsave(paste0("plots/", "PM", measurement_location,"_time series_", measurement_date, ".jpeg"), plot = plot_pm_total, width=7500, height = 3500, units = "px", dpi = 500)
  print("PM time series plot successful")
}


## PLOT PM AND NO2
for (i in seq_along(df_front_and_back)) {
  df <- df_front_and_back[[i]]
  filter_date <- df[1,9]
  meas_location <- df[1,22]
  pm_df <- df %>%
    mutate(across(contains("pm"), ~ .x * 1000))
  no2_df <- combined_no2_df %>% filter(meas_date_b == filter_date)

  pm <- pm_df %>% select(date_time_b, pmtot_f, pmtot_b)
  
  no2 <- no2_df %>% select(date_time_b, no2_f, no2_b)
  
  
  if (nrow(pm) > nrow(no2)) {
    pm <- pm[1:nrow(no2), ]
  } else {
    no2 <- no2[1:nrow(pm), ]
  }
 
  # Reshape PM data
  data_pm <- melt(pm, id.vars = "date_time_b", variable.name = "Location", value.name = "PM")
  data_pm$Variable <- "PM"
  
  # Reshape NO2 data
  data_no2 <- melt(no2, id.vars = "date_time_b", variable.name = "Location", value.name = "NO2")
  data_no2$Variable <- "NO2"
  
  # Rename Location for consistency
  data_pm$Location <- gsub("pmtot_", "", data_pm$Location)
  data_no2$Location <- gsub("no2_", "", data_no2$Location)
  
  # Format the time and replace to ensure same time in both data 
  data_no2$date_time_b <- with_tz(data_no2$date_time_b, tz = Sys.timezone())
  data_pm$date_time_b <- data_no2$date_time_b #with_tz(data_pm$date_time_b, tz = "Europe/London")
  
  
  # Combine PM and NO2 data
  data_combined <- merge(data_pm, data_no2, by = c("date_time_b", "Location", "Variable"), all = TRUE)
  data_combined$Location <- ifelse(data_combined$Location == "f", "Front", 
                                    ifelse(data_combined$Location == "b", "Back", data_combined$Location))

  # Plotting
  variable_labeller <- as_labeller(c(PM = "PM total (µg/m³)", NO2 = "NO2 (ppb)"))
  plot_pm_no2 <- ggplot(data_combined, aes(x = date_time_b)) +
    geom_line(aes(y = PM, color = Location), na.rm = TRUE) +
    geom_line(aes(y = NO2, color = Location), na.rm = TRUE) +
    facet_wrap(~ Variable, scales = "free_y", nrow = 2, labeller = variable_labeller) +
    labs(title = bquote("Time Series of PM"[total]*" and NO"[2]*" "*.(meas_location)),
         x = "Time(hr)", y = "Pollutant Concentration", color = "Location") +
    scale_x_datetime(
      breaks = date_breaks("1 hours"),  # Set breaks at 2-hour intervals
      labels = date_format("%H:%M")  # Format labels
    )+
    scale_color_manual(values = c("#1fc6cb", "#fa6b63")) +
    theme_minimal()
  ggsave(paste0("plots/", meas_location,"_time series_", filter_date, ".jpeg"), plot = plot_pm_no2, width=7500, height = 3500, units = "px", dpi = 500)
  print("pm and no2 plot successful")
  
}



## PLOT AFTER AVERAGE I.E BY LOCATION

## Prpare the data fro averages plot
daily_avg_df <- daily_averages(combined_df, combined_no2_df)
hourly_avg_df <- hourly_average(combined_df, combined_no2_df)

pm_daily_avg_df <- daily_avg_df[[1]]
no2_daily_avg_df <- daily_avg_df[[2]]
pm_hourly_avg_df <- hourly_avg_df[[1]]
no2_hourly_avg_df <- hourly_avg_df[[2]]

split_term <- readline(prompt = "Enter the split term or string for measurement Location names: ")
print(paste0("splitted file names with ", split_term))
# locs <- unlist(strsplit(as.character(pm_hourly_avg_df$meas_location), split_term))
# locs <- locs[!grepl(".csv", locs, fixed = TRUE)]
locs <- gsub("(L\\d+).*", "\\1", as.character(pm_hourly_avg_df$meas_location))
m_locs <- unique(locs)


## PLOT DAILY AVERAGE NO2 AND PM
for (i in seq_along(m_locs)) {
  m_loc <- m_locs[[i]]
  pm_df <- pm_daily_avg_df %>% filter(grepl(m_loc, meas_location))
  no2_df <- no2_daily_avg_df %>% filter(grepl(m_loc, meas_location))
  
  pm <- pm_df %>% select(meas_date, pmtot_f = pmtot_f_mean, pmtot_b =pmtot_b_mean)
  
  no2 <- no2_df %>% select(meas_date = meas_date_b, no2_f, no2_b)

  # Reshape PM data
  data_pm <- melt(pm, id.vars = "meas_date", variable.name = "Location", value.name = "PM")
  data_pm$Variable <- "PM"
  
  # Reshape NO2 data
  data_no2 <- melt(no2, id.vars = "meas_date", variable.name = "Location", value.name = "NO2")
  data_no2$Variable <- "NO2"
  
  # Rename Location for consistency
  data_pm$Location <- gsub("pmtot_", "", data_pm$Location)
  data_no2$Location <- gsub("no2_", "", data_no2$Location)
  

  # Combine PM and NO2 data
  data_combined <- merge(data_pm, data_no2, by = c("meas_date", "Location", "Variable"), all = TRUE)
  data_combined$Location <- ifelse(data_combined$Location == "f", "Front", 
                                   ifelse(data_combined$Location == "b", "Back", data_combined$Location))
  data_combined$meas_date <- as.Date(data_combined$meas_date)
  
  distinct_date <- unique(data_combined$meas_date)
  
  
  # Plotting
  variable_labeller <- as_labeller(c(PM = "PM (µg/m³)", NO2 = "NO2 (µg/m³)"))
  plot_pm_no2_avg <- ggplot(data_combined, aes(x = meas_date)) +
    geom_line(aes(y = PM, color = Location), na.rm = TRUE) +
    geom_point(aes(y = PM, color = Location), na.rm = TRUE, size = 2) +
    geom_line(aes(y = NO2, color = Location), na.rm = TRUE) +
    geom_point(aes(y = NO2, color = Location), na.rm = TRUE, size = 2) +
    facet_wrap(~ Variable, scales = "free_y", nrow = 2, labeller = variable_labeller) +
    labs(title = paste0("Time Series of Average PMtotal and NO2_", m_loc),
         x = "Date", y = "Pollutant Concentration", color = "Location") +
    scale_color_manual(values = c("#1fc6cb", "#fa6b63")) +
    theme_minimal()
  print(plot_pm_no2_avg)
  ggsave(paste0("plots/","daily average_", m_loc, ".jpeg"), plot = plot_pm_no2_avg, width=7500, height = 3500, units = "px", dpi = 500)
  print("pm and no2 daily avearage plot successful")
}


## PLOT HOURLY AVERAGE NO2 AND PM
for (i in seq_along(m_locs)) {
  m_loc <- m_locs[[i]]
  pm_df <- pm_hourly_avg_df %>% filter(grepl(m_loc, meas_location))
  no2_df <- no2_hourly_avg_df %>% filter(grepl(m_loc, meas_location))
  head(pm)
  pm <- pm_df %>% select(meas_date, time_hour, pmtot_f = pmtot_f_mean, pmtot_b =pmtot_b_mean)
  
  no2 <- no2_df %>% select(meas_date = meas_date_b, time_hour = time_hr_b, no2_f, no2_b)
  
  # Reshape PM data
  data_pm <- melt(pm, id.vars = c("meas_date", "time_hour"), variable.name = "Location", value.name = "PM")
  data_pm$Variable <- "PM (µg/m³)"

    # Reshape NO2 data
  data_no2 <- melt(no2, id.vars = c("meas_date", "time_hour"), variable.name = "Location", value.name = "NO2")
  data_no2$Variable <- "NO2 (µg/m³)"
  
  # Rename Location for consistency
  data_pm$Location <- gsub("pmtot_", "", data_pm$Location)
  data_no2$Location <- gsub("no2_", "", data_no2$Location)
  
  
  # Combine PM and NO2 data
  data_combined <- merge(data_pm, data_no2, by = c("meas_date", "time_hour", "Location", "Variable"), all = TRUE)
  data_combined$Location <- ifelse(data_combined$Location == "f", "Front", 
                                   ifelse(data_combined$Location == "b", "Back", data_combined$Location))
  data_combined$meas_date <- as.Date(data_combined$meas_date)
  distinct_date <- unique(data_combined$meas_date)

  
  # Plotting
  plot_pm_no2_hr_avg <- ggplot(data_combined, 
                               aes(x = time_hour, color = Location, linetype = Location, group = paste(Variable, Location))) +
    geom_line(aes(y = PM, color = Location), na.rm = TRUE) +
    geom_point(aes(y = PM, color = Location), na.rm = TRUE, size = 2) +
    geom_line(aes(y = NO2, color = Location), na.rm = TRUE) +
    geom_point(aes(y = NO2, color = Location), na.rm = TRUE, size = 2) +
    facet_grid(Variable ~ meas_date, scales = "free_y") +
    labs(title = paste0("PM and NO2 Concentration across ", m_loc),
         x = "Time(hr)", y = "Pollutant Concentration", color = "Location", linetype = "Location") +
    scale_color_manual(values = c("#1fc6cb", "#fa6b63")) +
    theme_minimal()+
    theme(legend.title = element_text(size = 16),
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16),
          axis.text.y = element_text(size = 16, colour = "black"),
          axis.text.x = element_text(size = 16, colour = "black"),
          strip.text = element_text(size = 16)
    )
  plot(plot_pm_no2_hr_avg)
  ggsave(paste0("plots/","hourly average_", m_loc, ".jpeg"), plot = plot_pm_no2_hr_avg, width=7500, height = 3500, units = "px", dpi = 500)
  print("pm and no2 hourly avearage plot successful")
}



## BOX PLOT AREA
## ================================================================ ==========##
merged_loc_avg_data <- Reduce(function(x, y) merge(x, y, by = "time", all = TRUE), loc_avg_Arr)

# Impute missing data using missForest
merged_loc_avg_data_rf <- missForest(merged_loc_avg_data)
merged_loc_avg_data <- merged_loc_avg_data_rf$ximp
print(merged_loc_avg_data_rf$OOBerror[[1]])
## Normalized root mean square error for random Forest = 1.798656e-06


# FIX TIME COLUMN
base_time <- as.POSIXct(start_time, format = "%H:%M:%S")
convert_to_time <- function(increment, base_time) {
  new_time <- base_time + increment
  format(new_time, "%H:%M:%S")
}
merged_loc_avg_data$new_time <- sapply(merged_loc_avg_data$time, convert_to_time, base_time = base_time)
# replace negative values with 0
merged_loc_avg_data[merged_loc_avg_data < 0] = 0
output_file <- paste0("merged_location_data_", ".xlsx")
write_xlsx(merged_loc_avg_data, output_file)

## PERFORM ROW WISE AVG
loc_averages <- merged_loc_avg_data %>% select(time, new_time)
row_avg_list <- list();
stations <- c("f", "b")
pms <- c("pm2.5_", "pmcoa_", "pm1_", "pm10_", "pmtot_")
for (i in seq_along(m_locs)) {
  loc <- m_locs[i]
  loc_cols <- grep(loc, names(merged_loc_avg_data), value = TRUE)
  loc_data <- merged_loc_avg_data[, loc_cols]
  for (s in stations) {
    sta_cols <- grep(s, names(loc_data), value = TRUE)
    sta_data <- loc_data[, sta_cols]
    for (p in pms) {
      pm_cols <- grep(p, names(sta_data), value = TRUE)
      pm_data <- sta_data[, pm_cols]
      row_avg <- rowMeans(pm_data, na.rm = TRUE)
      col_name <- paste(loc, p, s, sep = "_")
      loc_averages[[col_name]] <- row_avg
    }
  }
}

loc_averages <- loc_averages %>%
  mutate(across(contains("L"), ~ round(.x, 3)))

loc_averages <- loc_averages %>% mutate(across(contains("L"), ~ .x * 1000))

loc_averages <- loc_averages %>%
  mutate(across(contains("L"), ~ ifelse(. > 15, 0, .)))

View(loc_averages)
output_file <- paste0("pm_location_avg2", ".xlsx")
write_xlsx(loc_averages, output_file)

## ============================================= =============================##
## plot all box plot in one graph
loc_avg_to_plot <- loc_averages %>% select(-time, -new_time)
df_long <- loc_avg_to_plot %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(position = if_else(str_detect(variable, "_f$"), "front", "back"),
    variable = str_remove(variable, "_[fb]$")) %>% 
  arrange(variable, position)

output_file <- paste0("all", "_all_location_avg_data_", ".xlsx")
write_xlsx(df_long, output_file)

# Create box plot
loc_box_plot <- ggplot(df_long, aes(x = variable, y = value, fill = position)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 2, 
               color = "black", position = position_dodge(width = 0.75))+
  scale_fill_manual(values = c(front="#fa6b63", back="#1fc6cb")) +
  labs(y = expression(PM * (mu * g / m^3)), x = "PM sizes based on location - front(f) and back(b)") +
  theme_minimal() +
  theme(legend.title = element_blank())
loc_box_plot
ggsave(paste0("plots/","PM_all","_location summary", ".jpeg"), plot = loc_box_plot, width=7500, height = 3500, units = "px", dpi = 500)
## ============================================= =============================##


rename_columns <- function(name) {
  str_replace(name, "_pm[^_]+_", "_")
}

## PLOT IN LOOP ==================================== =========================##
plots_arr <- list();
pms2 <- c("pm2.5_", "pmcoa_", "pm1_", "pm10_")
for (p in pms2) {
  loc_cols <- grep(p, names(loc_averages), value = TRUE)
  pollutant_avg_by_loc <- loc_averages[, loc_cols]
  head(pollutant_avg_by_loc)
  pollutant_avg_by_loc <- pollutant_avg_by_loc %>%
    rename_with(rename_columns)
  
  loc_avg_to_plot <- pollutant_avg_by_loc
  df_long <- loc_avg_to_plot %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value" ) %>%
    mutate(position = if_else(str_detect(variable, "_f$"), "front", "back"),
      variable = str_remove(variable, "_[fb]$")) %>%
    arrange(variable, position)

  # Create box plot
  loc_box_plot <- ggplot(df_long, aes(x = variable, y = value, fill = position)) +
    geom_boxplot(outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 4, size = 2, 
                 color = "black", position = position_dodge(width = 0.25))+
    scale_fill_manual(values = c(front="#fa6b63", back="#1fc6cb")) +
    labs(y = bquote(PM[.(gsub("pm", "", p))] * (mu * g / m^3)), x = "") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(size = 14, colour = "black")
    )
  plots_arr[[p]] <- loc_box_plot;
  
  ggsave(paste0("plots/",p,"_location summary", ".jpeg"), plot = loc_box_plot, width=4000, height = 2500, units = "px", dpi = 500)
  print(paste0("PM",p, "_box plot saved"))
}

box_plot1 <- plots_arr[[1]]
box_plot2 <- plots_arr[[2]]
box_plot3 <- plots_arr[[3]]
box_plot4 <- plots_arr[[4]]



## =========================================================
no2_box_plot_data <- combined_no2_df %>% select(no2_b, no2_f, meas_location)
df_long_no2 <- no2_box_plot_data %>%
  pivot_longer(cols = starts_with("no2"), names_to = "variable", values_to = "value" ) %>%
  mutate(position = if_else(str_detect(variable, "_f$"), "front", "back"),
         variable = str_remove(variable, "_[fb]$")) %>%
  arrange(variable, position)
df_long_no2 <- df_long_no2 %>% select(-variable)
colnames(df_long_no2) <- c("variable", "value", "location")
df_long_no2$variable <- str_remove(df_long_no2$variable, "M\\d+")


no2_loc_box_plot <- ggplot(df_long_no2, aes(x = variable, y = value, fill = location)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 2, 
               color = "black", position = position_dodge(width = 0.25))+
  scale_fill_manual(values = c(front="#fa6b63", back="#1fc6cb")) +
  labs(y = "NO2 (µg/m³)", x = "") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black")
  )


ggsave(paste0("plots/","NO2_location summary", ".jpeg"), plot = no2_loc_box_plot, width=4000, height = 2500, units = "px", dpi = 500)
print(paste0("NO2",p, "_box plot saved"))


combined_plot <- box_plot1  +box_plot3 + box_plot4 +no2_loc_box_plot +plot_layout(ncol = 2)  +
  plot_annotation(title = "PM and NO2 across the three locations")
ggsave(paste0("plots/", "PM_combined_location summary", ".jpeg"), plot = combined_plot, width=7000, height = 4500, units = "px", dpi = 500)
print(paste0("PM",p, "_box plot saved"))





#=============
## PLOT AFTER AVERAGE I.E BY LOCATION
pm_no2_plots_arr <- list()

new_loc_avg <- loc_averages;
names(new_loc_avg) <- gsub("__", "_", names(new_loc_avg))

new_loc_avg$meas_date <- "2024-06-12"
new_loc_avg <- new_loc_avg %>%
  mutate(date_time = paste0(meas_date, " ", new_time))


L1_pm_df <-  new_loc_avg %>% select(date_time, L1_pm2.5_f, L1_pm2.5_b)
L2_pm_df <-  new_loc_avg %>% select(date_time, L2_pm2.5_f, L2_pm2.5_b)
L3_pm_df <-  new_loc_avg %>% select(date_time, L3_pm2.5_f, L3_pm2.5_b)
colnames(L1_pm_df) <- c("date_time", "pm2.5_f", "pm_2.5_b")
colnames(L2_pm_df) <- c("date_time", "pm2.5_f", "pm_2.5_b")
colnames(L3_pm_df) <- c("date_time", "pm2.5_f", "pm_2.5_b")
pm_df <- list(L1_pm_df, L2_pm_df, L3_pm_df)

no2_df_raw <- read.csv(paste0(file_path, "/NO2/NO2_averages.csv"))
no2_df_raw$meas_date <- "2024-06-12"
no2_df_raw <- no2_df_raw %>%
  mutate(date_time = paste0(meas_date, " ", new_time))

L1_NO2_df <-  no2_df_raw %>% select(date_time, L1_back, L1_front)
L2_NO2_df <-  no2_df_raw %>% select(date_time, L2_back, L2_front)
L3_NO2_df <-  no2_df_raw %>% select(date_time, L3_back, L3_front)
colnames(L1_NO2_df) <- c("date_time", "NO2_b", "NO2_f")
colnames(L2_NO2_df) <- c("date_time", "NO2_b", "NO2_f")
colnames(L3_NO2_df) <- c("date_time", "NO2_b", "NO2_f")
no2_df <- list(L1_NO2_df, L2_NO2_df, L3_NO2_df)

locs <- c("L1", "L2", "L3")

for (i in seq_along(pm_df)) {
  p_df <- pm_df[[i]]
  no_df <- no2_df[[i]]
  no_df <- no_df %>% mutate(across(contains("NO"), ~ .x * 1000 * 1.9125))
  m_loc <- locs[[i]]
  pm <- p_df %>% select(date_time, pm2.5_f, pm_2.5_b)
  no2 <- no_df %>% select(date_time, NO2_f, NO2_b)
  
  if (nrow(pm) > nrow(no2)) {
    pm <- pm[1:nrow(no2), ]
  } else {
    no2 <- no2[1:nrow(pm), ]
  }
  
  # Reshape PM data
  data_pm <- melt(pm, id.vars = "date_time", variable.name = "Location", value.name = "PM")
  data_pm$Variable <- "PM"
  
  # Reshape NO2 data
  data_no2 <- melt(no2, id.vars = "date_time", variable.name = "Location", value.name = "NO2")
  data_no2$Variable <- "NO2"
  
  # Rename Location for consistency
  data_pm$Location <- gsub("pm2.5_", "", data_pm$Location)
  data_no2$Location <- gsub("no2_", "", data_no2$Location)
  
  # Format the time and replace to ensure same time in both data 
  data_no2$date_time <- with_tz(data_no2$date_time, tz = Sys.timezone())
  data_pm$date_time <- data_no2$date_time #with_tz(data_pm$date_time_b, tz = "Europe/London")
  
  
  # Combine PM and NO2 data
  data_combined <- merge(data_pm, data_no2, by = c("date_time", "Location", "Variable"), all = TRUE)
  data_combined$Location <- ifelse(grepl("f", data_combined$Location), "front",
                                   ifelse(grepl("b", data_combined$Location), "back", data_combined$Location))
  # Plotting
  variable_labeller <- as_labeller(c(PM = "PM 2.5 (µg/m³)", NO2 = "NO2 (µg/m³)"))
  plot_pm_no2 <- ggplot(data_combined, aes(x = date_time)) +
    geom_line(aes(y = PM, color = Location), na.rm = TRUE) +
    geom_line(aes(y = NO2, color = Location), na.rm = TRUE) +
    facet_wrap(~ Variable, scales = "free_y", nrow = 2, labeller = variable_labeller) +
    labs(title = bquote("Time Series of PM"[2.5]*" and NO"[2]*" "*.(m_loc)),
         x = "Time(hr)", y = "Pollutant Concentration", color = "Location") +
    scale_x_datetime(
      breaks = date_breaks("1 hours"),  # Set breaks at 2-hour intervals
      labels = date_format("%H:%M")  # Format labels
    )+
    scale_color_manual(values = c("#1fc6cb", "#fa6b63")) +
    theme_minimal()
  pm_no2_plots_arr[[i]] <- plot_pm_no2
  print(plot_pm_no2)
  ggsave(paste0("plots/", m_loc,"_avg_time series_", filter_date, ".jpeg"), plot = plot_pm_no2, width=7500, height = 3500, units = "px", dpi = 500)

}

# calculate percentage changes by location
cc <- new_loc_avg %>% select (-time, -new_time, -meas_date, -date_time)
summ <- colMeans(cc, na.rm = TRUE)
d <- new_loc_avg

d$L1_per_change_pm2.5 <- ((d$L1_pm2.5_b - d$L1_pm2.5_f)/d$L1_pm2.5_f)*100
d$L2_per_change_pm2.5 <- ((d$L2_pm2.5_b - d$L2_pm2.5_f)/d$L2_pm2.5_f)*100
d$L3_per_change_pm2.5 <- ((d$L3_pm2.5_b - d$L3_pm2.5_f)/d$L3_pm2.5_f)*100

n <- no2_df_raw
n$L1_per_change_no2 <- ((n$L1_back - n$L1_front)/n$L1_front)*100
n$L2_per_change_no2 <- ((n$L2_back - n$L2_front)/n$L2_front)*100
n$L3_per_change_no2 <- ((n$L3_back - n$L3_front)/n$L3_front)*100

output_file <- paste0("pm_per_change_data", ".xlsx")
write_xlsx(d, output_file)
output_file <- paste0("no2_per_change_data", ".xlsx")
write_xlsx(n, output_file)


# go to excel file and remove Inf and arrange the data for plotting
per_change_df <- read_excel(paste0(file_path, "/Manual Analysis/per_change_pm_no2.xlsx"))

plot_NO2 <- ggplot(per_change_df[per_change_df$pollutant == "NO2", ], 
                   aes(x = meas_loc, y = perc_change, fill = meas_loc)) +
  geom_bar(stat = "identity") +
  ggtitle("% Change in NO2") +
  theme_minimal() +
  theme(
    legend.position = "none"
    
  )+
  ylab("% Change") +
  xlab("Location")

# Plot for PM
plot_PM <- ggplot(per_change_df[per_change_df$pollutant == "PM2.5", ], 
                  aes(x = meas_loc, y = perc_change, fill = meas_loc)) +
  geom_bar(stat = "identity") +
  ggtitle("% Change in PM2.5") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(), # Remove y-axis label
  )+
  xlab("Location")

# Arrange the plots side by side
perc_change_plot <- grid.arrange(plot_NO2, plot_PM, ncol = 2)
ggsave(paste0("plots/", "pecentage_change_plot", ".jpeg"), plot = perc_change_plot, width=4000, height = 3500, units = "px", dpi = 500)


## save combined plots
combined_plot <- reduce(pm_no2_plots_arr, `+`) + perc_change_plot + plot_layout(ncol = 2)
ggsave(paste0("plots/", "PM_NO2_time_series_combined", ".jpeg"), plot = combined_plot, width=7500, height = 4000, units = "px", dpi = 500)



# Load the data
data_ffp <- loc_averages %>% select(-time, -new_time)
data_nfp <- read.csv(paste0(file_path, "/Manual Analysis/no2_location_avg.csv"))

# Reshape the NO2 data into long format
data_long_nfp <- data_nfp %>%
  pivot_longer(cols = everything(),
               names_to = c("Location", "Pollutant", "Direction"),
               names_pattern = "(L[1-3])_(no2)_([fb])",
               values_to = "Value") %>%
  drop_na() # Remove NA values


# Reshape the pm data into long format
data_long_ffp <- data_ffp %>%
  pivot_longer(cols = everything(),
               names_to = c("Location", "Pollutant", "Direction"),
               names_pattern = "(L[1-3])_(pm[0-9\\.]+|pmtot|pmcoa)__([fb])",
               values_to = "Value") %>%
  drop_na() # Remove NA values

data_long_nfp <- data_long_nfp %>%   mutate(across(where(is.numeric), ~ . / 4))

combined_long_df <- bind_rows(data_long_ffp, data_long_nfp)

custom_labeller <- function(variable, value) {
  if (variable == "Pollutant") {
    value <- as.character(value)  # Ensure value is character
    value[value == "no2"] <- "NO2 (scale=1:4)"  # Replace "no2" with custom text
  }
  return(value)
}

ffp_plot_3 <- ggplot(combined_long_df, aes(x = factor(interaction(Location, Direction), 
                levels = c("L1.f", "L1.b", "L2.f", "L2.b", "L3.f", "L3.b")),  
                y = Value, fill = Location)) +
  geom_boxplot(width = 0.6) +
  stat_summary(
    fun = mean,  # Function to calculate mean
    geom = "point",  # Plot the mean as a point
    shape = 23,  # Use a diamond shape (or choose another)
    size = 1.5,  # Size of the mean point
    color = "yellow",  # Color of the mean point
    fill = "yellow"  # Fill color for the mean point
  ) +
  facet_wrap(~ Pollutant, scales = "free_x", labeller = custom_labeller) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    panel.grid.major = element_line(color = "grey85", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    panel.background = element_rect(fill = "grey95", color = NA),
    strip.background = element_rect(fill = "grey80", color = "black"),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, colour = "black"),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "",
       x = "Location and Direction [f=front, b=back]",
       y = "PM concentration (µg/m³)") +
  scale_fill_manual(values = c("#1fc6cb", "#a9d192", "#fae9ca")) +
  stat_boxplot(geom = "errorbar", width = 0.2)
ffp_plot_3
ggsave(paste0("plots/", "box_plot_all_pm", ".jpeg"), plot = ffp_plot_3, width=7500, height = 5000, units = "px", dpi = 500)




## ============================

# Load the data
data_nfp <- read.csv(paste0(file_path, "/Manual Analysis/no2_location_avg.csv"))

# Reshape the data into long format
data_long_nfp <- data_nfp %>%
  pivot_longer(cols = everything(),
               names_to = c("Location", "Pollutant", "Direction"),
               names_pattern = "(L[1-3])_(no2)_([fb])",
               values_to = "Value") %>%
  drop_na() # Remove NA values

nfp_plot_3 <- ggplot(data_long_nfp, aes(x = factor(interaction(Location, Direction), 
                                                   levels = c("L1.f", "L1.b", "L2.f", "L2.b", "L3.f", "L3.b")),  
                                        y = Value, fill = Location)) +
  geom_boxplot(width = 0.6) +
  
  geom_boxplot(width = 0.6) +
  facet_wrap(~ Location, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    panel.grid.major = element_line(color = "grey85", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    panel.background = element_rect(fill = "grey95", color = NA),
    strip.background = element_rect(fill = "grey80", color = "black"),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, colour = "black"),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "",
       x = "Location and Direction [f=front, b=back]",
       y = "NO2 concentration (µg/m³)") +
  scale_fill_manual(values = c("#1fc6cb", "#a9d192", "#fae9ca")) +
  stat_boxplot(geom = "errorbar", width = 0.2)
nfp_plot_3
ggsave(paste0("plots/", "box_plot_all_no2", ".jpeg"), plot = nfp_plot_3, width=7500, height = 4000, units = "px", dpi = 500)
