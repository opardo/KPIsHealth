library(forecast)
library(tibble)
library(lubridate)
library(dplyr)
library(strucchange)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(ggrepel)
library(R.utils)
library(dlm)
library(DT)
library(gridExtra)
library(magrittr)

setwd("C:\\Users\\PardoO\\Documents\\Projects\\TimeSeries\\EWS\\EWS")

function_files <- sourceDirectory("functions")
for (file in function_files){
  source(file, local = TRUE)
}

data_periods <- "months"
brand <- "brand1"
kpi <- "KPI - METRIC1"
noise_filter <- "kalman"
compare_versus <- "last_quarter"
breakpoints_criteria <- "kalman_series"
age <- c("21-30")
gender <- c("Male")
region <- c("East","West")
comparison_type <- "differences"

grouping_variables <- c("Region","Gender")

frequency <- get_frequency(data_periods)
file_name <- paste("data/",brand,".csv", sep="")
respondent_data <- import_respondent_data(file_name)
filtered_data  <- filter_respondent_data(respondent_data,age,gender,region)
period_data  <- group_filtered_data(filtered_data)

# SUMMARY
summary_data <- plot_summary_data(respondent_data, grouping_variables, compare_versus)
summary_data

# DOING

brand_health <- get_brand_health_map(period_data, frequency, breakpoints_criteria)
brand_latest_change_plot <- get_brand_latest_change(period_data, compare_versus)
brand_health
brand_latest_change_plot

# TREND

time_series <- get_kpi_data(period_data,kpi)
latest_change <- plot_latest_change(time_series,comparison_type,compare_versus)
mean_base <- ceiling(mean(time_series$base))
time_series <- add_kalman_filter(time_series,frequency)
breakpoints <- search_breakpoints(time_series,breakpoints_criteria)
trends <- calculate_trends(time_series,breakpoints)
forecast <- calculate_forecast(time_series,frequency)
ts_plot <- plot_time_series(time_series,forecast,noise_filter,breakpoints,trends)
ts_plot

