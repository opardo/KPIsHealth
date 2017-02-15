get_frequency  <- function(data_periods){
  if(data_periods == "months"){
    return(12)
  } else if (data_periods == "weeks"){
    return(52)
  } else if (data_periods == "years"){
    return(1)
  }
}

# import_time_series <- function(file_name){
#   time_series <- read_csv(file_name) %>%
#     na.omit %>% 
#     mutate(
#       dates = dmy(dates),
#       t = 1:nrow(.)
#     )
#   return(time_series)
# }
