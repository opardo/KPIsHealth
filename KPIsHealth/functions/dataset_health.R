get_brand_health_map <- function(period_data, frequency, breakpoints_criteria){
  
  if (!is.null(period_data)) {
    raw_data <- period_data %>% select(-c(base,dates,t))
    
    level <- lapply(
      colnames(raw_data),
      get_level,
      period_data = period_data,
      frequency = frequency
    )
    
    momentum <- lapply(
      colnames(raw_data),
      get_momentum,
      period_data = period_data,
      frequency = frequency,
      breakpoints_criteria = breakpoints_criteria
    )
    
    brand_health <- data_frame(
      brand = delete_kpi_from_names(colnames(raw_data)),
      level =  unlist(level),
      momentum = unlist(momentum)
    )
    
    brand_health_plot <- get_brand_health_plot(brand_health)
    
    return(brand_health_plot)
  } else {
    return(NULL)
  }
}

inverse_quantile <- function(column, value){
  return(sum(column <= value) / length(column))
}

get_level <- function(kpi, period_data, frequency){
  
  time_series <- get_kpi_data(period_data,kpi) 

  time_series <- add_kalman_filter(time_series,frequency)
  
  levels <- time_series$kalman
  
  level <- inverse_quantile(levels,levels[length(levels)])
  
  return(level)
}

get_momentum <- function(kpi, period_data, frequency, breakpoints_criteria){
  
  time_series <- get_kpi_data(period_data,kpi) %>% 
    add_kalman_filter(frequency)
  
  breakpoints <- search_breakpoints(time_series, breakpoints_criteria)
  
  last_trend <- time_series %>% 
    capture_linear_trend(
      breakpoints,
      length(breakpoints)+1
    ) %>% 
    use_series(trend) %>% 
    diff %>% 
    mean %>% 
    as.numeric
  
  series_range <- range(time_series$original)[2] - range(time_series$original)[1]
  
  if (abs(last_trend) < (series_range / 2)) {
    if (last_trend  > 0) {
      momentum <- 0.5 + 0.5 * sqrt(last_trend / (series_range / 2))
    } else {
      momentum <- 0.5 - 0.5 * sqrt(abs(last_trend) / (series_range / 2))
    }
  } else {
    momentum <- 1
  }
  
  return(momentum)
}

get_brand_health_plot <- function(brand_health){
  brand_health_plot <- ggplot(data = brand_health, aes(x = level, y = momentum)) +
    xlim(0,1) +
    ylim(0,1) +
    theme(
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_text(size=17),
      axis.title.y=element_text(size=17),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank()
    ) +
    geom_rect(
      aes(xmin=0.5, xmax=1, ymin=0.5, ymax=1),
      alpha=0.1,
      fill="green"
    ) +
    geom_rect(
      aes(xmin=0, xmax=0.5, ymin=0.5, ymax=1),
      alpha=0.1,
      fill="yellow"
    ) +
    geom_rect(
      aes(xmin=0.5, xmax=1, ymin=0, ymax=0.5),
      alpha=0.1,
      fill="orange"
    ) +
    geom_rect(
      aes(xmin=0, xmax=0.5, ymin=0, ymax=0.5),
      alpha=0.1,
      fill="red"
    ) +
    geom_vline(xintercept = 0.5) +
    geom_hline(yintercept = 0.5) +
    geom_point(
      size = 3,
      shape = 15
    ) +
    geom_label_repel(
      aes(
        label=brand,
        fontface = 'bold'
      )
    )
  
  return(brand_health_plot)
}

column_differences <- function(column){
  diff_column <- vector()
  for(i in 2:(length(column))){
    diff_column[i] <- column[i] - column[i-1]
  }
  return(diff_column)
}

# get_brand_health_map <- function(period_data){
#   
#   raw_data <- period_data %>% select(-c(base,dates,t))
#   differences <- apply(raw_data,2,column_differences) %>% as_data_frame
#   
#   level <- lapply(1:ncol(raw_data),function(i) get_level(
#     raw_data[[i]],
#     as.numeric(raw_data[nrow(raw_data),i])
#   ))
#   
#   momentum <- lapply(1:ncol(raw_data),function(i) get_momentum(
#     differences[[i]],
#     as.numeric(differences[nrow(differences),i])
#   ))
#   
#   brand_health <- data_frame(
#     brand = names(raw_data),
#     level =  unlist(level),
#     momentum = unlist(momentum)
#   )
#   
#   brand_health_plot <- get_brand_health_plot(brand_health)
#   
#   return(brand_health_plot)
# }
# 
# get_momentum <- function(column,value){
#   
#   positives <- column[which(column > 0)]
#   negatives <- column[which(column < 0)]
#   
#   if(value==0){
#     return(0.5)
#   } else if (value > 0){
#     return(0.5 + inverse_quantile(positives,value)/2)
#   } else {
#     return(0.5 - inverse_quantile(negatives,value)/2)
#   }
# }
