get_brand_latest_change <- function(period_data, compare_versus){
  
  if (!is.null(period_data)) {
    kpis <- colnames(period_data)[startsWith(colnames(period_data),"KPI")]
    
    kpis_interval <- do.call(
      "rbind",
      lapply(
        kpis,
        get_kpi_interval,
        period_data = period_data,
        compare_versus = compare_versus
      )
    ) %>% 
      cbind(kpi = delete_kpi_from_names(kpis),.) %>% 
      mutate(limit = max(limit))
    
    difference_plots <- apply(
      kpis_interval,
      MARGIN = 1,
      function(row) get_difference_plot(row, title = TRUE)
    )
    
    brand_latest_change_plot <- do.call("grid.arrange", c(difference_plots))
    
    return(brand_latest_change_plot)
  } else {
    return()
  }
}

get_kpi_interval <- function(kpi, period_data, compare_versus){
  
  time_series <- get_kpi_data(period_data,kpi)
  kpi_interval <- get_difference_interval(time_series, compare_versus)
  
  return(kpi_interval)
}

get_difference_interval <- function(time_series, compare_versus){
  
  last_period <- nrow(time_series)
  period_to_compare <- last_period - compare_versus_traduction(compare_versus)
  
  compared_points <- time_series[c(period_to_compare,last_period),] %>% 
    mutate(var = (original * (1-original)) / base) %>% 
    select(original,var)
  
  mean <- compared_points$original[2] - compared_points$original[1]
  sd <- sqrt(compared_points$var[2] + compared_points$var[1])
  lower <- mean - 1.96 * sd 
  upper <- mean + 1.96 * sd
  limit <- 2 * max(abs(lower),abs(upper))
  color <- ifelse(lower > 0, "green", ifelse(upper < 0, "red", "yellow"))
  
  kpi_interval <- data_frame(
    lower = lower,
    mean = mean,
    upper = upper,
    sd = sd,
    limit = limit,
    color = color
  )
  
  return(kpi_interval)
}

color_traduction <- function(color){
  ifelse(
    color == "red",
    "Decreasing",
    ifelse(
      color == "green",
      "Growing",
      ifelse(
        color == "yellow",
        "Constant",
        color
      )
    )
  )
}

plot_latest_change <- function(time_series, comparison_type, compare_versus){
  
  if(!is.null(time_series)) {
    if (comparison_type == "differences"){
      latest_change_plot <- difference_distribution(time_series, compare_versus)
    } else if (comparison_type == "two_points") {
      latest_change_plot <- two_points_distribution(time_series, compare_versus)
    } else {
      stop("Invalid latest change criteria")
    }
    return(latest_change_plot)
  } else {
    return()
  }
}

difference_distribution <- function(time_series, compare_versus){
  
  plot_parameters <- get_difference_interval(time_series, compare_versus)
  
  difference_plot <- get_difference_plot(plot_parameters)
  
  return(difference_plot)
}

two_points_distribution <- function(time_series, compare_versus){
  
  last_period <- nrow(time_series)
  period_to_compare <- last_period - compare_versus_traduction(compare_versus)
  
  compared_points <- time_series[c(period_to_compare,last_period),] %>% 
    mutate(
      var = (original * (1-original)) / base,
      sd = sqrt(var),
      upper = original + 1.645 * sd,
      lower = original - 1.645 * sd
    ) %>% 
    rename(mean = original) %>% 
    select(mean,sd,upper,lower)
  
  plot_parameters <- list()
  plot_parameters[["point1"]] <- unlist(compared_points[1,])
  plot_parameters[["point2"]] <- unlist(compared_points[2,])
  plot_parameters[["compared_points"]] <- compared_points
  
  last_two_points_plot <- get_two_points_plot(plot_parameters)
  
  return(last_two_points_plot)
}

get_difference_plot <- function(plot_parameters, title = FALSE){

  mean <- as.numeric(plot_parameters[["mean"]])
  sd <- as.numeric(plot_parameters[["sd"]])
  lower <- as.numeric(plot_parameters[["lower"]])
  upper <- as.numeric(plot_parameters[["upper"]])
  limit <- as.numeric(plot_parameters[["limit"]])
  color <- plot_parameters[["color"]]
  
  difference_plot <- ggplot(
    data_frame(x=c(-limit,limit)),
    aes(x)
  ) +
    theme(
      axis.line=element_blank(),
      axis.text.x=element_text(size=9,colour="black"),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank()
    ) +
    scale_x_continuous(
      limits = c(-limit,limit),
      breaks = c(
        lower,
        upper
      ),
      labels = c(
        paste(round(lower*100,1),"%",sep=""),
        paste(round(upper*100,1),"%",sep="")
      )
    ) +
    geom_area(
      stat = "function",
      fun = dnorm,
      fill = color,
      args = list(mean=mean,sd=sd),
      alpha = 0.4,
      linetype="solid",
      color = "gray"
    ) +
    geom_area(
      stat = "function",
      fun = dnorm,
      fill = color,
      args = list(mean=mean,sd=sd),
      alpha = 0.5,
      linetype="solid",
      color = "gray",
      xlim = c(lower,upper)
    ) +
    geom_vline(linetype="dashed",xintercept = 0) +
    geom_point(
      data = data_frame(
        x = c(lower,upper),
        y = c(0,0)
      ),
      size = 3,
      aes(x=x,y=y),
      show.legend = TRUE
    ) +
    geom_point(
      data = data_frame(
        x = mean,
        y = dnorm(mean,mean=mean,sd=sd)
      ),
      size = 3,
      aes(x=x,y=y),
      show.legend = TRUE
    ) +
    geom_label_repel(
      data = data_frame(
        x = mean,
        y = dnorm(mean,mean=mean,sd=sd)
      ),
      aes(
        x=x,
        y=y,
        label=paste(round(x*100,1),"%"),
        fontface = 'bold'
      )
    )
  
  if (title){
    difference_plot <- difference_plot +
      ggtitle(toString(plot_parameters[["kpi"]])) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  }
  
  return(difference_plot)
}

get_two_points_plot <- function(plot_parameters){
  
  point1 <- plot_parameters[["point1"]]
  point2 <- plot_parameters[["point2"]]
  compared_points <- plot_parameters[["compared_points"]]
  
  two_points_plot <- ggplot(
    data_frame(x=c(0.9*min(compared_points$lower),1.1*max(compared_points$upper))),
    aes(x)
  ) +
    theme(
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank()
    ) +
    geom_area(
      stat = "function",
      fun = dnorm,
      fill = "blue",
      args = list(mean=point1[["mean"]],sd=point1[["sd"]]),
      alpha = 0.25,
      linetype="solid",
      color = "gray"
    ) +
    geom_area(
      stat = "function",
      fun = dnorm,
      fill = "blue",
      args = list(mean=point1[["mean"]],sd=point1[["sd"]]),
      alpha = 0.35,
      xlim = c(point1[["lower"]],point1[["upper"]]),
      linetype="solid",
      color = "gray"
    ) +
    geom_area(
      stat = "function",
      fun = dnorm,
      fill = "green",
      args = list(mean=point2[["mean"]],sd=point2[["sd"]]),
      alpha = 0.25,
      linetype="solid",
      color = "gray"
    ) +
    geom_area(
      stat = "function",
      fun = dnorm,
      fill = "green",
      args = list(mean=point2[["mean"]],sd=point2[["sd"]]),
      alpha = 0.35,
      xlim = c(point2[["lower"]],point2[["upper"]]),
      linetype="solid",
      color = "gray"
    ) +
    geom_point(
      data = data_frame(
        x = c(point1[["mean"]],point2[["mean"]]),
        y = c(
          dnorm(point1[["mean"]],mean = point1[["mean"]],sd = point1[["sd"]]),
          dnorm(point2[["mean"]],mean = point2[["mean"]],sd = point2[["sd"]])
        )
      ),
      size = 3,
      aes(x=x,y=y),
      show.legend = TRUE
    ) +
    geom_label_repel(
      data = data_frame(
        x = c(point1[["mean"]],point2[["mean"]]),
        y = c(
          dnorm(point1[["mean"]],mean = point1[["mean"]],sd = point1[["sd"]]),
          dnorm(point2[["mean"]],mean = point2[["mean"]],sd = point2[["sd"]])
        )
      ),
      aes(x=x,y=y,label=paste(round(x*100,1),"%")),
      label.size = 0.1
    ) +
    scale_y_continuous(expand = c(0.3, 0.3))
  
  return(two_points_plot)
}

compare_versus_traduction <- function(compare_versus){
  if(compare_versus == "last_month"){
    return(1)
  } else if (compare_versus == "last_quarter"){
    return(3)
  } else if (compare_versus == "last_year"){
    return(12)
  } else {
    stop("Invalid 'compare_versus'")
  }
}
