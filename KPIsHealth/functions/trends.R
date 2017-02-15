get_time_series_plot <- function(time_series, frequency, breakpoints_criteria, noise_filter){
  
  if (!is.null(time_series)) {
    time_series <- add_kalman_filter(time_series,frequency)
    breakpoints <- search_breakpoints(time_series,breakpoints_criteria)
    trends <- calculate_trends(time_series,breakpoints)
    forecast <- calculate_forecast(time_series,frequency)
    ts_plot <- plot_time_series(time_series,forecast,noise_filter,breakpoints,trends)
    return(ts_plot)
  } else {
    return()
  }
  
}

add_kalman_filter <- function(time_series,frequency){
  
  if (!is.null(time_series)) {
    time_series_object <- ts(
      time_series$original,
      start = c(year(time_series$dates)[1],month(time_series$dates)[1]),
      frequency = frequency
    )
    
    kalman_object <- kalman_filter(
      time_series_object,
      var(time_series$original)
    )
    
    kalman_mean <- kalman_object$smooth
    kalman_upper <- kalman_object$smooth + 1.96 * sqrt(kalman_object$variance_smooth)
    kalman_lower <- kalman_object$smooth - 1.96 * sqrt(kalman_object$variance_smooth)
    
    time_series <- time_series %>%
      mutate(
        kalman = kalman_mean,
        kalman_upper = kalman_upper,
        kalman_lower = kalman_lower
      )
    return(time_series)
  } else {
    return()
  }
}

search_breakpoints <- function(time_series,breakpoints_criteria){
  if(breakpoints_criteria=="kalman_series"){
    breakpoints <- breakpoints(kalman ~ 1, data = time_series)$breakpoints
  } else if(breakpoints_criteria=="original_series"){
    breakpoints <- breakpoints(original ~ t, data = time_series)$breakpoints
  }
  return(breakpoints)
}

calculate_trends <- function(time_series,breakpoints){
  trend_numbers <- 1:(length(breakpoints)+1)
  trends <- do.call(
    "rbind",
    lapply(
      trend_numbers,
      capture_linear_trend,
      time_series=time_series,
      breakpoints=breakpoints
    )
  )
  return(trends)
}

calculate_forecast <- function(time_series,frequency){
  forecast_periods <- ceiling(log(nrow(time_series)))
  
  kalman_series_object <- ts(
    time_series$kalman,
    start = c(year(time_series$dates)[1],month(time_series$dates)[1]),
    frequency = frequency
  )
  
  forecast_model <- forecast(Arima(kalman_series_object,order=c(0,2,1)))
  last_mean <- time_series$kalman[nrow(time_series)]
  last_upper <- time_series$kalman_upper[nrow(time_series)]
  last_lower <- time_series$kalman_lower[nrow(time_series)]
  last_interval <- last_upper - last_mean
  
  forecast <- data_frame(
    dates = seq(
      time_series$dates[nrow(time_series)],
      time_series$dates[nrow(time_series)] %m+% months(forecast_periods),
      by = "months"
    ),
    mean = c(last_mean, as.numeric(forecast_model$mean[1:forecast_periods])),
    lower = c(
      last_lower,
      as.numeric(forecast_model$lower[1:forecast_periods,2]) - last_interval
    ),
    upper = c(
      last_upper,
      as.numeric(forecast_model$upper[1:forecast_periods,2]) + last_interval
    )
  )
  
  return(forecast)
}

plot_time_series <- function(time_series,forecast,noise_filter,breakpoints,trends){
  if (noise_filter == "kalman"){
    time_series_plot <- plot_time_series_kalman(time_series,forecast,breakpoints)
  } else if (noise_filter == "trends"){
    time_series_plot <- plot_time_series_linear_trends(time_series,forecast,breakpoints,trends)
  } else {
    stop("Invalid noise_filter to plot")
  }
  return(time_series_plot)
}

plot_time_series_kalman <- function(time_series,forecast,breakpoints){
  
  kalman_time_series_plot <- ggplot()+
    geom_line(
      aes(x=dates, y=original, group=1),
      data=time_series,
      color="#BBBBBB"
    ) +
    geom_line(
      aes(x=dates, y=kalman),
      data=time_series,
      color="#073980",
      size=1
    ) +
    geom_ribbon(
      aes(x=dates,ymin=kalman_lower,ymax=kalman_upper),
      data=time_series,
      alpha=0.1
    ) +
    geom_vline(
      aes(xintercept = as.numeric(time_series$dates[breakpoints])),
      size=0.5,
      color="#555555"
    ) +
    geom_ribbon(
      aes(x=dates,ymin=lower,ymax=upper),
      data=forecast,
      alpha=0.3,
      fill="#e6bd37"
    ) +
    geom_line(
      aes(x=dates, y=mean),
      data=forecast,
      color="#073980",
      size=1,
      linetype=2
    ) +
    xlab("") + ylab("") +
    theme_light() +
    scale_y_continuous(labels = scales::percent)
  
  return(kalman_time_series_plot)
}

plot_time_series_linear_trends <- function(time_series,forecast,breakpoints,trends){
  
  linear_trends_time_series_plot <- ggplot() +
    aes(x = time_series$dates, group= 1) +
    geom_line(
      aes(x=dates, y=original, group=1),
      data=time_series,
      color = "#BBBBBB"
    ) + 
    geom_line(
      aes(x=dates, y = trend, group=trend_number),
      data=trends,
      color = "#073980",
      size = 1) + 
    geom_ribbon(
      aes(x=dates,ymin=kalman_lower,ymax=kalman_upper),
      data=time_series,
      alpha=0.1
    ) +
    geom_vline(
      aes(xintercept = as.numeric(time_series$dates[breakpoints])),
      size=0.5,
      color="#555555"
    ) +
    geom_ribbon(
      aes(x=dates,ymin=lower,ymax=upper),
      data=forecast,
      alpha=0.3,
      fill="#e6bd37"
    ) +
    geom_line(
      aes(x=dates, y=mean),
      data=forecast,
      color="#073980",
      size=1,
      linetype=2
    ) +
    xlab("") + ylab("") +
    theme_light()+
    scale_y_continuous(labels = scales::percent)
  
  return(linear_trends_time_series_plot)
}

kalman_filter <- function(y, variance = var(y)){
  
  buildRandomWalk <- function(v) {
    dW <- exp(v[1])
    m0 <- v[2]
    dlmModPoly(order = 1, dV = variance, dW = dW, m0 = m0)
  }
  
  varGuess <- var(diff(y), na.rm = TRUE)
  mu0Guess <- as.numeric(y[1])
  
  parm <- c(log(varGuess), mu0Guess)
  mle <- dlmMLE(y, parm = parm, buildRandomWalk)
  model <- buildRandomWalk(mle$par)
  
  model_smooth <-  dlmSmooth(y, model)
  model_filter <-  dlmFilter(y, model)
  
  result <- list(
    smooth = model_smooth$s[-1],
    filter =  model_filter$f,
    variance_smooth =  as.numeric(dlmSvd2var(model_smooth$U.S, model_smooth$D.S)[-1]),
    variance_filter =  as.numeric(dlmSvd2var(model_filter$U.R, model_filter$D.R))
  )
  
  return(result)
}

capture_linear_trend <- function(time_series,breakpoints,trend_number){
  if (trend_number == 1){
    initial <- 1
    final <- breakpoints[trend_number]
  } else if (trend_number > length(breakpoints)){
    initial <- breakpoints[trend_number-1]
    final <- nrow(time_series)
  } else {
    initial <- breakpoints[trend_number-1]
    final <- breakpoints[trend_number]
  }
  trend <- predict(lm(
    original ~ t,
    time_series[initial:final,]
  ))
  trend_df <- data_frame(
    dates = time_series$dates[initial:final],
    trend = as.numeric(trend),
    trend_number = trend_number
  )
  return(trend_df)
}
