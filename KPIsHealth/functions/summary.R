plot_summary_data <- function(respondent_data, grouping_variables, compare_versus){
  
  summary_data <- summarize_grouped_data(
    respondent_data, 
    grouping_variables, 
    compare_versus
  )
  
  colnames(summary_data) <- delete_kpi_from_names(colnames(summary_data))
  
  print(summary_data)
  
  summary_plot <- datatable(
    summary_data,
    rownames = FALSE,
    filter = "top",
    class = "cell-border"
  ) %>% formatStyle(
    colnames(summary_data),
    backgroundColor = styleEqual(
      c("Decreasing", "Constant", "Growing"),
      c("#fd6b6b", "#f9fb60", "#7ff28d")
    )
  )
  return(summary_plot)
}

summarize_grouped_data <- function(respondent_data, grouping_variables, compare_versus){
  
  if (length(grouping_variables)){
    summary_data <- respondent_data %>%
      select(
        match(grouping_variables,colnames(.)),
        starts_with("KPI"),
        Month,Year
      ) %>% 
      slice_rows(grouping_variables) %>% 
      by_slice(
        get_kpis_lastest_change_color,
        compare_versus = compare_versus, 
        .collate="rows"
      )
  } else {
    summary_data <- respondent_data %>%
      select(
        match(grouping_variables,colnames(.)),
        starts_with("KPI"),
        Month,Year
      ) %>% 
      get_kpis_lastest_change_color(compare_versus)
  }
  
  return(summary_data)
}

get_kpis_lastest_change_color <- function(slice, compare_versus){
  
  period_data <- group_filtered_data(slice)
  kpis <- colnames(period_data)[startsWith(colnames(period_data),"KPI")]
  
  kpis_color <- do.call(
    "rbind",
    lapply(
      kpis,
      get_kpi_interval,
      period_data = period_data,
      compare_versus = compare_versus
    )
  ) %>% 
    select(color) %>%
    t %>% 
    color_traduction %>% 
    as_data_frame
  
  colnames(kpis_color) <- kpis

  return(kpis_color)
}
