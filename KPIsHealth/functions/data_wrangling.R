import_respondent_data <- function(file_name){
  
  respondent_data <- read_csv(file_name) %>% 
    separate(Date,c("Day","Month","Year"),sep="/") %>% 
    select(-Day)
  
  return(respondent_data)
} 

filter_respondent_data <- function(respondent_data, age, gender, region){
  
  if(!is.null(age) & !is.null(gender) & !is.null(region)) {
    filtered_data <- respondent_data %>%
      select(-id) %>% 
      filter(
        Age %in% age,
        Gender %in% gender,
        Region %in% region
      ) %>% 
      select(-c(Age,Gender,Region))
    
    return(filtered_data)
  } else {
    return()
  }
  
  
}

group_filtered_data <- function(filtered_data){
  
  if(!is.null(filtered_data)) {
    period_data <- filtered_data %>% 
      slice_rows(c("Month","Year")) %>% 
      by_slice(
        function(slice) cbind(
          t(apply(slice,2,mean)) %>% as_data_frame,
          data_frame(base=nrow(slice))
        ),
        .collate="rows"
      ) %>% 
      mutate(dates = dmy(paste("15",Month,Year,sep="/"))) %>% 
      arrange(dates) %>% 
      mutate(t = 1:nrow(.)) %>% 
      select(-c(Month,Year)) 
    
    return(period_data)
  } else {
    return()
  }
}

get_kpi_data <- function(period_data,kpi){
  
  if(!is.null(period_data)) {
    colnames(period_data)[which(colnames(period_data)==kpi)] <- "original"
    variable_data <- period_data %>% select(t,dates,original,base)
    return(variable_data)
  } else {
    return()
  }
  
}

delete_kpi_from_names <- function(names_vector) gsub("KPI - ","",names_vector)
