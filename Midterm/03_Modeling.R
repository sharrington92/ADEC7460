# Setup
{
  library(tidyverse)
  library(fpp3)
  
  data <- read_csv("Midterm/Columbia River Flows.csv") %>% 
    mutate(
      date = as.Date(datetime) %>% yearmonth(),
      year = year(date),
      month = month(date)
    ) %>% 
    group_by(date, site_name) %>% 
    summarize(flow = mean(flow_cfs), gage_feet = mean(gage_feet)) %>% 
    tsibble()
}

# Preliminary
{
  # Use 80% for training set, 20% set
  test <- data %>% 
    slice_max(n = 365, order_by = date)
  
  train <- data %>% 
    anti_join(y = test, by = "date")
}


# Model Estimation
{
  # build three models + one ensemble model (average of the three forecasts) for the last year
  fit <- train %>% 
    model(
      "ets" = ETS(flow),
      "arima" = ARIMA(flow),
      "stl" = STL(flow ~ trend ),
      "ensemble" = ets
    )
}


# Validation
{
  # You will estimate appropriate model statistics on the with-held 20% test set. 
  
  # You will compare these statistics across the models and suggest the best. 
  
  
}


