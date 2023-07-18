# Setup
{
  library(tidyverse)
  library(fpp3)
  
  data <- read_csv("Midterm/Columbia River Flows.csv") %>% 
    mutate(
      date = as.Date(datetime),
      year = year(date),
      month = month(date)
    )
}

# Preliminary
{
  # Use 80% for training set, 20% set
  
  
}


# Model Estimation
{
  # build three models + one ensemble model (average of the three forecasts) for the last year
  
}


# Validation
{
  # You will estimate appropriate model statistics on the with-held 20% test set. 
  
  # You will compare these statistics across the models and suggest the best. 
  
  
}


