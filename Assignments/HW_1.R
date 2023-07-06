# Setup
{
  library(tidyverse)
  library(fpp3)
  library(jsonlite)
  
  eia.key <- "cb6b389405d665805266fd673992edc9"
  
  
  fn_query_eia <- function(
    the_series_id, the_source = "steo", the_frequency = "monthly", 
    the_offset = 0, the_length = 5000, the_eia_key = eia.key){
    
    the_url = "https://api.eia.gov/v2/"
    
    # Query must be no more than 5,000
    if(the_length > 5000) break
    
    get_call <- paste0(the_url, the_source, "/data/?", paste(
      paste0("frequency=", the_frequency), 
      "data[0]=value", 
      paste0("facets[seriesId][]=", the_series_id), 
      "sort[0][column]=period", 
      "sort[0][direction]=desc", 
      paste0("offset=", the_offset), 
      paste0("length=", the_length),
      sep = "&"
    ))
    
    eia_list <- fromJSON(str_c(get_call, "&api_key=", the_eia_key))
    
    eia_data <- eia_list$response$data
    
    eia_data %>% 
      as_tibble() %>% 
      return()
  }
}

# Data Prep
{
  # Get 5 years of data
  {
    data <- fn_query_eia("HVEPGEN_NW") %>% 
      mutate(
        period = ym(period) %>% yearmonth(.)
      ) %>% 
      filter(year(period) < 2023) %>% 
      tsibble()
  }
  
  # Train/Test Split
  {
    train <- data %>% 
      filter(year(period) < 2022)# %>% 
      filter(year(period) >= 2018)
    
    test <- data %>% 
      filter(year(period) == 2022)
  }
  
  # Other Data
  {
    # Grand Coulee Gen
    {
      gc.url <- "https://api.eia.gov/v2/electricity/facility-fuel/data/?frequency=monthly&data[0]=generation&facets[state][]=WA&facets[plantCode][]=6163&facets[fuel2002][]=ALL&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
      
      eia_list <- gc.url %>% 
        str_c(., "&api_key=", eia.key) %>% 
        fromJSON()
      GET
      eia_data <- eia_list$response$data
      
      gc.gen <- eia_data %>% 
        as_tibble() %>% 
        mutate(
          period = ym(period) %>% yearmonth(.)
        ) %>% 
        filter(year(period) < 2023) %>% 
        tsibble()
      
    }
  }
}

# Data Exploration
{
  # Time Plot
  train %>% 
    autoplot(value) +
    theme_bw() +
    ggtitle("Northwest hydroelectric net generation") +
    ylab("billion kilowatthours")
  
  # Seasonal Plot
  train %>% 
    gg_season(value, labels = "both") +
    theme_bw() +
    ggtitle("Seasonal Plot: Northwest hydroelectric net generation") +
    ylab("billion kilowatthours")
  
  # Seasonal Subseries Plot
  train %>% 
    gg_subseries(value) +
    theme_bw() +
    ggtitle("Seasonal Plot: Northwest hydroelectric net generation") +
    ylab("billion kilowatthours")
  
  # Lag Plot
  train %>% 
    gg_lag(value, geom = "point", lags = 1:12) +
    theme_bw() +
    ggtitle("Lag Plot: Northwest hydroelectric net generation") +
    ylab("billion kilowatthours")
  
  # Autocorrelation
  train %>% 
    ACF(value) %>% 
    autoplot() +
    theme_bw() +
    ggtitle("Autocorrelation Plot: Northwest hydroelectric net generation")
  
  # Other
  {
    gc.gen %>% 
      filter(year(period) >= 2010) %>% 
      autoplot(generation)
  }
}

# Decomposition
{
  data %>% 
    model(
      # STL(box_cox(value, .5) ~ trend(window = 39) + season(window = 13), robust = TRUE)
      STL(value ~ trend(window = 73) + season(window = 13), robust = TRUE)
    ) %>% 
    components() %>% 
    autoplot() +
    theme_bw()
}

# Model Estimation
{
  # Naive
  {
    train %>% 
      model(
        
      )
  }
  
  # SNAIVE
  {
    
  }
  
  # ETS
  {
    
  }
}

# Forecast
{
  # Predict Test data
  {
    
  }
  
  # Assess & Compare Prediction Errors
  {
    
  }
}