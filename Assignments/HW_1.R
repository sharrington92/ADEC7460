# Setup ----
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

# Data Prep ----
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
      # filter(year(period) >= 2018)
    
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
      
      eia_data <- eia_list$response$data
      
      gc.gen <- eia_data %>% 
        as_tibble() %>% 
        mutate(
          period = ym(period) %>% yearmonth(.)
        ) %>% 
        filter(year(period) < 2023) %>% 
        tsibble()
      
    }
    
    # Hydroelectric Capacity
    {
      eia_data <- list()
      
      
      for(i in 1:ceiling(82334 / 5000)){
        
        url.cap <- paste0(
          "https://api.eia.gov/v2/electricity/operating-generator-capacity/data/?",
          "frequency=monthly&data[0]=nameplate-capacity-mw&",
          "data[1]=net-summer-capacity-mw&data[2]=net-winter-capacity-mw&data[3]=planned-derate-summer-cap-mw&data[4]=planned-uprate-summer-cap-mw&",
          "facets[stateid][]=ID&facets[stateid][]=OR&facets[stateid][]=WA&",
          "facets[energy_source_code][]=WAT&",
          "facets[energy_source_code][]=WAT&facets[sector][]=electric-utility&facets[sector][]=industrial-chp&",
          "start=2010-01&end=2022-12&",
          "sort[0][column]=period&",
          "sort[0][direction]=desc&",
          "offset=", (i - 1)*5000,
          "&length=5000"
        )
        
        
        eia_list <- url.cap %>% 
          str_c(., "&api_key=", eia.key) %>%
          fromJSON()
        
        eia_data[[i]] <- eia_list$response$data
        
      }
      
      hydro.cap <- eia_data %>% 
        do.call(bind_rows, .) %>% 
        as_tibble() %>% 
        mutate(
          period = ym(period) %>% yearmonth(.)
        ) %>% 
        filter(year(period) < 2023) %>% 
        group_by(period) %>% 
        summarize(across(
          c(
            `nameplate-capacity-mw`, `net-summer-capacity-mw`, `net-winter-capacity-mw`, 
            `planned-derate-summer-cap-mw`, `planned-uprate-summer-cap-mw`
          ),
          \(x){sum(x, na.rm = T)}
        )) %>% 
        tsibble()
      
        

    }
  }
  
  # Save data
  {
    list(
      data, hydro.cap, gc.gen
    ) %>% 
      saveRDS(file.path("Assignments", "Week 1", "Data.RDS"))
  }
}

# Data Exploration ----
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
    geom_point() +
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
    
    
    hydro.cap %>% 
      autoplot(`net-winter-capacity-mw`) +
      theme_bw() +
      geom_smooth(span = 1)
  }
}

# Decomposition ----
{
  # Guerrero lambda
  lambda <- data %>% 
    features(value, features = guerrero) %>% 
    pull(lambda_guerrero)
  
  # Trend captures water-year
  data %>% 
    model(
      STL(box_cox(value, lambda) ~ trend(window = 19) + season(window = 21), robust = TRUE)
    ) %>% 
    components() %>% 
    autoplot() +
    theme_bw()
  
  hydro.cap %>% 
    model(
      STL(`net-winter-capacity-mw` ~ trend(window = 21) + season(window = 13), robust = TRUE)
    ) %>% 
    components() %>% 
    autoplot() +
    theme_bw()
}

# Model Estimation ----
{
  fit <- train %>% 
    model(
      # Naive
      "naive" = NAIVE(value),
      # SNAIVE
      "snaive" = SNAIVE(value ~ lag("year")),
      # ETS Additive
      "ets_add" = ETS(value ~ error("A") + trend("A") + season("A")),
      # ETS Multiplicative
      "ets_mult" = ETS(value ~ error("M") + trend("A") + season("M")),
      # ETS Auto
      "ets_auto" = ETS(value)
    )
  
  report(fit)
  
  components(fit) %>% 
    autoplot()
  
  fit.resid <- augment(fit)
  autoplot(fit.resid, .innov) +
    facet_grid(.model ~ .)
  
  augment(fit$ets_add) %>% 
    gg_tsresiduals()
}

# Forecast ----
{
  # Predict Test data
  {
    fx <- fit %>% 
      forecast(new_data = test)
    
  }
  
  # Assess & Compare Prediction Errors
  {
    # Plot
    {
      fx %>% autoplot(
        test,
        level = NULL
      ) +
        theme_bw() +
        ggtitle("Forecasts for Northwest Hydro Generation")
    }
    
    accuracy(fx, test)
  }
}


# Cross Validation ----
{
  # Create CV dataset
  train.cv <- train %>% 
    stretch_tsibble(.init = 24, .step = 12)
  test.cv <- test %>% 
    stretch_tsibble(.init = 6, .step = 1)
  
  # Number of groups
  max(train.cv$.id)
  
  # Fit models
  fit.cv <- train.cv %>% 
    model(
      # Naive
      "naive" = NAIVE(value),
      # SNAIVE
      "snaive" = SNAIVE(value ~ lag("year")),
      # ETS Additive
      "ets_add" = ETS(value ~ error("A") + trend("A") + season("A")),
      # ETS Multiplicative
      "ets_mult" = ETS(value ~ error("M") + trend("A") + season("M"))
    )
  
  # Training Set Metrics
  fit.cv %>% 
    accuracy() %>% 
    group_by(.model, .type) %>% 
    summarize(
      across(c(ME, RMSE, MAE, MPE, MAPE, MASE, RMSSE), \(x){mean(x, na.rm = T)})
    )
  
  # Forecast Test Set
  fx.cv <- fit.cv %>% 
    forecast(new_data = test.cv)
  
  # Test Set Metrics
  fx.cv %>% 
    accuracy(test.cv) %>% 
    group_by(.model, .type) %>% 
    summarize(
      across(c(ME, RMSE, MAE, MPE, MAPE, MASE, RMSSE), \(x){mean(x, na.rm = T)})
    )
}