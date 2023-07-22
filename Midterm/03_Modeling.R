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
    summarize(flow = mean(flow_cfs, na.rm = T)/1000, gage_feet = mean(gage_feet)) %>% 
    ungroup() 
  
  theme_set(theme_bw())
}

# Preliminary
{
  # Use 80% for training set, 20% set
  test <- data %>% 
    slice_max(n = 12, order_by = date) %>% 
    tsibble()
  
  train <- data %>% 
    anti_join(y = as_tibble(test), by = "date") %>% 
    tsibble()
}

# Visualizations
{
  # Time
  data %>% tsibble() %>% 
    autoplot(flow) +
    ggtitle("Columbia River Flow (kcfs)")
  
  
  # Seasonality
  train %>% 
    gg_season(flow) +
    ggtitle("Seasonality Plot: Columbia River Flow (kcfs)")
  
  # Autocorrelation
  train %>% 
    gg_tsdisplay(
      difference(flow, 12) %>% difference(), plot_type = "partial", lag_max = 36
    ) 
}


# Model Estimation
{
  # build three models + one ensemble model (average of the three forecasts) for the last year
  fit <- train %>% 
    model(
      "ets" = ETS(flow ~ error() + trend("N") + season()),
      "arima" = ARIMA((flow)),
      # "lm" = TSLM(flow ~ season()),
      "lm" = TSLM(flow ~ season() + lag(flow))
    ) %>% 
    mutate(
      ensemble = (ets + arima + lm) / 3
    )
  
  saveRDS(fit, "fit.RDS")
}


# Validation
{
  fx <- fit %>% 
    forecast(test)
  
  fx %>% 
    autoplot(
      data %>% filter(year(date)>2021),
      level = NULL
    ) +
    ggtitle("Monthly Columbia River Flow Out-of-Sample Forecast")
  
  # You will estimate appropriate model statistics on the with-held 20% test set. 
  fx %>% 
    accuracy(test)
  
  fx %>% 
    accuracy(test, measures = distribution_accuracy_measures)
  
  # You will compare these statistics across the models and suggest the best. 
  
  
  saveRDS(fx, "fx.RDS")
}

# Unsuccessful Attempt to Bootstrap 3 models to create the ensemble model
{
  # sim <- fit %>% 
  #   select(-ensemble) %>% 
  #   generate(
  #     new_data = test, 
  #     times = 100,
  #     bootstrap_block_size = 24
  #   ) %>% 
  #   as_tibble() %>% 
  #   group_by(date, .rep) %>% 
  #   summarize(ensemble = mean(.sim)) %>% 
  #   ungroup() %>% 
  #   tsibble(key = .rep)
  # 
  # sim %>% 
  #   autoplot(ensemble) +
  #   autolayer(test, flow, size = 1) +
  #   geom_line(
  #     inherit.aes = F,
  #     data = as_tibble(sim) %>% group_by(date) %>% summarize(ensemble = mean(ensemble)),
  #     aes(x = date, y = ensemble), 
  #     linetype = "dashed", size = 1
  #   ) +
  #   theme(legend.position = "none")
  # 
  # 
  # 
  # m1 <- fit %>% 
  #   select(arima) %>%
  #   forecast(
  #     new_data = test, 
  #     times = 100,
  #     bootstrap_block_size = 24
  #   ) 
  # m2 <- fit %>% 
  #   select(lm) %>%
  #   forecast(
  #     new_data = test, 
  #     times = 100,
  #     bootstrap_block_size = 24
  #   ) 
  # m3 <- fit %>% 
  #   select(ets) %>%
  #   forecast(
  #     new_data = test, 
  #     times = 100,
  #     bootstrap_block_size = 24
  #   ) 
  # m4 <- (m1$flow + m2$flow) / 2
  # 
  # 
  #   update_tsibble(key = c(.rep, .model)) %>% 
  #   summarize(flow = mean(flow)) #%>% 
  #   accuracy(test, measures = distribution_accuracy_measures)
  # 
  # # You will estimate appropriate model statistics on the with-held 20% test set. 
  # sim %>% 
  #   accuracy(test)
  # 
  # fx %>% 
  #   accuracy(test, measures = distribution_accuracy_measures)
}


