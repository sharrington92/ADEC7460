# Setup
{
  library(tidyverse)
  library(fpp3)
  library(zoo)
  
  # https://www.drivendata.org/competitions/44/dengai-predicting-disease-spread/page/82/#features_list
  
  
  
  
  theme_set(theme_bw())
}

# Data Preparation
{
  test <- read_csv(file.path("Final", "Data", "dengue_features_test.csv")) %>% 
    mutate(week_start_date = yearweek(week_start_date)) %>% 
    tsibble(key = city, index = week_start_date)
  
  train.all <- read_csv(file.path("Final", "Data", "dengue_labels_train.csv")) %>% 
    inner_join(
      y = read_csv(file.path("Final", "Data", "dengue_features_train.csv")),
      by = c("city", "year", "weekofyear")
    ) %>% 
    mutate(yearweek = yearweek(week_start_date)) %>% 
    tsibble(key = city, index = yearweek) %>% 
    fill_gaps() %>% 
    mutate()
  
  valid <- train.all %>% 
    group_by(city) %>% 
    slice_max(order_by = yearweek, prop = .2) %>% 
    ungroup()
  train <- train.all %>% 
    anti_join(y = valid, by = c("city", "yearweek"))
  
  
  valid %>% 
    as_tibble() %>% 
    group_by(city) %>% 
    summarize(
      max = max(yearweek),
      min = min(yearweek),
      n = n()
    )
  
}

# Cases
{
  # Time plot
  train %>% 
    autoplot(log(total_cases))
  
  # Seasonality
  train %>% 
    gg_season(box_cox(total_cases,0))
  
  # Autocorrelation
  train %>% 
    filter(city == "iq") %>% 
    mutate(total_cases = na.approx(total_cases)) %>% 
    gg_tsdisplay(
      difference(total_cases, 52) %>% difference(), 
      plot_type = "partial", lag_max = 104
    )
  
  
  # Lag
  train %>% 
    features(total_cases, guerrero)
  
  # Density
  train %>% 
    filter(city == "iq") %>% 
    mutate(
      total_cases = box_cox(total_cases, .151),
      total_cases = difference(total_cases, 1)
    ) %>% 
    ggplot(aes(x = total_cases)) +
    geom_density()
  
  
  # Decomp
  train %>% 
    mutate(total_cases = na.approx(total_cases)) %>% 
    model(STL(total_cases)) %>% 
    components() %>% 
    autoplot()
}

# Bivariate
{
  train %>% 
    filter(city == "iq") %>% 
    mutate(total_cases = box_cox(total_cases, .151)) %>% 
    GGally::ggpairs(columns = c(10:15, 4))
  
  
  train %>% 
    filter(city == "iq") %>% 
    mutate(
      total_cases = difference(total_cases),
      across(
        -c(city, year, weekofyear, week_start_date, yearweak, total_cases), 
        # difference
        # \(x){log(x) %>% difference()}
        \(x){lag(x, 1) %>% difference()}
      )
    ) %>% 
    as_tibble() %>% 
    select(-c(city, year, weekofyear, week_start_date, yearweak)) %>% #filter(!is.na(total_cases)) %>% cor(use = 'complete.obs')
    pivot_longer(-total_cases) %>% 
    ggplot(aes(x = value, y = total_cases, color = name)) +
    geom_point() +
    facet_wrap(name ~ ., scales = "free")
}

# Linear Model
{
  colnames(train)
  
  fit <- train %>% 
    model(
      TSLM(
        box_cox(total_cases, 1) ~ season() + lag(total_cases) + difference(lag(total_cases))
          + lag(total_cases^2) + fourier
        # difference(total_cases) ~ season()
      )
    ); report(fit %>% filter(city == "iq")); report(fit %>% filter(city == "sj"))
  fx <- fit %>% 
    forecast(valid)
  fx %>% 
    autoplot(valid); accuracy(fx, valid)
  
  fit %>% filter(city == "sj") %>% gg_tsresiduals()
  fit %>% filter(city == "iq") %>% gg_tsresiduals()
  
  {
    augment(fit) %>% select(city, yearweek, .resid) %>% 
      full_join(train, by = c("city", "yearweek")) %>% 
      filter(city == "iq") %>% 
      mutate(
        # total_cases = difference(total_cases),
        across(
          -c(city, year, weekofyear, week_start_date, yearweek, total_cases, .resid),
          # difference
          # log
          # lag
          # \(x){log(x) %>% difference()}
          \(x){lag(x, 1) %>% difference()}
        )
      ) %>%
      as_tibble() %>% 
      select(-c(city, year, weekofyear, week_start_date, yearweek)) %>% #filter(!is.na(total_cases)) %>% cor(use = 'complete.obs')
      pivot_longer(-.resid) %>% 
      ggplot(aes(x = value, y = .resid, color = name)) +
      geom_point() +
      facet_wrap(name ~ ., scales = "free") +
      theme(
        legend.position = "none"
      )
    }
}