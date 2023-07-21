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
  
  train <- read_csv(file.path("Final", "Data", "dengue_labels_train.csv")) %>% 
    inner_join(
      y = read_csv(file.path("Final", "Data", "dengue_features_train.csv")),
      by = c("city", "year", "weekofyear")
    ) %>% 
    mutate(yearweak = yearweek(week_start_date)) %>% 
    tsibble(key = city, index = yearweak) %>% 
    fill_gaps() %>% 
    mutate()
  
  train %>% 
    fill_gaps() %>% 
    anti_join(y = train) %>% View()
}

# Cases
{
  # Time plot
  train %>% 
    autoplot(log(total_cases))
  
  # Seasonality
  train %>% 
    gg_season(log(total_cases))
  
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