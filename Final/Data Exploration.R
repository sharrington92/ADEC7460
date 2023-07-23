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
  
  train.all.raw <- read_csv(file.path("Final", "Data", "dengue_labels_train.csv")) %>% 
    inner_join(
      y = read_csv(file.path("Final", "Data", "dengue_features_train.csv")),
      by = c("city", "year", "weekofyear")
    ) %>% 
    mutate(yearweek = yearweek(week_start_date)) %>% 
    tsibble(key = city, index = yearweek) %>% 
    fill_gaps() %>% 
    relocate(yearweek, everything())
  
  vars.id <- c("yearweek", "year", "city", "weekofyear", "week_start_date")
  vars.fact <- setdiff(colnames(train.all.raw), vars.id)
  
  
  # Missing Values
  {
    train.all.raw %>% 
      select(total_cases) %>% 
      filter(is.na(total_cases))
    
    train.all <- train.all.raw %>% 
      mutate(
        is_missing = coalesce(total_cases*0, 1) %>% factor,
        year = year(yearweek),
        week_start_date = as.Date(yearweek),
        weekofyear= week(week_start_date)
      ) %>% 
      mutate(
        across(all_of(vars.fact), na.approx)
      )
    
    
    
    train.all2 %>% 
      # filter(year %in% c(2006:2006)) %>% 
      filter(week_start_date >= ymd("2006-09-01")) %>% 
      filter(week_start_date <= ymd("2007-03-01")) %>% 
      filter(city != "iq") %>% 
      # mutate(total_cases_approx = na.approx(total_cases)) %>% 
      ggplot(aes(
        x = yearweek, 
        y = coalesce(total_cases, -1),
        # y = total_cases_approx,
        # color = is_missing,
        shape = is_missing, group = city
      )) +
      geom_line() +
      geom_point(aes(color = is_missing), size = 3)
    
    
    # Will linearly approx missing values for now. 
    #   Need to check residuals to see if those points are outliers
  }
  
  # Add other vars
  {
    train.all.done <- train.all %>% 
      group_by(year, city) %>% 
      mutate(
        cases_ytd = cumsum(total_cases)
        # cases_ytd = ifelse(weekofyear != 1, coalesce(lag(cases_ytd), 0) + total_cases, total_cases)
      ) %>% 
      ungroup()
  }
  
  valid <- train.all.done %>% 
    group_by(city) %>% 
    slice_max(order_by = yearweek, prop = .2) %>% 
    ungroup()
  train <- train.all.done %>% 
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


# Other Calcs
{
  years_with_bgn <- train %>% 
    filter(weekofyear==1) %>% 
    as_tibble() %>% 
    select(year, city)
}


# Visualizations
{
  # Cases
  {
    # Time plot
    train %>% 
      autoplot(box_cox(total_cases, .25))
    
    # Seasonality
    {
      train %>% 
        gg_season(box_cox(total_cases,0))
      
      
      train %>% 
        inner_join(years_with_bgn, by = c("city", "year")) %>% 
        # gg_season(cases_ytd)
        gg_season(box_cox(cases_ytd, .25))
    }
    
    # Autocorrelation
    train %>% 
      filter(city == "iq") %>% 
      gg_tsdisplay(
        total_cases,
        # difference(total_cases, 52) %>% difference(), 
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
      # geom_histogram() +
      geom_density(color = "red3")
    
    
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
}


# Decomposition
{
  train %>% 
    model(STL(box_cox(total_cases, .25))) %>% 
    components() %>% 
    autoplot()
  
  
  train %>% 
    model(STL(box_cox(cases_ytd, .25))) %>% 
    components() %>% 
    autoplot()
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


