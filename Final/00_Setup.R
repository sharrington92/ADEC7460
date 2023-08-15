# Setup ----
{
  library(tidyverse)
  library(fpp3)
  library(zoo)
  library(tidymodels)
  require(timetk)
  require(randomForest)
  require(xgboost)
  require(doParallel)
  
  # https://www.drivendata.org/competitions/44/dengai-predicting-disease-spread/page/82/#features_list
  
  if(!str_detect(basename(getwd()), "Time Series") & str_detect(dirname(getwd()), "Time Series")){
    repeat{
      setwd("../")
      if(str_detect(basename(getwd()), "Time Series")){
        break
      }
    }
  }
  
  if(basename(getwd()) != "Final") setwd(file.path(getwd(), "Final"))
  
  
  theme_set(theme_bw())
}


# User-defined Functions ----
{
  fn_model.validate.reg <- function(model, data.valid, yvar){
    
    model.pred <- augment(model, data.valid)
    
    fx.rmse <- rmse_vec(truth = pull(model.pred[,yvar]), estimate = model.pred$.pred)
    
    fx.rsq <- rsq_vec(truth = pull(model.pred[,yvar]), estimate = model.pred$.pred)
    
    fx.mape <- mape_vec(truth = pull(model.pred[,yvar]), estimate = model.pred$.pred)
    
    fx.plot <- qplot(x = pull(model.pred[,yvar]), y = model.pred$.pred) +
      geom_abline(slope = 1, intercept = 0, size = .75) +
      theme_bw()
    
    fx.metrics <- tibble(
      Variable = c("rmse", "mape", "rsq"),
      Value = c(fx.rmse, fx.mape, fx.rsq)
    )
    
    list(
      "model.pred" = model.pred,
      "plot" = fx.plot,
      "metrics" = fx.metrics
    ) %>% 
      return()
  }
  
  
  fn_cumulative_to_week <- function(the_fable, the_city, the_data = train.all, adjust_fx = 0){
    
    fx_start <- the_fable %>% 
      as_tibble() %>% 
      summarize(min(yearweek)) %>% 
      pull()
    
    last_val <- the_data %>% 
      filter(city == the_city) %>% 
      filter(yearweek < fx_start) %>% 
      pull(cases_cumulative) %>% 
      last()
    
    weekly <- the_fable$.mean - lag(the_fable$.mean, default = last_val)
    
    if(adjust_fx == 1){
      weekly <- ifelse(weekly < 0, 0, weekly)
    }
    
    return(weekly)
  }
}


# Load Raw Data ----
{
  test.raw <- read_csv(file.path("Data", "dengue_features_test.csv")) %>% 
    mutate(
      yearweek = yearweek(week_start_date),
      is_test = 1
    ) %>% 
    tsibble(key = city, index = week_start_date)
  
  test.start <- as_tibble(test.raw) %>% 
    group_by(city) %>% 
    slice_min(n = 1, order_by = week_start_date) %>% 
    select(week_start_date, city) %>% 
    rename(test_start = week_start_date)
  
  data.all.raw <- read_csv(file.path("Data", "dengue_labels_train.csv")) %>% 
    inner_join(
      y = read_csv(file.path("Data", "dengue_features_train.csv")),
      by = c("city", "year", "weekofyear")
    ) %>% 
    mutate(is_test = 0) %>% 
    bind_rows(test.raw) %>% 
    mutate(yearweek = yearweek(week_start_date)) %>% 
    tsibble(key = city, index = yearweek) %>% 
    fill_gaps() %>%
    relocate(yearweek, everything())
  
  
  other.data.list <- list.files(file.path("Data", "Additional"), pattern = "csv", full.names = T) %>% 
    lapply(., read_csv)
  
  names(other.data.list) <- list.files(file.path("Data", "Additional"), pattern = "csv", full.names = F)
  
  
  
}


# Data Preparation ----
{
  
  
  vars.id <- c("yearweek", "year", "city", "weekofyear", "week_start_date", "is_test")
  vars.fact <- setdiff(colnames(data.all.raw), vars.id)
  
  
  ## Missing Values ----
  {
    # train.all.raw %>% 
    #   select(total_cases) %>% 
    #   filter(is.na(total_cases))
    
    data.all.raw_with.miss <- data.all.raw %>% 
      mutate(
        is_missing = coalesce(total_cases*0, 1) %>% factor,
        week_start_date = as.Date(yearweek),
        year = year(week_start_date),
        weekofyear= week(week_start_date)
      ) %>% 
      left_join(
        y = test.start, by = "city"
      ) %>% 
      mutate(
        across(all_of(setdiff(c(vars.fact, "is_test"), "total_cases")), na.approx),
        total_cases = ifelse(week_start_date < test_start, coalesce(total_cases, 0), NA)
      ) %>% 
      select(-test_start)
    
    
    
    # data.all.raw_with.miss %>%
    #   # filter(year %in% c(2006:2006)) %>%
    #   filter(week_start_date >= ymd("2006-09-01")) %>%
    #   filter(week_start_date <= ymd("2007-03-01")) %>%
    #   filter(city != "iq") %>%
    #   # mutate(total_cases_approx = na.approx(total_cases)) %>%
    #   ggplot(aes(
    #     x = yearweek,
    #     y = coalesce(total_cases, -10),
    #     # y = total_cases_approx,
    #     # color = is_missing,
    #     shape = is_missing, group = city
    #   )) +
    #   geom_line() +
    #   geom_point(aes(color = is_missing), size = 3)
    
    
    # Will linearly approx missing values for now. 
    #   Need to check residuals to see if those points are outliers
    
    rm(data.all.raw)
  }
  
  # Add other vars ----
  {
    data.all.raw_with.calcs <- data.all.raw_with.miss %>% 
      group_by(year, city) %>% 
      mutate(
        cases_ytd = cumsum(total_cases)
      ) %>% 
      ungroup() %>% 
      group_by(city) %>% 
      mutate(
        days_in_week = week_start_date - lag(week_start_date),
        total_cases_scaled = scale(total_cases),
        cases_cumulative = cumsum(total_cases),
        total_cases_trans_log = log(cases_cumulative) - lag(log(cases_cumulative), default = 0),
        total_cases_trans_bc2 = box_cox(cases_cumulative, 2) - lag(box_cox(cases_cumulative, 2), default = 0),
        total_cases_trans_bc.7 = box_cox(cases_cumulative, .75) - lag(box_cox(cases_cumulative, .75), default = 0),
        cases_365d = rollsum(total_cases, k = 52, na.pad = TRUE, fill = NA, align = "right"),
        cases_8w = rollsum(total_cases, k = 8, na.pad = TRUE, fill = NA, align = "right"),
        cases_26w = rollsum(total_cases, k = 26, na.pad = TRUE, fill = NA, align = "right"),
        
        precip_365d = rollsum(precipitation_amt_mm, k = 52, na.pad = TRUE, fill = NA, align = "right"),
        precip_4w = rollsum(precipitation_amt_mm, k = 4*7, na.pad = TRUE, fill = NA, align = "right"),
        
        hdd_station = pmin(pmax(station_avg_temp_c - 10, 0), 34-10),
        hdd_station_365d = rollsum(hdd_station, k = 52, na.pad = TRUE, fill = NA, align = "right"),
        hdd_station_4w = rollsum(hdd_station, k = 4, na.pad = TRUE, fill = NA, align = "right"),
        hdd_reanalysis = pmax(reanalysis_avg_temp_k - 18, 0),
        hdd_reanalysis_365d = rollsum(hdd_reanalysis, k = 52, na.pad = TRUE, fill = NA, align = "right"),
        hdd_reanalysis_4w = rollsum(hdd_reanalysis, k = 4, na.pad = TRUE, fill = NA, align = "right"),
        
        humidity_rel_avg_4w = rollmean(reanalysis_relative_humidity_percent, k = 4, na.pad = TRUE, fill = NA, align = "right"),
        humidity_rel_avg_2w = rollmean(reanalysis_relative_humidity_percent, k = 2, na.pad = TRUE, fill = NA, align = "right"),
      ) %>% 
      ungroup() %>% 
      left_join(
        y = bind_rows(
          other.data.list$Iquitos_Population_Data.csv %>%
            mutate(city = "iq"),
          other.data.list$San_Juan_Population_Data.csv %>% 
            mutate(city = "sj")
         ) %>% 
          mutate(
            week_start_date = paste(Year, 1, 1, sep = "-") %>%
              ymd %>%
              floor_date(unit = "weeks")
          ) %>%
          group_by(city) %>% 
          complete(
            week_start_date = seq.Date(from = min(week_start_date), to = max(week_start_date), by = "1 week")
          ) %>%
          mutate(
            population = na.approx(Estimated_population),
            population = rollmean(population, k = 13, fill = population, align = "center"),
            week_start_date = yearweek(week_start_date)
          ) %>% 
          tsibble(key = city, index = (week_start_date)) %>%
          select(city, week_start_date, population)
      ) %>% 
      group_by(city) %>% 
      mutate(
        case_rate = total_cases / population,
        susc.1w = population - rollsum(total_cases, k = 1, fill = total_cases, align = "right"),
        susc.2w = population - rollsum(total_cases, k = 2, fill = total_cases, align = "right"),
        susc.4w = population - rollsum(total_cases, k = 4, fill = total_cases, align = "right"),
        susc.8w = population - rollsum(total_cases, k = 8, fill = total_cases, align = "right"),
        susc.13w = population - rollsum(total_cases, k = 13, fill = total_cases, align = "right"),
        susc.26w = population - rollsum(total_cases, k = 26, fill = total_cases, align = "right"),
        susc.52w = population - rollsum(total_cases, k = 52, fill = total_cases, align = "right")
      ) %>% 
      ungroup()
    
    rm(data.all.raw_with.miss)
  }
  
  
  # Deseasonalize ----
  {
    fn_deseasonalize <- function(the_data, the_column){
      the_column_var <- rlang::ensym(the_column)
      
      the_data %>% 
        filter(!is.na(!!the_column_var)) %>% 
        model(
          STL(!!the_column_var)
        ) %>% 
        components() %>% 
        select(yearweek, city, season_adjust) %>% 
        rename(!!the_column_var := season_adjust)
      
    }
    
    cols <- setdiff(
      colnames(data.all.raw_with.calcs), 
      c(vars.id, "is_missing", "total_cases_scaled", "total_cases", "is_test")
    )
    
    
    data.all.sa <- cols[!str_detect(cols, "cases")] %>% 
      lapply(., \(x){
        print(x)
        fn_deseasonalize(data.all.raw_with.calcs, !!x)
      }) %>% 
      reduce(., \(x, y){inner_join(x, y, by = c("yearweek", "city"))})
    
    rm(cols)
  }
  
  
  # PCA ----
  {
    # train.all.raw_with.calcs[,setdiff(vars.fact, "total_cases")] %>% 
    #   prcomp()
    
    data.citysplit <- data.all.sa %>% 
      # filter(city == "iq") %>% 
      mutate(
        across(setdiff(vars.fact, "total_cases"), \(x){difference(x, 52)})
      ) %>% 
      arrange(yearweek) %>% 
      as_tibble() %>% 
      filter(!is.na(ndvi_nw)) %>% 
      split(~city)
    
    data.pca.matrix <- data.citysplit %>% 
      lapply(., \(x){
        dat <- x %>% 
          
          select(setdiff(vars.fact, "total_cases")) %>% 
          as.matrix()
        
        prcomp(dat)$x[,c(1:20)]
      })
    
    data.all.pca <- bind_rows(
      bind_cols(
        data.citysplit$iq, 
        as_tibble(data.pca.matrix$iq)
      ),
      bind_cols(
        data.citysplit$sj, 
        as_tibble(data.pca.matrix$sj)
      )
    ) %>% 
      tsibble(index = yearweek, key = city)
    
    rm(data.pca.matrix, data.citysplit)
  }
  
  
  # Ready training & valid datasets
  {
    data.all <- data.all.raw_with.calcs %>% 
      left_join(
        y = data.all.sa %>% 
          rename_with(.cols = -c(yearweek, city), ~paste0(.x, "_sa")),
        by = c("yearweek", "city")
      ) %>% 
      left_join(
        y = data.all.pca %>% 
          select(yearweek, city, contains("PC")),
        by = c("yearweek", "city")
      )
    
    test <- data.all %>% 
      filter(is_test == 1) %>% 
      select(-is_test)
    valid <- data.all %>% 
      filter(is_test == 0) %>% 
      select(-is_test) %>% 
      group_by(city) %>% 
      slice_max(order_by = yearweek, prop = .2) %>% 
      ungroup() %>% 
      tsibble(index = yearweek, key = city)
    train <- data.all %>% 
      filter(is_test == 0) %>% 
      select(-is_test) %>% 
      anti_join(y = as_tibble(valid), by = c("city", "yearweek")) %>% 
      tsibble(index = yearweek, key = city)
    
    train.all <- data.all %>% 
      filter(is_test == 0) %>% 
      select(-is_test)
    
    rm(data.all.raw_with.calcs, data.all.pca, data.all.sa)
  }
}


# Data Recipe ----
{
  # data.recipe_gen <- train.all.raw %>% 
  #   as_tibble() %>% 
  #   recipe(total_cases ~ ., data = .) %>% 
  #   # update_role(
  #   #   any_of(c(setdiff(vars.y, "total_cases"))), 
  #   #   new_role = "ID"
  #   # ) %>%
  #   update_role(
  #     any_of(setdiff(vars.id, c("week_start_date", "is_missing"))),
  #     new_role = "ID"
  #   ) %>% 
  #   step_mutate()
  #   ## Missing Variables
  #   step_impute_bag(all_numeric_predictors()) %>% 
  #   step_imp
  #   ## Calculated variables
  #   
  #   ## Deseasonalize
  #   
  #   ## PCA
  #   
  #   
  #   timetk::step_smooth(
  #     # all_numeric_predictors(),
  #     period = 4,
  #     precipitation_amt_mm, reanalysis_air_temp_k, reanalysis_avg_temp_k,
  #     reanalysis_min_air_temp_k, reanalysis_precip_amt_kg_per_m2,
  #     reanalysis_max_air_temp_k, reanalysis_tdtr_k, reanalysis_relative_humidity_percent
  #   ) %>%
  #   step_arrange() %>% 
  #   step_lag(all_numeric_predictors(), lag = 1:10) %>% 
  #   # update_role(total_cases, new_role = "predictor") %>%
  #   # step_lag(total_cases, lag = 1:10, skip = T) %>%
  #   # update_role(total_cases, new_role = "outcome") %>%
  #   step_diff(all_numeric_predictors(), lag = 52) %>%
  #   step_diff(all_numeric_predictors()) %>%
  #   timetk::step_fourier(week_start_date, K = 4, period = 365/52) %>%
  #   step_naomit(all_numeric()) %>%
  #   step_timeseries_signature(week_start_date) %>%
  #   step_dummy(all_nominal_predictors()) %>% 
  #   step_corr(all_numeric_predictors(), threshold = .9) %>% 
  #   step_nzv() %>%
  #   update_role(week_start_date, new_role = "ID")
  
  
    
}


# Other Calcs ----
{
  vars.id <- c(vars.id, "is_missing")
  vars.y <- colnames(train.all)[str_detect(colnames(train.all), "case")]
  vars.x <- setdiff(colnames(train.all), c(vars.id, vars.y))
  vars.x.pc <- vars.x[str_detect(vars.x, "PC")]
  vars.x.sa <- vars.x[str_detect(vars.x, "_sa")]
  vars.x.roll <- vars.x[str_detect(vars.x, "_[0-9]+[wd]_")]
  
  
  years_with_bgn <- train %>% 
    filter(weekofyear==1) %>% 
    as_tibble() %>% 
    select(year, city)
  
  lambda.iq <- train %>% 
    filter(city == "iq") %>% 
    features(total_cases, features = guerrero) %>% 
    pull(lambda_guerrero)
  
  lambda.sj <- train %>% 
    filter(city == "sj") %>% 
    features(total_cases, features = guerrero) %>% 
    pull(lambda_guerrero)
  
  
  lambda.iq_cum <- train %>% 
    filter(city == "iq") %>% 
    features(log(cases_cumulative), features = guerrero) %>% 
    pull(lambda_guerrero)
  
  lambda.sj_cum <- train %>% 
    filter(city == "sj") %>% 
    features(log(cases_cumulative), features = guerrero) %>% 
    pull(lambda_guerrero)
  
  
  lambda.iq_trans_log <- train %>% 
    filter(city == "iq") %>% 
    features(total_cases_trans_log, features = guerrero) %>% 
    pull(lambda_guerrero)
  
  lambda.sj_cum_log <- train %>% 
    filter(city == "sj") %>% 
    features(total_cases_trans_log, features = guerrero) %>% 
    pull(lambda_guerrero)
  
  lambda.iq_trans_bc2 <- train %>% 
    filter(city == "iq") %>% 
    features(total_cases_trans_bc2, features = guerrero) %>% 
    pull(lambda_guerrero)
  
  lambda.sj_cum_bc2 <- train %>% 
    filter(city == "sj") %>% 
    features(total_cases_trans_bc2, features = guerrero) %>% 
    pull(lambda_guerrero)
  
  
  lambda.iq_trans_bc.7 <- train %>% 
    filter(city == "iq") %>% 
    features(total_cases_trans_bc.7, features = guerrero) %>% 
    pull(lambda_guerrero)
  
  lambda.sj_cum_bc.7 <- train %>% 
    filter(city == "sj") %>% 
    features(total_cases_trans_bc.7, features = guerrero) %>% 
    pull(lambda_guerrero)
}

