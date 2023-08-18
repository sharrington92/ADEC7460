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
  
  
  fn_cumulative_to_week <- function(the_fable, the_col, the_city, the_data = train.all, adjust_fx = 0){
    
    fx_start <- the_fable %>% 
      as_tibble() %>% 
      summarize(min(yearweek)) %>% 
      pull()
    
    last_val <- the_data %>% 
      filter(city == the_city) %>% 
      filter(yearweek < fx_start) %>% as_tibble() %>% 
      pull(cases_cumulative) %>% 
      last()
    
    # weekly <- the_fable$.mean - lag(the_fable$.mean, default = last_val)
    
    weekly <- pull(the_fable[,the_col]) - lag(pull(the_fable[,the_col]), default = last_val)
    
    if(adjust_fx == 1){
      weekly <- ifelse(weekly < 0, 0, weekly)
    }
    
    return(weekly)
  }
  
  
  fn_standardize_fx <- function(the_fx, the_model, the_city, the_smooth = 1){
    
    if(the_model == 1){
      fx.tbl <- the_fx %>% 
        select(city, .model, yearweek, cases_cumulative, .mean) %>% 
        hilo(95) %>% 
        mutate(
          predicted_cum_lo = `95%`$lower,
          predicted_cum_hi = `95%`$upper
        )
      
      fx.tbl$predicted_cases <- fn_cumulative_to_week(fx.tbl, ".mean", the_city, train.all, 1)
      fx.tbl$predicted_cases[1:3] <- 5
      fx.tbl$predicted_cases_lo <- fn_cumulative_to_week(fx.tbl, "predicted_cum_lo", the_city, train.all, 1)
      fx.tbl$predicted_cases_hi <- fn_cumulative_to_week(fx.tbl, "predicted_cum_hi", the_city, train.all, 1)
      
      fx.tbl %>% 
        select(-contains("cum"), -`95%`, -.mean) %>% 
        return()
      
    } else if(the_model == 2){
      
      fx.tbl <- the_fx %>% 
        select(city, .model, yearweek, cases_cumulative, .mean) %>% 
        hilo(95) %>% 
        mutate(
          predicted_cum_lo = `95%`$lower,
          predicted_cum_hi = `95%`$upper
        )
      
      fx.tbl$predicted_cases <- fn_cumulative_to_week(fx.tbl, ".mean", the_city, train.all, 1)
      fx.tbl$predicted_cases_lo <- fn_cumulative_to_week(fx.tbl, "predicted_cum_lo", the_city, train.all, 1)
      fx.tbl$predicted_cases_hi <- fn_cumulative_to_week(fx.tbl, "predicted_cum_hi", the_city, train.all, 1)
      
      fx.tbl %>% 
        select(-contains("cum"), -`95%`, -.mean) %>% 
        return()
      
    } else if(the_model == 3){
      
      if(the_city == "iq"){
        
        the_date <- ymd(valid.start.iq)
        
        the_fx %>% 
          group_by(.model_desc) %>%
          arrange(.index) %>%
          mutate(
            .value = exp(.value) - 1, 
            .value = ifelse(.key == "prediction", rollmean(.value, k = the_smooth, fill = .value, align = "center"), .value)
          ) %>% 
          ungroup() %>% 
          mutate(
            city = the_city,
            .model = "fit3",
            yearweek = yearweek(.index)
          ) %>% 
          rename(
            predicted_cases = .value, 
            predicted_cases_lo = .conf_lo,
            predicted_cases_hi = .conf_hi
          ) %>% 
          filter(.index >= year(the_date)) %>%
          select(city, .model, yearweek, predicted_cases, predicted_cases_lo, predicted_cases_hi) %>% 
          mutate(
            across(c(predicted_cases, predicted_cases_lo, predicted_cases_hi), \(x){pmax(x, 0)})
          ) %>% 
          tsibble(index = yearweek, key = c(city, .model)) %>% 
          return()
        
      } else{
        
        the_date <- ymd(valid.start.sj)
        
        the_fx %>% 
          group_by(.model_desc) %>%
          arrange(.index) %>%
          mutate(
            city = the_city,
            .model = "fit3",
            yearweek = yearweek(.index)
          ) %>% 
          left_join(
            data.all %>% select(city, yearweek, population),
            by = c("city", "yearweek")
          ) %>%
          mutate(
            across(c(.value, .conf_hi, .conf_lo), \(x){(1 / (1+exp(-(x-.00001)))) * population}),
            # .value = (1 / (1+exp(-(.value-.00001)))) * population,
            .value = ifelse(
              .key == "prediction",
              rollmean(.value, k = the_smooth, fill = .value, align = "center"),
              .value
            )
          ) %>%
          ungroup() %>% 
          # tsibble(index = .index, key = .key) %>% filter(year(.index) > 2003) %>% autoplot(.value)
          filter(.model_desc != "ACTUAL") %>% 
          rename(
            predicted_cases = .value, 
            predicted_cases_lo = .conf_lo,
            predicted_cases_hi = .conf_hi
          ) %>% 
          filter(year(.index) >= year(the_date)) %>%
          # summarize(MAE(predicted_cases - total_cases)) %>% pull()
          select(city, .model, yearweek, predicted_cases, predicted_cases_lo, predicted_cases_hi) %>% 
          mutate(
            across(c(predicted_cases, predicted_cases_lo, predicted_cases_hi), \(x){pmax(x, 0)})
          ) %>% #duplicates(index = yearweek, key = c(city, .model))
          tsibble(index = yearweek, key = c(city, .model)) %>% 
          return()
      }
      
    }
    
    
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
        # susc.1w = population - rollsum(total_cases, k = 1, fill = total_cases, align = "right"),
        # susc.2w = population - rollsum(total_cases, k = 2, fill = total_cases, align = "right"),
        # susc.4w = population - rollsum(total_cases, k = 4, fill = total_cases, align = "right"),
        # susc.8w = population - rollsum(total_cases, k = 8, fill = total_cases, align = "right"),
        # susc.13w = population - rollsum(total_cases, k = 13, fill = total_cases, align = "right"),
        # susc.26w = population - rollsum(total_cases, k = 26, fill = total_cases, align = "right"),
        # susc.52w = population - rollsum(total_cases, k = 52, fill = total_cases, align = "right")
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
      c(vars.id, "is_missing", "total_cases_scaled", "total_cases", "is_test", "case_rate")
    )
    
    
    data.all.sa <- cols[!str_detect(cols, "cases")] %>% 
      lapply(., \(x){
        # print(x)
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
  
  
  # Ready training & valid datasets ----
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
      ) %>%
      mutate(
        across(
          contains("PC"), .names = "{.col}_365d", 
          \(x){rollmean(x, k = 52, fill = NA, align = "right")}
        ),
        reanalysis_tdtr_k_sa_smooth = rollmean(reanalysis_tdtr_k_sa, 26, NA, align = "right") %>% 
          rollmax(., k = 15, NA, align = "right"),
        across(
          c(contains("reanalysis"), -contains("_sa"), reanalysis_tdtr_k_sa_smooth), 
          .names = "{.col}_cut", 
          \(x){cut_interval(x, n = 10)}
        )
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


# Save Data ----
{
  saveRDS(train, "Report Objects/train.RDS")
  saveRDS(train.all, "Report Objects/train.all.RDS")
  saveRDS(valid, "Report Objects/valid.RDS")
  saveRDS(test, "Report Objects/test.RDS")
}


# Other Calcs ----
{
  vars.id <- c(vars.id, "is_missing")
  vars.y <- colnames(train.all)[str_detect(colnames(train.all), "case")]
  vars.x <- setdiff(colnames(train.all), c(vars.id, vars.y))
  vars.x.pc <- vars.x[str_detect(vars.x, "PC")]
  vars.x.sa <- vars.x[str_detect(vars.x, "_sa")]
  vars.x.roll <- vars.x[str_detect(vars.x, "_[0-9]+[wd]_")]
  vars.x.cut <- vars.x[str_detect(vars.x, "_cut")]
  
  
  train.start.iq <- as_tibble(train) %>% filter(city == "iq") %>% summarize(first(week_start_date)) %>% pull()
  valid.start.iq <- as_tibble(valid) %>% filter(city == "iq") %>% summarize(first(week_start_date)) %>% pull()
  test.start.iq <- as_tibble(test) %>% filter(city == "iq") %>% summarize(first(week_start_date)) %>% pull()
  test.end.iq <- as_tibble(test) %>% filter(city == "iq") %>% summarize(last(week_start_date)) %>% pull()
  
  train.start.sj <- as_tibble(train) %>% filter(city == "sj") %>% summarize(first(week_start_date)) %>% pull()
  valid.start.sj <- as_tibble(valid) %>% filter(city == "sj") %>% summarize(first(week_start_date)) %>% pull()
  test.start.sj <- as_tibble(test) %>% filter(city == "sj") %>% summarize(first(week_start_date)) %>% pull()
  test.end.sj <- as_tibble(test) %>% filter(city == "sj") %>% summarize(last(week_start_date)) %>% pull()
  
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

