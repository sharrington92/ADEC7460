# Setup ----
{
  library(tidyverse)
  library(fpp3)
  library(zoo)
  
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


# Load Raw Data ----
{
  test <- read_csv(file.path("Data", "dengue_features_test.csv")) %>% 
    mutate(week_start_date = yearweek(week_start_date)) %>% 
    tsibble(key = city, index = week_start_date)
  
  train.all.raw <- read_csv(file.path("Data", "dengue_labels_train.csv")) %>% 
    inner_join(
      y = read_csv(file.path("Data", "dengue_features_train.csv")),
      by = c("city", "year", "weekofyear")
    ) %>% 
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
  
  
  vars.id <- c("yearweek", "year", "city", "weekofyear", "week_start_date")
  vars.fact <- setdiff(colnames(train.all.raw), vars.id)
  
  
  ## Missing Values ----
  {
    # train.all.raw %>% 
    #   select(total_cases) %>% 
    #   filter(is.na(total_cases))
    
    train.all.raw_with.miss <- train.all.raw %>% 
      mutate(
        is_missing = coalesce(total_cases*0, 1) %>% factor,
        year = year(yearweek),
        week_start_date = as.Date(yearweek),
        weekofyear= week(week_start_date)
      ) %>% 
      mutate(
        across(all_of(vars.fact), na.approx)
      )
    
    
    
    # train.all.raw_with.miss %>% 
    #   # filter(year %in% c(2006:2006)) %>% 
    #   filter(week_start_date >= ymd("2006-09-01")) %>% 
    #   filter(week_start_date <= ymd("2007-03-01")) %>% 
    #   filter(city != "iq") %>% 
    #   # mutate(total_cases_approx = na.approx(total_cases)) %>% 
    #   ggplot(aes(
    #     x = yearweek, 
    #     y = coalesce(total_cases, -1),
    #     # y = total_cases_approx,
    #     # color = is_missing,
    #     shape = is_missing, group = city
    #   )) +
    #   geom_line() +
    #   geom_point(aes(color = is_missing), size = 3)
    
    
    # Will linearly approx missing values for now. 
    #   Need to check residuals to see if those points are outliers
    
    rm(train.all.raw)
  }
  
  # Add other vars ----
  {
    train.all.raw_with.calcs <- train.all.raw_with.miss %>% 
      group_by(year, city) %>% 
      mutate(
        cases_ytd = cumsum(total_cases)
      ) %>% 
      ungroup() %>% 
      group_by(city) %>% 
      mutate(
        total_cases_scaled = scale(total_cases),
        cases_cumulative = cumsum(total_cases),
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
      ungroup()
    
    
    # bind_rows(
    #   other.data.list$Iquitos_Population_Data.csv %>% 
    #     mutate(city = "iq"),
    #   other.data.list$San_Juan_Population_Data.csv %>% 
    #     mutate(city = "sj")
    # ) %>% #group_by(city) #%>% summarize(min(Year))
    #   mutate(
    #     week_start_date = paste(Year, 1, 1, sep = "-") %>% 
    #       ymd %>% 
    #       floor_date(unit = "weeks")
    #   ) %>% 
    #   complete(
    #     week_start_date = seq.Date(from = ymd("1990-1-7"), to = ymd("2015-1-5"), by = "1 week"),
    #     nesting(city)
    #   ) %>% 
    #   group_by(city) %>% 
    #   mutate(
    #     population = na.spline(Estimated_population),
    #     week_start_date = yearweek(week_start_date)
    #   ) %>% View()
    #   tsibble(key = city, index = (week_start_date)) %>% 
    #   autoplot(population)
    
    rm(train.all.raw_with.miss)
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
    
    train.all.sa <- setdiff(colnames(train.all.raw_with.calcs), c(vars.id, "is_missing", "total_cases_scaled")) %>% 
      lapply(., \(x){
        print(x)
        fn_deseasonalize(train.all.raw_with.calcs, !!x)
      }) %>% 
      reduce(., \(x, y){inner_join(x, y, by = c("yearweek", "city"))})
    
  }
  
  
  # PCA ----
  {
    # train.all.raw_with.calcs[,setdiff(vars.fact, "total_cases")] %>% 
    #   prcomp()
    
    train.split <- train.all.sa %>% 
      # filter(city == "iq") %>% 
      mutate(
        across(setdiff(vars.fact, "total_cases"), \(x){difference(x, 52)})
      ) %>% 
      arrange(yearweek) %>% 
      as_tibble() %>% 
      filter(!is.na(ndvi_nw)) %>% 
      split(~city)
    
    train.pca.matrix <- train.split %>% 
      lapply(., \(x){
        dat <- x %>% 
          
          select(setdiff(vars.fact, "total_cases")) %>% 
          as.matrix()
        
        prcomp(dat)$x[,c(1:20)]
      })
    
    train.all.pca <- bind_rows(
      bind_cols(
        train.split$iq, 
        as_tibble(train.pca.matrix$iq)
      ),
      bind_cols(
        train.split$sj, 
        as_tibble(train.pca.matrix$sj)
      )
    ) %>% 
      tsibble(index = yearweek, key = city)
    
    rm(train.pca.matrix, train.split)
  }
  
  
  # Ready training & valid datasets
  {
    train.all <- train.all.raw_with.calcs %>% 
      left_join(
        y = train.all.sa %>% 
          rename_with(.cols = -c(yearweek, city), ~paste0(.x, "_sa")),
        by = c("yearweek", "city")
      ) %>% 
      left_join(
        y = train.all.pca %>% 
          select(yearweek, city, contains("PC")),
        by = c("yearweek", "city")
      )
    
    valid <- train.all %>% 
      group_by(city) %>% 
      slice_max(order_by = yearweek, prop = .2) %>% 
      ungroup() %>% 
      tsibble(index = yearweek, key = city)
    train <- train.all %>% 
      anti_join(y = as_tibble(valid), by = c("city", "yearweek")) %>% 
      tsibble(index = yearweek, key = city)
    
    rm(train.all.raw_with.calcs, train.all.pca, train.all.sa)
  }
}


# Other Calcs ----
{
  vars.id <- c(vars.id, "is_missing")
  vars.y <- colnames(train.all)[str_detect(colnames(train.all), "cases")]
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
}
