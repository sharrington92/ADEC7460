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

# Data Preparation ----
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
  
  vars.id <- c("yearweek", "year", "city", "weekofyear", "week_start_date")
  vars.fact <- setdiff(colnames(train.all.raw), vars.id)
  
  
  ## Missing Values ----
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
    
    
    
    train.all %>% 
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
  
  # Add other vars ----
  {
    train.all.done <- train.all %>% 
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
  }
  
  
  # PCA ----
  {
    train.all.done[,setdiff(vars.fact, "total_cases")] %>% 
      prcomp()
    
    train.split <- train.all.done %>% 
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
    
    train.all.sa <- setdiff(colnames(train.all.done), c(vars.id, "is_missing", "total_cases_scaled")) %>% 
      lapply(., \(x){
        print(x)
        fn_deseasonalize(train.all.done, !!x)
      }) %>% 
      reduce(., \(x, y){inner_join(x, y, by = c("yearweek", "city"))})
    
  }
  
  train.all.done_with.sa.pca <- train.all.done %>% 
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
  
  valid <- train.all.done_with.sa.pca %>% 
    group_by(city) %>% 
    slice_max(order_by = yearweek, prop = .2) %>% 
    ungroup() %>% 
    tsibble(index = yearweek, key = city)
  train <- train.all.done_with.sa.pca %>% 
    anti_join(y = as_tibble(valid), by = c("city", "yearweek")) %>% 
    tsibble(index = yearweek, key = city)
  
  
  valid %>% 
    as_tibble() %>% 
    group_by(city) %>% 
    summarize(
      max = max(yearweek),
      min = min(yearweek),
      n = n()
    )
}


# Other Calcs ----
{
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


# Visualizations ----
{
  ## Univariate ----
  {
    # Cases
    {
      # Time plot
      {
        train %>% 
          autoplot(box_cox(total_cases_scaled, 1))
        
        train %>% 
          autoplot(box_cox(cases_ytd, 1))
      }
      
      # Seasonality
      {
        train %>% 
          gg_season(box_cox(total_cases,1)) +
          scale_y_continuous(trans = "log10")
        
        train %>% 
          gg_season(box_cox(cases_ytd, 1)) +
          scale_y_continuous(trans = "log10")
        
        
        train %>% 
          inner_join(years_with_bgn, by = c("city", "year")) %>% 
          # gg_season(cases_ytd)
          gg_season(box_cox(cases_ytd, .25))
      }
      
      # Autocorrelation
      {
        train %>% 
          filter(city == "iq") %>% 
          gg_tsdisplay(
            total_cases,
            # difference(box_cox(total_cases, 1)),
            # difference(total_cases, 52),
            # difference(total_cases, 52) %>% difference(),
            plot_type = "partial", lag_max = 104
          )
        
        
        train %>% 
          filter(city == "iq") %>% 
          gg_tsdisplay(
            # box_cox(cases_cumulative, .75),
            difference(box_cox(cases_cumulative, .75)),
            # difference(box_cox(cases_cumulative, .75), 52),
            # difference(box_cox(cases_cumulative, .75), 52) %>% difference(),
            plot_type = "partial", lag_max = 104
          )
      }
      
      
      
      # Lag
      train %>% 
        features(cases_cumulative, guerrero)
      
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
        model(STL(box_cox(total_cases, .25))) %>% 
        components() %>% 
        autoplot()
    }
    
    
    # Regressors
    {
      train %>% 
        autoplot(reanalysis_relative_humidity_percent)
      
      train %>% 
        autoplot(reanalysis_specific_humidity_g_per_kg)
      
      train %>% 
        autoplot(ndvi_ne)
    }
  }
  
  # Bivariate ----
  {
    train %>% 
      filter(city == "iq") %>% 
      mutate(total_cases = box_cox(total_cases, .151)) %>% 
      GGally::ggpairs(columns = c(10:15, 4))
    
    
    train %>% 
      filter(city == "iq") %>% 
      mutate(
        # total_cases = difference(total_cases),
        across(
          -c(city, year, weekofyear, week_start_date, yearweek, total_cases, is_missing),
          # difference
          # \(x){log(x) %>% difference()}
          # \(x){lag(x, 1) %>% difference()}
          \(x){lag(x, 4)}
        )
      ) %>%
      as_tibble() %>% 
      select(-c(
        city, year, weekofyear, week_start_date, yearweek, is_missing, 
        cases_cumulative, 
        cases_ytd
        # total_cases
      )) %>% #filter(!is.na(total_cases)) %>% cor(use = 'complete.obs')
      select(total_cases, contains("_sa")) %>% 
      pivot_longer(-total_cases) %>% 
      slice_sample(prop = .25) %>% 
      ggplot(aes(x = box_cox(value, 1), y = box_cox(total_cases, .15), color = name)) +
      geom_point() +
      geom_smooth(method = "lm", color = "gray50") +
      facet_wrap(name ~ ., scales = "free") +
      theme(
        legend.position = "none"
      )
  }
  
  
  # Multivariate ----
  {
    L = 5
    train %>%
      mutate(
        cases_365d_cut = cut_number(cases_365d, n = L),
        precip_4w_cut = cut_number(precip_4w_sa, n = L),
        humidity_rel_avg_4w_cut = cut_number(humidity_rel_avg_4w, n = L),
        humidity_rel_avg_2w_cut = cut_number(humidity_rel_avg_2w, n = L),
        hdd_reanalysis_4w_cut = cut_number(hdd_reanalysis_4w_sa, n = L),
        hdd_station_4w_cut = cut_number(hdd_station_4w, n = L)
      ) %>% 
      ggplot(aes(
        x = lag(precip_4w_sa, n = L), 
        y = lag(hdd_station_4w_sa, n = L), 
        color = (total_cases_scaled), alpha = (total_cases_scaled)
      )) +
      geom_point() +
      facet_grid(lag(humidity_rel_avg_2w_cut, n = L) ~ city, scales = "free") +
      scale_color_viridis_c()
    
    
    # PCA
    {
      L = 4
      train %>%
        ggplot(aes(
          x = lag(PC2, n = L), 
          y = lag(PC20, n = L), 
          color = (total_cases_scaled), alpha = (total_cases_scaled)
        )) +
        geom_point(size = 3) +
        scale_color_viridis_c()
      
      fn_plot_pca_lags <- function(pcA, pcB, L){
        # if(is.character(pcA)){
          pcA <- rlang::sym(pcA)
          pcB <- rlang::sym(pcB)
        # } else{
        #   pcA <- rlang::ensym(pcA)
        #   pcB <- rlang::ensym(pcB)
        # }
        
        plots <- lapply(c(1:L), \(X){
          train %>%
            as_tibble() %>% 
            # slice_sample(prop = .5) %>% 
            ggplot(aes(
              x = lag(!!pcA, n = X), 
              y = lag(!!pcB, n = X)
            )) +
            geom_point(aes(
              color = (total_cases_scaled), alpha = (total_cases_scaled), size = total_cases_scaled
            )) +
            facet_grid(. ~ city, scales = "free") +
            # geom_contour(aes(z = total_cases), color = "red", size = 4) +
            scale_color_viridis_c() +
            ggtitle(paste0("Lag: ", X)) +
            theme(
              legend.position = "none"
            )
        })
        
        gridExtra::grid.arrange(grobs = plots)
      }
      
      fn_plot_pca_lags("PC4", "PC12", 6) 
      
      plots <- expand.grid(
        pcA = paste("PC", 1:20, sep = ""),
        pcB = paste("PC", 1:20, sep = "")
      ) %>% 
        as_tibble() %>% 
        filter(pcA != pcB) %>% 
        # slice_sample(n = 1) %>% 
        mutate(
          across(contains("pc"), as.character),
          L=6
        ) %>% 
        pmap(., fn_plot_pca_lags)
      
      gridExtra::grid.arrange(grobs = plots)
    }
    
    
  }
}


# Decomposition ----
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


# Linear Model ----
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


# Estimation ----
{
  ## Cases ----
  {
    ## Combined ----
    {
      (fit <- train %>% 
         model(
           "arima1" = ARIMA(total_cases),
           "arima2" = ARIMA((total_cases) ~ PDQ(0, 0, 0) + fourier(K = 2)),
           "arima3" = ARIMA((total_cases) ~ PDQ(0, 0, 0) + fourier(K = 3)),
           "arima4" = ARIMA((total_cases) ~ PDQ(0, 0, 0) + fourier(K = 4)),
           "arima5" = ARIMA((total_cases) ~ PDQ(0, 0, 0) + fourier(K = 5)),
           "arima6" = ARIMA((total_cases) ~ PDQ(0, 0, 0) + fourier(K = 6))
         ))
      
      glance(fit)
      
      fx <- fit %>% 
        forecast(new_data = valid)
      
      fx %>% 
        accuracy(valid) %>% 
        arrange(city, RMSE)
      
      fx %>% 
        autoplot(
          train.all %>% filter(year >= 2002),
          level = NULL
        )
    }
    
    
    ## IQ ----
    {
      
      
      
      (fit.iq <- train %>% 
         filter(city == "iq") %>% 
         model(
           "arima1" = ARIMA(box_cox(total_cases, lambda.iq)),
           "harmonic1" = ARIMA(box_cox(total_cases, lambda.iq) ~ PDQ(0, 0, 0) + fourier(K = 4)),
           "harmonic2" = ARIMA(
             box_cox(total_cases, lambda.iq) ~ PDQ(0, 0, 0) + fourier(K = 4) 
             + hdd_reanalysis_4w + precip_365d + precip_4w
           )
         ))
      
      glance(fit.iq)
      
      fx.iq <- fit.iq %>% 
        forecast(new_data = valid)
      
      fx.iq %>% 
        accuracy(valid) %>% 
        arrange(city, RMSE)
      
      fx.iq %>% 
        autoplot(
          train.all %>% filter(year >= 2007),
          level = NULL
        )
      
      
      # Find Correlations with residuals
      {
        train.with.resid <- fit.iq %>% 
          select(harmonic1) %>% 
          augment() %>% 
          select(yearweek, .resid, .innov) %>% 
          left_join(y = train %>% filter(city == "iq"))
        
        
        train.with.resid %>% 
          autoplot(.innov)
        
        
        train.with.resid %>% 
          autoplot()
        
        train.with.resid %>% 
          mutate(
            # total_cases = difference(total_cases),
            across(
              -c(city, year, weekofyear, week_start_date, yearweek, total_cases),
              # difference
              # \(x){log(x) %>% difference()}
              \(x){lag(x, 4)}
              # \(x){lag(x, 1) %>% difference()}
            )
          ) %>%
          as_tibble() %>% 
          select(-c(
            city, year, weekofyear, week_start_date, yearweek, is_missing, total_cases, cases_ytd, cases_cumulative,
            # .resid,
            .innov
          )) %>% 
          # filter(!is.na(total_cases)) %>% 
          # cor(use = 'complete.obs')
          pivot_longer(-.resid) %>% 
          ggplot(aes(x = (value), y = .resid, color = name)) +
          geom_point() +
          geom_smooth(method = "lm", color = "gray50") +
          facet_wrap(name ~ ., scales = "free") +
          theme(legend.position = "none")
        
        
        train.with.resid %>% 
          select(yearweek, contains("nvdi"), contains("temp"), contains("precip")) %>% 
          pivot_longer(-yearweek) %>% 
          ggplot(aes(x = yearweek, y = value, color = name)) +
          geom_line() +
          facet_wrap(name ~ ., scales = "free") +
          theme(legend.position = "none")
        
        
        # Against PCs
        train.with.resid %>% 
          select(
            yearweek, city, 
            .innov, 
            # .resid
          ) %>% 
          inner_join(train.all.pca, by = c("yearweek", "city")) %>% 
          select(
            yearweek, city, contains("PC"),
            .innov, 
            # .resid
          ) %>% 
          pivot_longer(-c(yearweek, city, .innov)) %>% 
          ggplot(aes(x = value, y = .innov, color = name)) +
          geom_point() +
          facet_wrap(name ~ ., scales = "free") +
          theme(legend.position = "none")
        }
    }
    
    
    ## GLM ----
    {
      fit.glm <- train %>% 
        filter(city == "iq") %>% 
        MASS::glm.nb(
          data = .,
          total_cases ~ 1 
          + lag(box_cox(total_cases, lambda.iq), n = 1)
          + lag(box_cox(total_cases^2, lambda.iq), n = 1)
          + lag(box_cox(total_cases, lambda.iq), n = 2)
          + lag(box_cox(total_cases, lambda.iq), n = 3)
          + lag(box_cox(total_cases^2, lambda.iq), n = 3)
          # + (rollmean(total_cases, k = 2, fill = NA, align = "right")>=5) %>% lag
          # + (rollmean(total_cases, k = 2, fill = NA, align = "right")>=30) %>% lag
          + I((lag(total_cases, n = 1)+1) / (lag(total_cases, n = 2)+1))
          # + I((lag(total_cases, n = 1)+1) / (lag(rollsum(total_cases, k = 20, fill = NA, align = "right"), n = 1)+1))
          + I((lag(total_cases, n = 1)+1) / (lag(rollsum(total_cases, k = 8, fill = NA, align = "right"), n = 1)+1))
          + I((lag(total_cases, n = 1)+1) / (lag(rollsum(total_cases, k = 52, fill = NA, align = "right"), n = 1)+1))
          # + lag(box_cox(total_cases^2, lambda.iq), n = 2)
          # + rollsum(box_cox(total_cases, lambda.iq), k = 8, fill = NA, align = "right") %>% lag(, n = 1)
          + rollsum(box_cox(total_cases, lambda.iq), k = 52, fill = NA, align = "right") %>% lag(, n = 1)
          + rollsum(box_cox(total_cases, lambda.iq), k = 52, fill = NA, align = "right")^2 %>% lag(, n = 1)
          # + lag(cases_8w^2, n = 1)
          # + lag(cases_26w, n = 1) + lag(cases_26w^2, n = 1)
          # + lag(cases_365d, n = 4)
          # + lag(cases_365d^2, n = 4)
          + lag(precip_4w, n = 5) #+ lag(precip_4w^2, n = 5)
          + lag(precip_365d, n = 1) #+ lag(precip_365d^2, n = 1)
          # + lag(hdd_reanalysis_365d, n = 1)
          # + hdd_reanalysis_365d + I(hdd_reanalysis_365d^2)
          # + lag(humidity_rel_avg_2w, n = 8)
          # + ndvi_ne #%>% log()
          # + ndvi_nw #%>% log()
          # + ndvi_se %>% log()
          + ndvi_sw #%>% log()
          + reanalysis_air_temp_k %>% rollmean(k = 3, fill = NA, align = "right") %>% lag(., n = 4)
          # + reanalysis_avg_temp_k
          # + reanalysis_max_air_temp_k
          # + reanalysis_min_air_temp_k
          + lag(hdd_reanalysis_4w, n = 6) #+ lag(hdd_reanalysis_4w^2, n = 4)
          + lag(humidity_rel_avg_4w, n = 6)
          + I(hdd_reanalysis_4w_sa * humidity_rel_avg_4w_sa) %>% lag(n = 6)
          
          # + precip_365d
          + station_diur_temp_rng_c_sa
          # + station_avg_temp_c_sa %>% rollmean(k = 2, fil = NA, align = "right") #%>% lag(n = 1)
          + reanalysis_tdtr_k_sa %>% rollmean(k = 5, fil = NA, align = "right") #%>% lag(n = 1)
          
          + PC1 %>% lag(n = 4)
          + lag(PC15, n =5)
          + lag(PC13, n = 3)
          + lag(PC4, n = 3)*lag(PC5, n = 3)
          
        ); summary(fit.glm);par(mfrow = c(2,2));# plot(fit.glm)
      
      fx.glm <- train.all.done_with.sa.pca %>% 
        filter(city == "iq") %>% 
        predict(
          fit.glm, newdata = .
        ); RMSE(fx.glm - pull(valid[valid$city=="iq", "total_cases"]))
      
      
      
      valid.glm.iq <- train.all.done_with.sa.pca %>%
        filter(city == "iq") %>% 
        bind_cols(
          fitted = fx.glm
        ) %>% 
        mutate(
          resids = fitted - total_cases
        ) %>% 
        inner_join(valid)
      
      
      train.all.done_with.sa.pca %>% 
        filter(city == "iq") %>% 
        filter(year>=2007) %>%
        autoplot(total_cases) +
        autolayer(valid.glm.iq %>% filter(year>=2007), fitted, color = "red3")
      
      train.glm.iq <- train %>% 
        filter(city == "iq") %>% 
        slice_max(n = length(residuals(fit.glm)), order_by = yearweek) %>% 
        bind_cols(
          resids = residuals(fit.glm),
          fitted = fitted(fit.glm)
        )
      
      # Fit ARIMA
      {
        fit.iq <- train.glm.iq %>% 
          model(
            "arima1" = ARIMA(resids),
            "harmonic1" = ARIMA(resids ~ PDQ(0, 0, 0) + fourier(K = 4)),
          )
        
        
        glance(fit.iq)
        
        fx.iq <- fit.iq %>% 
          forecast(new_data = valid.glm.iq)
        
        fx.iq %>% 
          # mutate(total_cases = total_cases + fitted) %>% 
          accuracy(valid.glm.iq) %>% 
          arrange(city, RMSE)
        
        fx.iq %>% 
          mutate(
            agg_cases = fitted + resids
          ) %>% as_tibble() %>% View()
        autoplot(
          agg_cases,
          # valid.glm.iq, #%>% filter(year >= 2007),
          level = NULL
        ) +
          autolayer(valid.glm.iq, resids)
        }
      
      
      # Find Correlations with residuals
      {
        
        car::crp(fit.glm)
        
        train.glm.iq %>% 
          autoplot(resids)
        train.glm.iq %>% 
          gg_tsdisplay(resids, plot_type = "partial", lag_max = 104)
        
      
        
        train.glm.iq %>% 
          mutate(
            # total_cases = difference(total_cases),
            across(
              -c(city, year, weekofyear, week_start_date, yearweek, total_cases),
              # difference
              # \(x){log(x) %>% difference()}
              \(x){lag(x, 1)}
              # \(x){lag(x, 1) %>% difference()}
            )
          ) %>%
          as_tibble() %>% 
          select(-c(
            city, year, weekofyear, week_start_date, yearweek, is_missing, total_cases, cases_ytd, cases_cumulative,
            # .resid,
            # resids
          )) %>% 
          # filter(!is.na(total_cases)) %>% 
          # cor(use = 'complete.obs')
          select(resids, contains("PC")) %>% 
          pivot_longer(-resids) %>% 
          slice_sample(prop = .25) %>% 
          ggplot(aes(x = (value), y = resids, color = name)) +
          geom_point() +
          geom_smooth(method = "lm", color = "gray50") +
          facet_wrap(name ~ ., scales = "free") +
          theme(legend.position = "none")
        
        
        train.with.resid %>% 
          select(yearweek, contains("nvdi"), contains("temp"), contains("precip")) %>% 
          pivot_longer(-yearweek) %>% 
          ggplot(aes(x = yearweek, y = value, color = name)) +
          geom_line() +
          facet_wrap(name ~ ., scales = "free") +
          theme(legend.position = "none")
        
        
        # Against PCs
        train.with.resid %>% 
          select(
            yearweek, city, 
            .innov, 
            # .resid
          ) %>% 
          inner_join(train.all.pca, by = c("yearweek", "city")) %>% 
          select(
            yearweek, city, contains("PC"),
            .innov, 
            # .resid
          ) %>% 
          pivot_longer(-c(yearweek, city, .innov)) %>% 
          ggplot(aes(x = value, y = .innov, color = name)) +
          geom_point() +
          facet_wrap(name ~ ., scales = "free") +
          theme(legend.position = "none")
        }
    }
  }
  
  # Cases YTD
  {
    
  }
}
