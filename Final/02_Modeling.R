# Setup ----
{
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
  
  
  source(file.path("00_Setup.R"))
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
