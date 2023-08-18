# Setup
{
  require(tidyverse)
  require(fpp3)
  library(modeltime)
  
  if(!stringr::str_detect(basename(getwd()), "Time Series") & stringr::str_detect(dirname(getwd()), "Time Series")){
    repeat{
      setwd("../")
      if(stringr::str_detect(basename(getwd()), "Time Series")){
        break
      }
    }
  }
  
  if(basename(getwd()) != "Final") setwd(file.path(getwd(), "Final"))
  
  source(file.path("00_Setup.R"))
  
  # 
  (all_cores <- parallel::detectCores())
  (cl <- makePSOCKcluster(all_cores))
  registerDoParallel(cl)
}


#
{
  # IQ
  {
    # MAE Goal: 5.09
    
    # Cumulative Cases
    {
      fit2.iq <- train %>% 
        filter(city == "iq") %>% 
        filter(cases_cumulative > 100) %>% 
        model(
          fit2a = ARIMA(
            box_cox(cases_cumulative, .9) ~ 1 + 
              fourier(K = 10) + PDQ(P=0, D=0,Q=0) 
            # + lag(precip_4w, n = 10) 
            # rollmean(reanalysis_tdtr_k_sa, k = 5, fill = NA, align = "right") +
            # lag(hdd_reanalysis_4w_sa * humidity_rel_avg_4w_sa, n = 6)
            # + PC15 %>% lag(n = 6)
            # + PC20 %>% lag(n = 1)
            # + PC1 %>% lag(n = 1)
            # + lag(station_diur_temp_rng_c, n = 5)
            # + lag(hdd_reanalysis_365d_sa, n = 5)
            # + lag(hdd_station_365d, n = 10)
            # + lag(precip_365d_sa, n = 8)
          ),
          
          fit2b = ARIMA(
            box_cox(cases_cumulative, .85) ~ 1 + 
              fourier(K = 10) + PDQ(P=0, D=0,Q=0) 
            # + lag(precip_4w, n = 10) 
            # rollmean(reanalysis_tdtr_k_sa, k = 5, fill = NA, align = "right") +
            # lag(hdd_reanalysis_4w_sa * humidity_rel_avg_4w_sa, n = 6)
            # + PC15 %>% lag(n = 6)
            # + PC20 %>% lag(n = 1)
            # + PC1 %>% lag(n = 1)
            # + lag(station_diur_temp_rng_c, n = 5)
            # + lag(hdd_reanalysis_365d_sa, n = 5)
            # + lag(hdd_station_365d, n = 10)
            # + lag(precip_365d_sa, n = 8)
          )
        )
    }
    
    # Case Rate
    {
      fit2.iq <- train %>% 
        filter(city == "iq") %>% 
        # filter(cases_cumulative > 10) %>% 
        model(
          fit2a = ARIMA(
            box_cox(case_rate + .00001, 0) ~ 1 + 
              fourier(K = 5)
            + PDQ(P=0, D=0,Q=0) 
            + lag(humidity_rel_avg_4w_sa, n = 5)
            + hdd_station_365d_sa
            + hdd_station_4w
            + lag(humidity_rel_avg_4w, 8)
            + lag(hdd_reanalysis_365d, 8)
            + lag(humidity_rel_avg_4w * hdd_reanalysis_365d, 8)
            # + lag(hdd_reanalysis_365d, 14)
            + lag(precip_365d, 1)
            # + PC10_365d
            # * PC12_365d
            + lag(PC13, 4)
            + lag(PC14_365d, 8)
            # + PC7_365d 
            # + lag(precip_4w * hdd_reanalysis_365d, 14)
          ),
          
          fit2b = ARIMA(
            box_cox(case_rate + .00001, 0) ~ 1 + 
              fourier(K = 10) + PDQ(P=0, D=0,Q=0) 
          ),
          
          fit2c = ARIMA(
            box_cox(case_rate + .00001, 0) ~ 1 + 
              fourier(K = 5)
            + PDQ(P=0, D=0,Q=0) 
            + lag(humidity_rel_avg_4w_sa, n = 5)
            + hdd_station_365d_sa
            + hdd_station_4w
            + lag(humidity_rel_avg_4w, 8)
            + lag(hdd_reanalysis_365d, 8)
            + lag(humidity_rel_avg_4w * hdd_reanalysis_365d, 8)
            # + lag(hdd_reanalysis_365d, 14)
            + lag(precip_365d, 1)
            # + PC10_365d
            # + lag(precip_4w * hdd_reanalysis_365d, 14)
          ),
        )
    }
    
    fit2.iq %>% 
      select(fit2a) %>%
      report()
    
    fit2.iq %>% 
      select(fit2a) %>%
      gg_tsresiduals(lag_max = 100)
    
    fx.valid <- forecast(fit2.iq, valid) # , bootstrap = TRUE, times = 1000
    
    fx.valid %>%
      autoplot(
        valid, 
        level = NULL
      ) ; fx.valid %>%
      fabletools::accuracy(valid)
    
    fx.valid %>%
      fabletools::accuracy(valid, measures = distribution_accuracy_measures)
    
    # fx2.iq <- fn_standardize_fx(forecast(fit2.iq, valid), the_model = 2, the_city = "iq")
    
    # fx2.iq %>%
    #   left_join(train.all %>% select(city, yearweek, total_cases)) %>%
    # as_tibble() %>%
    # summarize(
    #   MAE(predicted_cases - total_cases)
    # )
    
    fx.valid %>%
      mutate(predicted_cases = .mean * population) %>%
      as_tibble() %>%
      group_by(.model) %>%
      summarize(
        MAE(predicted_cases - total_cases)
      )
    
    # GLM On Residuals ----
    {
      
      L=10; K = 52; fit.glm <- augment(fit2.iq) %>%
        filter(.model == "fit2a") %>%
        inner_join(train.all, by = c("city", "yearweek")) %>% #as_tibble() #%>% View()
        # mutate(
        #   across(c(PC8, PC9, PC10, PC11), \(x){
        #     rollmean(x, k = K, fill = x, align = "right")
        #   })
        # ) %>%
        lm( # MASS::glm.nb
          data = .,
          .resid ~ 1 
          # + lag(hdd_reanalysis_365d, L)
          + lag(PC10_365d, L) + lag(PC14_365d, L) + lag(PC15_365d, L) 
          # + lag(PC1, L+1)
          # + lag(PC1, L+2)
          # + lag(PC1, L+3)
          # + lag(PC1, L+4)
          # + lag(hdd_reanalysis_365d, L)
          # + lag(precip_365d_sa * hdd_reanalysis_365d, L)
        ); summary(fit.glm);par(mfrow = c(2,2)); plot(fit.glm)
      
      (fit.glm)
      model.matrix(fit.glm)
    }
    
    
    # Bagged Forecast ----
    {
      
      
      # Generate new data
      {
        train_stl <- train %>% 
          filter(city == "iq") %>% 
          model(STL(box_cox(case_rate + .00001, 0)))
        
        train_stl %>% 
          components() %>% 
          autoplot()
        
        sim <- train_stl %>% 
          fabletools::generate(new_data = train, times = 100, bootstrap_block_size = 52) %>% 
          select(-.model, -case_rate)
        
        valid.sim <- valid %>% 
          rename(.sim = case_rate) %>% 
          as_tibble() %>% 
          cross_join(y = tibble(.rep = as.character(1:100))) %>% 
          tsibble(key = c(.rep), index = yearweek)
      }
      
      
      
      # Model & Forecast
      {
        sim_models <- sim %>% 
          model(
            fit.ar = ARIMA(
              box_cox(.sim + .00001, 0) ~ 1 + 
                fourier(K = 10)
              + PDQ(P=0, D=0,Q=0) 
              + lag(humidity_rel_avg_4w_sa, n = 5)
              + hdd_station_365d_sa
              + hdd_station_4w
              + lag(humidity_rel_avg_4w, 8)
              + lag(hdd_reanalysis_365d, 8)
              + lag(humidity_rel_avg_4w * hdd_reanalysis_365d, 8)
            ),
            
            fit.lm = TSLM(box_cox(.sim + .00001, 0) ~ fourier(K = 10) + trend())
          )
        
        sim_forecasts <- sim_models %>% 
          forecast(update_tsibble(valid.sim, key = c(city, .rep)))
        
        
        
        sim_forecasts %>% 
          autoplot() +
          autolayer(
            valid %>% filter(year>2000), case_rate
          ) +
          guides(colour = "none") 
        
        
        sim_forecasts %>% 
          as_tibble() %>% #View()
          ggplot(aes(x = yearweek, y = .mean, color = (.model), group = interaction(.model, .rep))) +
          geom_line() +
          geom_line(
            data = valid %>% filter(city == "iq"), 
            inherit.aes = F,
            aes(x = yearweek, y = case_rate)
          )
          guides(colour = "none") 
        
        
        sim_forecasts %>% 
          fabletools::accuracy(valid.sim) %>% 
          group_by(.model) %>% 
          summarize(mean(MAE))
        
        
        bagged <- sim_forecasts %>% 
          group_by(.model) %>% 
          summarise(
            bagged_mean = mean(.mean),
            bagged_median = median(.mean),
            bagged_p80 = quantile(.mean, .8),
            bagged_p20 = quantile(.mean, .2)
          )
        
        
        bagged %>% 
          left_join(y = valid %>% filter(city == "iq") %>% select(city, yearweek, case_rate)) %>% 
          pivot_longer(-c(.model, yearweek, city, case_rate), names_to = "forecast") %>% 
          as_tibble() %>% 
          group_by(.model, forecast) %>% 
          summarise(
            mae = mean(abs(value - case_rate))
          ) %>% 
          arrange(mae)
        
        
        
        fx.valid %>% 
          autoplot(
            valid, 
            level = NULL,
            size = .75, alpha = .75#, linetype = "dashed"
          ) +
          autolayer(
            bagged %>% filter(.model == "fit.ar"), 
            bagged_median, col = "gray30", size = 1, linetype = "dotted"
          ) +
          autolayer(
            bagged %>% filter(.model == "fit.ar"), 
            bagged_mean, col = "gray30", size = 1, linetype = "dashed"
          ) +
          autolayer(
            bagged %>% filter(.model == "fit.ar"), 
            bagged_p80, col = "gray30", size = 1, linetype = "dashed"
          ) +
          autolayer(
            bagged %>% filter(.model == "fit.ar"), 
            bagged_p20, col = "gray30", size = 1, linetype = "dashed"
          ) 
        
        
        
        
        
        
      }
    }
  }
  
  # SJ
  {
    # MAE Goal: 5.09
    
    # Cumulative Cases
    {
      fit2.sj_curr <- train %>% 
        filter(city == "sj") %>% 
        filter(cases_cumulative > 10) %>% 
        model(
          fit2_curr = ARIMA(
            box_cox(cases_cumulative, 1.4) ~ 1 
              + fourier(K = 5) + PDQ(P=0,D=0,Q=0) 
            + lag(precip_4w, n = 10) 
            + rollmean(reanalysis_tdtr_k_sa, k = 5, fill = NA, align = "right") 
            + lag(hdd_reanalysis_4w_sa * humidity_rel_avg_4w_sa, n = 6)
            + lag(station_diur_temp_rng_c, n = 5)
            + lag(hdd_reanalysis_365d_sa, n = 5)
            + lag(hdd_station_365d, n = 10)
            + lag(precip_365d_sa, n = 8)
          )
        )
      fit2.sj_curr %>% forecast(valid) %>% 
        # fabletools::accuracy(valid)
        autoplot(valid, level = NULL)
      
      fit2.sj <- train %>% 
        filter(city == "sj") %>% 
        filter(cases_cumulative > 100) %>% 
        model(
          fit2 = ARIMA(
            box_cox(cases_cumulative, 1.3) ~ 1 + 
              fourier(K = 2) + PDQ(P=0, D=0,Q=0) 
            + lag(rollmean(reanalysis_min_air_temp_k, k = 8, NA, align = "right"), n = 8)
            + lag(hdd_reanalysis_4w, n = 9)
            + lag(reanalysis_tdtr_k_sa_smooth, n = 6)
            + population
          )
        )
    }
    
    # Case Rate
    {
      fit2.sj <- train %>% 
        filter(city == "sj") %>% 
        # filter(cases_cumulative > 10) %>% 
        model(
          fit2a = ARIMA(
            box_cox(case_rate + 1, 0) ~ PDQ(P=0, D=0,Q=0) + pdq(d=0)
            + fourier(K = 1)
            + reanalysis_relative_humidity_percent_cut
            + reanalysis_tdtr_k_cut 
            + station_diur_temp_rng_c_sa 
          ),
          
          fit2b = ARIMA(
            box_cox(case_rate + .00001, 0) ~ PDQ(P=0, D=0,Q=0) + pdq(d=0)
            + fourier(K = 1)
            # + (reanalysis_tdtr_k_sa_smooth > 2.8)
            # + reanalysis_relative_humidity_percent_cut #* fourier(K = 1)
            # + lag(hdd_reanalysis_4w_sa > 1128, 5)
             # rollmean(difference(reanalysis_tdtr_k, 52), 26, NA, align = "right")
            + reanalysis_tdtr_k_sa_smooth * station_diur_temp_rng_c * reanalysis_min_air_temp_k_cut
          ),
          
          fit2c = ARIMA(
            box_cox(case_rate + 1, 0) ~ PDQ(P=0, D=0,Q=0) + pdq(d=0)
            + fourier(K = 1)
          )
        )
    }
    
    {
      fit2.sj %>% 
        select(fit2a) %>%
        report()
      
      fit2.sj %>% 
        select(fit2b) %>%
        gg_tsresiduals(lag_max = 100)
    }
    
    fx.valid <- forecast(fit2.sj, valid) # , bootstrap = TRUE, times = 1000
    
    fx.valid %>%
      autoplot(
        valid, 
        level = NULL
      ) ; fx.valid %>%
      fabletools::accuracy(valid)
    
    
    fx2.sj <- fn_standardize_fx(forecast(fit2.sj, valid), the_model = 2, the_city = "sj")
    
    fx2.sj %>%
      left_join(train.all %>% select(city, yearweek, total_cases)) %>%
      as_tibble() %>%
      summarize(
        MAE(predicted_cases - total_cases)
      )
    
    # fx.valid %>%
    #   mutate(predicted_cases = .mean * population) %>%
    #   as_tibble() %>%
    #   group_by(.model) %>%
    #   summarize(
    #     MAE(predicted_cases - total_cases)
    #   )
    
    # GLM On Residuals ----
    {
      
      L=10; K = 52; fit.glm <- augment(fit2.sj) %>%
        filter(.model == "fit2a") %>%
        inner_join(train.all, by = c("city", "yearweek")) %>% #as_tibble() #%>% View()
        # mutate(
        #   across(c(PC8, PC9, PC10, PC11), \(x){
        #     rollmean(x, k = K, fill = x, align = "right")
        #   })
        # ) %>%
        lm( # MASS::glm.nb
          data = .,
          .resid ~ 1 
          # + lag(hdd_reanalysis_365d, L)
          + lag(PC10_365d, L) + lag(PC14_365d, L) + lag(PC15_365d, L) 
          # + lag(PC1, L+1)
          # + lag(PC1, L+2)
          # + lag(PC1, L+3)
          # + lag(PC1, L+4)
          # + lag(hdd_reanalysis_365d, L)
          # + lag(precip_365d_sa * hdd_reanalysis_365d, L)
        ); summary(fit.glm);par(mfrow = c(2,2)); plot(fit.glm)
      
      (fit.glm)
      model.matrix(fit.glm)
    }
    
    
    # Bagged Forecast ----
    {
      
      
      # Generate new data
      {
        train_stl <- train %>% 
          filter(city == "sj") %>% 
          model(STL(box_cox(case_rate + .00001, 0)))
        
        train_stl %>% 
          components() %>% 
          autoplot()
        
        sim <- train_stl %>% 
          fabletools::generate(new_data = train, times = 100, bootstrap_block_size = 52) %>% 
          select(-.model, -case_rate)
        
        valid.sim <- valid %>% 
          rename(.sim = case_rate) %>% 
          as_tibble() %>% 
          cross_join(y = tibble(.rep = as.character(1:100))) %>% 
          tsibble(key = c(.rep), index = yearweek)
      }
      
      
      
      # Model & Forecast
      {
        sim_models <- sim %>% 
          model(
            fit.ar = ARIMA(
              box_cox(.sim + .00001, 0) ~ 1 + 
                fourier(K = 10)
              + PDQ(P=0, D=0,Q=0) 
              + lag(humidity_rel_avg_4w_sa, n = 5)
              + hdd_station_365d_sa
              + hdd_station_4w
              + lag(humidity_rel_avg_4w, 8)
              + lag(hdd_reanalysis_365d, 8)
              + lag(humidity_rel_avg_4w * hdd_reanalysis_365d, 8)
            ),
            
            fit.lm = TSLM(box_cox(.sim + .00001, 0) ~ fourier(K = 10) + trend())
          )
        
        sim_forecasts <- sim_models %>% 
          forecast(update_tsibble(valid.sim, key = c(city, .rep)))
        
        
        
        sim_forecasts %>% 
          autoplot() +
          autolayer(
            valid %>% filter(year>2000), case_rate
          ) +
          guides(colour = "none") 
        
        
        sim_forecasts %>% 
          as_tibble() %>% #View()
          ggplot(aes(x = yearweek, y = .mean, color = (.model), group = interaction(.model, .rep))) +
          geom_line() +
          geom_line(
            data = valid %>% filter(city == "sj"), 
            inherit.aes = F,
            aes(x = yearweek, y = case_rate)
          )
        guides(colour = "none") 
        
        
        sim_forecasts %>% 
          fabletools::accuracy(valid.sim) %>% 
          group_by(.model) %>% 
          summarize(mean(MAE))
        
        
        bagged <- sim_forecasts %>% 
          group_by(.model) %>% 
          summarise(
            bagged_mean = mean(.mean),
            bagged_median = median(.mean),
            bagged_p80 = quantile(.mean, .8),
            bagged_p20 = quantile(.mean, .2)
          )
        
        
        bagged %>% 
          left_join(y = valid %>% filter(city == "sj") %>% select(city, yearweek, case_rate)) %>% 
          pivot_longer(-c(.model, yearweek, city, case_rate), names_to = "forecast") %>% 
          as_tibble() %>% 
          group_by(.model, forecast) %>% 
          summarise(
            mae = mean(abs(value - case_rate))
          ) %>% 
          arrange(mae)
        
        
        
        fx.valid %>% 
          autoplot(
            valid, 
            level = NULL,
            size = .75, alpha = .75#, linetype = "dashed"
          ) +
          autolayer(
            bagged %>% filter(.model == "fit.ar"), 
            bagged_median, col = "gray30", size = 1, linetype = "dotted"
          ) +
          autolayer(
            bagged %>% filter(.model == "fit.ar"), 
            bagged_mean, col = "gray30", size = 1, linetype = "dashed"
          ) +
          autolayer(
            bagged %>% filter(.model == "fit.ar"), 
            bagged_p80, col = "gray30", size = 1, linetype = "dashed"
          ) +
          autolayer(
            bagged %>% filter(.model == "fit.ar"), 
            bagged_p20, col = "gray30", size = 1, linetype = "dashed"
          ) 
        
        
        
        
        
        
      }
    }
  }
}