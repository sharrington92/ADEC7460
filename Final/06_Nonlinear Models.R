# Setup ----
{
  # library(future)
  
  
  # https://www.drivendata.org/competitions/44/dengai-predicting-disease-spread/page/82/#features_list
  
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
  
  
}

# Prep Data ----
{
  the_city = "iq"
  
  train.all_sub <- train.all %>% 
    as_tibble() %>% 
    filter(city == the_city) %>% 
    tsibble(index = yearweek)
  
  train_sub <- train %>% 
    as_tibble() %>% 
    filter(city == the_city) %>% 
    tsibble(index = yearweek) 
  
  valid_sub <- valid %>% 
    as_tibble() %>% 
    filter(city == the_city) %>% 
    tsibble(index = yearweek)
  
  test_sub <- test %>% 
    as_tibble() %>% 
    filter(city == the_city) %>% 
    tsibble(index = yearweek)
  
  data.all_sub <- data.all %>% 
    as_tibble() %>% 
    filter(city == the_city) %>% 
    tsibble(index = yearweek)
}


# Neural Network ----
{
  
  ## Fit ----
  {
    # Prep
    {
      train_sub %>% 
        scan_gaps()
      valid_sub %>% 
        scan_gaps()
      train.all_sub %>% 
        scan_gaps()
      
      
      train_sub %>% 
        filter(cases_cumulative > 10) %>%
        gg_tsdisplay(
          total_cases_trans_bc.7 #%>% #log() %>% 
          # box_cox(., lambda.iq_trans) %>% 
          # difference()
        )
      
      L = .75
      train_sub %>% 
        # filter(cases_cumulative > 10) %>%
        mutate(cases_trans = box_cox(cases_cumulative, L) - lag(box_cox(cases_cumulative, L))) %>% 
        autoplot(cases_trans) 
      autolayer(train_sub, total_cases, color = "red3", alpha = .25) 
    }
    
    
    # Tidymodels to Optimize Hyperparameters ----
    {
      library(modeltime)
      
      # Prep
      {
        {
          recipe.nn <- train_sub %>% 
            recipe(data = ., total_cases ~ .) %>% 
            # step_mutate(total_cases = cumsum(total_cases)) %>% 
            step_filter(cases_cumulative >= 10, skip = FALSE) %>% 
            update_role(
              any_of(c(
                setdiff(vars.y, "total_cases"),
                "", "predict_cases"
              )), 
              new_role = "ID"
            ) %>%
            update_role(
              any_of(setdiff(vars.id, c("week_start_date", "weekofyear"))),
              # vars.id,
              new_role = "ID"
            ) %>% 
            update_role(
              contains("station"),
              new_role = "ID"
            ) %>% 
            update_role(
              contains("PC"),
              new_role = "ID"
            ) %>%
            # step_mutate(total_cases_bc = log(total_cases + .1527629)) %>%
            # step_arrange() %>%
            # step_lag(all_numeric_predictors(), lag = 1:10) %>%
            # step_diff(all_numeric_predictors(), -weekofyear, lag = 52) %>%
            # step_diff(all_numeric_predictors()) %>%
            # timetk::step_smooth(
            #   period = 4,
            #   precipitation_amt_mm, 
            #   # reanalysis_air_temp_k, 
            #   # reanalysis_avg_temp_k,
            #   # reanalysis_min_air_temp_k, 
            #   # reanalysis_precip_amt_kg_per_m2,
            #   # reanalysis_max_air_temp_k, 
            #   reanalysis_tdtr_k, 
            #   reanalysis_relative_humidity_percent
            # ) %>%
          # step_diff(total_cases) %>%
          # timetk::step_fourier(week_start_date, K = 4, period = 365/52) %>%
            step_naomit(all_numeric()) %>%
            step_naomit(total_cases) %>%
            # step_timeseries_signature(week_start_date) %>%
            # step_dummy(all_nominal_predictors()) %>%
            # step_corr(all_numeric_predictors(), threshold = .9) %>% 
            step_nzv() %>%
            update_role(week_start_date, new_role = "ID")
          
        }
        
        model.nn <- nnetar_reg(
          hidden_units = tune(), seasonal_period = "1 week",
          non_seasonal_ar = tune(), num_networks = tune()
        ) %>% 
          set_engine("nnet") %>% 
          set_mode("regression")
        
        
        workflow.lm <- workflow() %>% 
          add_model(model.nn) %>% 
          add_recipe(
            recipe.nn %>% 
              prep()
          )
        
        
        (tuning.grid <- grid_regular(
          hidden_units(c(1, 10)),
          num_networks(c(1, 100)), 
          non_seasonal_ar(c(1,4)),
          levels = c(4, 3, 4)
        ))
        
        # folds <- vfold_cv(data.train, v = 2)
        (folds <- rolling_origin(
          train_sub_with.fx, 
          cumulative = F,
          initial = 52*4, assess = 50, skip = 50
        ))
        
        # parallel::stopCluster(cl)
        (all_cores <- parallel::detectCores())
        (cl <- makePSOCKcluster(all_cores))
        registerDoParallel(cl)
      }
      
      
      ## Fit ----
      {
        # Tune model
        tuning.nn <- model.lm %>%
          fit(cases_cumulative ~ week_start_date, data = train_sub)
        
        collect_notes(tuning.lm)$note[1]# %>% View()
        show_notes(.Last.tune.result)
        
        # Tuning Results
        collect_metrics(tuning.lm) #%>% View()
        collect_metrics(tuning.lm) %>% 
          filter(.metric == "rmse") %>% 
          ggplot(aes(
            x = learn_rate, 
            y = mean, 
            # color = interaction(learn_rate, trees)
            color = as.factor(mtry)
          )) +
          geom_line() + 
          geom_point() +
          # geom_errorbar(aes(ymax = mean + std_err, ymin = mean - std_err)) +
          scale_x_log10() +
          # facet_grid(tree_depth ~ learn_rate, scales = "free_x") +
          scale_y_continuous(labels = label_comma()) +
          theme_bw() +
          theme(legend.position = "bottom")
        
        
        ## Best results ----
        (best.param <- select_best(tuning.lm, "rmse"))
        (best.param <- select_by_one_std_err(tuning.lm, "rmse"))
        
        
        
      }
    }
    
    
    ## Estimate ----
    {
      
      fit.nn <- train_sub %>% 
        filter(cases_cumulative > 10) %>% 
        model(
          m1 = NNETAR(
            total_cases
              # lag(precip_4w, n = 5) +
              # rollmean(reanalysis_tdtr_k_sa, k = 5, fill = NA, align = "right") +
              # lag(hdd_reanalysis_4w_sa * humidity_rel_avg_4w_sa, n = 6)
          ),
          # ETS(log(cases_cumulative) ~ trend("Ad", phi = .5))
        )
    }
    
    ## Analyze ----
    {
      fit.nn %>% report()
      
      fit.nn %>% 
        # augment() %>% autoplot(.fitted, color = "red3") + autolayer(train_sub, cases_cumulative, color = "black")
        gg_tsresiduals(lag_max = 100)
      report()
    }
    
    ## Forecast Validation ----
    {
      
      ### Evaluate ----
      {
        
        fx.valid_nn <- fit.nn %>% 
          forecast(valid_sub) #%>% 
          mutate(
            predict_cases = difference(.mean),
            .resid = predict_cases - total_cases
          ) %>% 
          select(.model, yearweek, total_cases, predict_cases, cases_cumulative, .resid)
        
        # Cumulative Cases
        fx.valid_ar %>% #as_tibble() %>% View()
          autoplot(valid_sub, level = NULL) + 
          autolayer(train.all_sub, cases_cumulative) #+
        # scale_y_continuous(trans = "log10")
        
        fx.valid_ar %>% 
          fabletools::accuracy(valid_sub)
        
        
        # Weekly Cases
        fx.valid_ar  %>% 
          pivot_longer(c(total_cases, predict_cases)) %>% #View()
          ggplot(aes(x = yearweek, y = value, color = name)) + geom_line()
        
        fx.valid_ar %>%
          filter(!is.na(predict_cases)) %>% 
          MAE(.resid = .$predict_cases - .$total_cases)
        
        
        
        
        
        
        
      }
      
      fit.ar.resid <- fit.ar %>% 
        augment()
      
      fx.valid_ar
      
      fit.ar %>% 
        forecast(valid_sub)
    }
  }
  
  
  ## Final Fit & Test Forecast ----
  {
    ### Fit ----
    {
      final.fit.glm <- update(fit.glm, data = as_tibble(train.all_sub))
      
      final.fit.ar <- train.all_sub %>% 
        filter(cases_cumulative > 0) %>% 
        model(
          ARIMA(box_cox(cases_cumulative, .756) ~ fourier(K = 10)),
          # ETS(log(cases_cumulative) ~ trend("Ad", phi = .5))
        )
    }
    
    
    ## Test ----
    {
      fx.test_ar <- final.fit.ar %>% 
        forecast(test_sub)
      fx.test_ar %>% #as_tibble() %>% View()
        autoplot(valid_sub, level = NULL) + autolayer(train.all_sub, cases_cumulative) +
        scale_y_continuous(trans = "log10")
      
      
      fx.test_ar %>% 
        select(yearweek, .mean, cases_cumulative) %>% 
        # hilo(1)
        mutate(
          total_cases.pred = difference(.mean)
        ) %>% 
        right_join(
          y = data.all_sub %>% select(yearweek, total_cases)
        ) %>% 
        filter(year(yearweek) >= 2000) %>% 
        ggplot(aes(x = yearweek)) +
        geom_line(aes(y = total_cases.pred), color = "red3") +
        geom_line(aes(y = total_cases), color = "black")
      
    }
    
    
    ## Save Forecast
    {
      fx_to_save <- list()
      
      fx_to_save[[1]] <- fx.valid_ar %>% 
        fabletools::accuracy(valid_sub)
      
      fx_to_save[[2]] <- fx.valid_ar %>% 
        select(yearweek, .mean, cases_cumulative) %>% 
        # hilo(1)
        mutate(
          total_cases.pred = difference(.mean)
        ) %>% 
        right_join(
          y = train.all_sub %>% select(yearweek, total_cases, cases_cumulative) %>% rename(cases_cumulative_act = cases_cumulative)
        )  %>% 
        arrange(yearweek) %>% 
        mutate(
          total_cases.pred = ifelse(!is.na(.mean), coalesce(total_cases.pred, .mean - lag(cases_cumulative_act)), total_cases)
        ) %>% 
        filter(!is.na(cases_cumulative))
      
      
      valid.mae <- mae_vec(fx_to_save[[2]]$total_cases, fx_to_save[[2]]$total_cases.pred)
      
      fx_to_save[[2]] <- fx_to_save[[2]] %>% 
        select(-cases_cumulative_act, -total_cases)
      
      fx_to_save[[3]] <- fx.test_ar %>% 
        select(yearweek, .mean, cases_cumulative) %>% 
        # hilo(1)
        mutate(
          total_cases.pred = difference(.mean)
        ) %>% 
        right_join(
          y = data.all_sub %>% select(yearweek, total_cases, cases_cumulative) %>% rename(cases_cumulative_act = cases_cumulative)
        )  %>% 
        arrange(yearweek) %>% 
        mutate(
          total_cases.pred = ifelse(!is.na(.mean), coalesce(total_cases.pred, .mean - lag(cases_cumulative_act)), total_cases)
        ) %>% 
        filter(!is.na(cases_cumulative)) %>% 
        select(-cases_cumulative_act, -total_cases)
      
      
      fx_to_save[[5]] <- final.fit.ar
      
      fx_to_save[[4]] <- final.fit.ar %>% 
        augment() %>% 
        select(yearweek, .fitted, .resid, .innov)
      
      names(fx_to_save) <- c("accuracy", "fx.valid", "fx.test", "resid.train.all", "final.fit")
      
      
      fx_to_save %>% 
        saveRDS(., file.path("Forecasts", paste0(
          "iq_ARIMA_", format(today(), "%Y%m%d"), "_MAE_", round(valid.mae, 2),".RDS"
        )))
    }
  }
}


# Prophet ----
{
  library(fable.prophet)
  
  ## Fit ----
  {
    # Prep
    {
      train_sub %>% 
        scan_gaps()
      
      train_sub %>% 
        filter(cases_cumulative > 10) %>%
        gg_tsdisplay(
          total_cases_trans_bc.7 #%>% #log() %>% 
          # box_cox(., lambda.iq_trans) %>% 
          # difference()
        )
      
      L = .75
      train_sub %>% 
        # filter(cases_cumulative > 10) %>%
        mutate(cases_trans = box_cox(cases_cumulative, L) - lag(box_cox(cases_cumulative, L))) %>% 
        autoplot(cases_trans) 
      autolayer(train_sub, total_cases, color = "red3", alpha = .25) 
    }
    
    ## Estimate ----
    {
      
      fit.pr <- train_sub %>% 
        filter(cases_cumulative > 10) %>% 
        model(
          m1 = prophet(
            box_cox(cases_cumulative, .7) ~ #season(period = "week", order = 5, type = 'multiplicative') +
              season(period = "month", order = 3, type = 'multiplicative') +
              season(period = "year", order = 10, type = 'multiplicative') +
              lag(hdd_reanalysis_4w)
          )
        )
    }
    
    # Analyze ----
    {
      fit.pr %>% 
        # augment() %>% autoplot(.fitted, color = "red3") + autolayer(train_sub, cases_cumulative, color = "black")
        gg_tsresiduals(lag_max = 100)
      
      fit.pr %>% 
        components %>% 
        autoplot()
    }
    
    # Forecast Validation ----
    {
      
      ### Evaluate ----
      {
        
        fx.valid_pr <- fit.pr %>% 
          forecast(valid_sub) %>% 
          mutate(
            predict_cases = difference(.mean),
            .resid = predict_cases - total_cases
          ) %>% 
          select(.model, yearweek, total_cases, predict_cases, cases_cumulative, .resid)
        
        # Cumulative Cases
        fx.valid_pr %>% #as_tibble() %>% View()
          autoplot(valid_sub, level = NULL) + 
          autolayer(train.all_sub, cases_cumulative) #+
        # scale_y_continuous(trans = "log10")
        
        fx.valid_pr %>% 
          fabletools::accuracy(valid_sub)
        
        
        # Weekly Cases
        fx.valid_pr  %>% 
          pivot_longer(c(total_cases, predict_cases)) %>% #View()
          ggplot(aes(x = yearweek, y = value, color = name)) + geom_line()
        
        fx.valid_pr %>%
          filter(!is.na(predict_cases)) %>% 
          MAE(.resid = .$predict_cases - .$total_cases)
        
        
        
        
        
        
        
      }
      
      fit.ar.resid <- fit.ar %>% 
        augment()
      
      fx.valid_ar
      
      fit.ar %>% 
        forecast(valid_sub)
    }
  }
  
  
  ## Final Fit & Test Forecast ----
  {
    ### Fit ----
    {
      final.fit.glm <- update(fit.glm, data = as_tibble(train.all_sub))
      
      final.fit.ar <- train.all_sub %>% 
        filter(cases_cumulative > 0) %>% 
        model(
          ARIMA(box_cox(cases_cumulative, .756) ~ fourier(K = 10)),
          # ETS(log(cases_cumulative) ~ trend("Ad", phi = .5))
        )
    }
    
    
    ## Test ----
    {
      fx.test_ar <- final.fit.ar %>% 
        forecast(test_sub)
      fx.test_ar %>% #as_tibble() %>% View()
        autoplot(valid_sub, level = NULL) + autolayer(train.all_sub, cases_cumulative) +
        scale_y_continuous(trans = "log10")
      
      
      fx.test_ar %>% 
        select(yearweek, .mean, cases_cumulative) %>% 
        # hilo(1)
        mutate(
          total_cases.pred = difference(.mean)
        ) %>% 
        right_join(
          y = data.all_sub %>% select(yearweek, total_cases)
        ) %>% 
        filter(year(yearweek) >= 2000) %>% 
        ggplot(aes(x = yearweek)) +
        geom_line(aes(y = total_cases.pred), color = "red3") +
        geom_line(aes(y = total_cases), color = "black")
      
    }
    
    
    ## Save Forecast
    {
      fx_to_save <- list()
      
      fx_to_save[[1]] <- fx.valid_ar %>% 
        fabletools::accuracy(valid_sub)
      
      fx_to_save[[2]] <- fx.valid_ar %>% 
        select(yearweek, .mean, cases_cumulative) %>% 
        # hilo(1)
        mutate(
          total_cases.pred = difference(.mean)
        ) %>% 
        right_join(
          y = train.all_sub %>% select(yearweek, total_cases, cases_cumulative) %>% rename(cases_cumulative_act = cases_cumulative)
        )  %>% 
        arrange(yearweek) %>% 
        mutate(
          total_cases.pred = ifelse(!is.na(.mean), coalesce(total_cases.pred, .mean - lag(cases_cumulative_act)), total_cases)
        ) %>% 
        filter(!is.na(cases_cumulative))
      
      
      valid.mae <- mae_vec(fx_to_save[[2]]$total_cases, fx_to_save[[2]]$total_cases.pred)
      
      fx_to_save[[2]] <- fx_to_save[[2]] %>% 
        select(-cases_cumulative_act, -total_cases)
      
      fx_to_save[[3]] <- fx.test_ar %>% 
        select(yearweek, .mean, cases_cumulative) %>% 
        # hilo(1)
        mutate(
          total_cases.pred = difference(.mean)
        ) %>% 
        right_join(
          y = data.all_sub %>% select(yearweek, total_cases, cases_cumulative) %>% rename(cases_cumulative_act = cases_cumulative)
        )  %>% 
        arrange(yearweek) %>% 
        mutate(
          total_cases.pred = ifelse(!is.na(.mean), coalesce(total_cases.pred, .mean - lag(cases_cumulative_act)), total_cases)
        ) %>% 
        filter(!is.na(cases_cumulative)) %>% 
        select(-cases_cumulative_act, -total_cases)
      
      
      fx_to_save[[5]] <- final.fit.ar
      
      fx_to_save[[4]] <- final.fit.ar %>% 
        augment() %>% 
        select(yearweek, .fitted, .resid, .innov)
      
      names(fx_to_save) <- c("accuracy", "fx.valid", "fx.test", "resid.train.all", "final.fit")
      
      
      fx_to_save %>% 
        saveRDS(., file.path("Forecasts", paste0(
          "iq_ARIMA_", format(today(), "%Y%m%d"), "_MAE_", round(valid.mae, 2),".RDS"
        )))
    }
  }
}


