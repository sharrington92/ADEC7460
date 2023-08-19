# Setup ----
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

# IQ ----
{
  ## Recipe ----
  {
    recipe_spec_iq <- train %>%
      as_tibble() %>% 
      recipe(total_cases ~ ., data = .) %>%
      step_filter(city == "iq") %>% 
      step_timeseries_signature(week_start_date) %>% 
      step_rm(
        any_of(setdiff(!!vars.y, "total_cases"))
      ) %>%
      step_rm(
        any_of(setdiff(!!vars.id, c("weekofyear", "week_start_date")))
      ) %>% 
      step_rm(contains("PC")) %>% 
      step_rm(
        contains("am.pm"), contains("hour"), contains("minute"),
        contains("second"), contains("xts"), contains("day"), contains("susc")
      ) %>% 
      step_fourier(week_start_date, period = 52, K = 5) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_lag(all_numeric_predictors(), lag = 1:10) %>%
      step_normalize(all_numeric_predictors()) %>%
      step_diff(contains("_sa")) %>% 
      step_naomit(all_predictors(), total_cases) %>% 
      step_log(total_cases, offset = 1)
    
    recipe_spec_iq %>% prep() %>% juice() %>% #select(week_start_date, contains("cases")) #%>% View()
      colnames() %>% sort()
    
    recipe_spec_iq$var_info %>% View()
    
  }
  
  
  # Tune ----
  {
    model_spec_iq <- prophet_boost(mtry = tune(), tree_depth = tune(), learn_rate = tune(), min_n = tune()) %>%
      set_engine("prophet_xgboost", yearly.seasonality = TRUE, weekly.seasonality = TRUE) 
    
    workflow_fit_proph_boost_iq <- workflow() %>% 
      add_model(model_spec_iq) %>% 
      add_recipe(recipe_spec_iq)
    
    
    (tuning.grid_iq <- grid_regular(
      mtry(c(200, 400)),
      learn_rate(c(-1, 0)),
      tree_depth(c(10, 40)),
      min_n(c(20,26)),
      levels = c(4, 3, 4, 3)
    ))
    
    (folds_iq <- rolling_origin(
      train %>% filter(city == "iq"), 
      cumulative = F,
      initial = 52*4, assess = 50, skip = 60
    ))
    
    # Tune model
    fit3.tuning_iq <- workflow_fit_proph_boost_iq %>%
      tune_grid(folds_iq, grid = tuning.grid_iq)
    
    
    # saveRDS(fit3.tuning_iq, "Report Objects/fit3.tuning_iq.RDS")
  }
  
  
  # Inspect ----
  {
    collect_metrics(fit3.tuning_iq) %>%
      filter(.metric == "rmse") %>%
      ggplot(aes(
        x = learn_rate,
        y = mean,
        color = as.factor(mtry)
      )) +
      geom_line() +
      geom_point() +
      geom_errorbar(aes(ymax = mean + std_err, ymin = mean - std_err), alpha = .25) +
      scale_x_log10() +
      facet_grid(tree_depth ~ min_n, scales = "free_x") +
      scale_y_continuous(labels = label_comma()) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    
    collect_metrics(fit3.tuning_iq) %>% 
      filter(.metric == "rmse") %>% 
      rename(RMSE = mean) %>% 
      ggplot(aes(
        x = learn_rate,
        y = mtry, 
        fill = RMSE
      )) +
      geom_raster(interpolate = T) + 
      geom_point(
        data = select_best(fit3.tuning_iq, "rmse"),
        inherit.aes = F,
        aes(x = learn_rate, y = mtry), 
        shape = 3
      ) +
      geom_point(
        data = select_by_one_std_err(fit3.tuning_iq, "rmse"),
        inherit.aes = F,
        aes(x = learn_rate, y = mtry), 
        shape = 4
      ) +
      scale_x_log10() +
      facet_grid(tree_depth ~ min_n, scales = "free_x") +
      scale_y_continuous(labels = label_comma()) +
      scale_fill_viridis_c(option = "B", direction = -1) +
      ggtitle("Heat Map of Cross Validated RMSE facted by tree depth (horizontally) and min_n (vertically)") +
      theme_bw() +
      theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))
  }
  
  
  # Refit ----
  {
    (best.param_iq <- select_best(fit3.tuning_iq, "rmse"))
    
    wf.final_iq <- workflow_fit_proph_boost_iq %>% 
      finalize_workflow(best.param_iq)
    
    fit3.iq <- wf.final_iq %>% 
      fit(train)
    
    
    # saveRDS(fit3.iq, "Report Objects/fit3.iq.RDS")
    
    fit3.orig.iq <- extract_fit_engine(fit3.iq)
    
    xgboost::xgb.importance(model = fit3.orig.iq$models$model_2) %>%
      xgboost::xgb.plot.importance(top_n = 20) 
  }
  
  
  # Forecast ----
  {
    ### Valid ----
    {
      calibration_table_iq <- modeltime_table(fit3.iq) %>%
        modeltime_calibrate(as_tibble(valid) %>% filter(city == "iq"))
      
      fx3.iq_trans <- calibration_table_iq %>%
        modeltime_forecast(
          # new_data = as_tibble(train.all) %>% filter(city == "iq", year >= year(ymd(valid.start.iq)))
          new_data = as_tibble(valid) %>% filter(city == "iq"),
          actual_data = as_tibble(train.all) %>% filter(city == "iq")
        )
      
      modeltime::plot_modeltime_forecast(fx3.iq_trans, .conf_interval_show = T)
      
      # fx3.iq <- fn_standardize_fx(fx3.iq_trans, the_model = 3, the_city = "iq")
      
      ## trans to cases ----
      {
        
        # fx3.iq <- fn_standardize_fx(fx3.iq_trans, the_model = 3, the_city = "iq")
        the_city = "iq"
        
        if(the_city == "iq"){
          the_date <- ymd(valid.start.iq) 
        } else{
          the_date <- ymd(valid.start.iq)
        }
        
        fx3.iq_trans %>% 
          group_by(.model_desc) %>%
          arrange(.index) %>%
          mutate(
            city = the_city,
            .model = "fit3",
            yearweek = yearweek(.index)
          ) %>% 
          left_join(train.all %>% select(city, yearweek, total_cases, population), by = c("city", "yearweek")) %>% 
          mutate(
            # .value = (exp(.value) - .00001) * population,
            # .value = (1 / (1+exp(-(.value-.00001)))) * population,
            # .value2 = inv_box_cox(.value, .4)
            .value = exp(.value) - 1,
            .value = ifelse(.key == "prediction", rollmean(.value, k = 2, fill = .value, align = "center"), .value)
          ) %>%
          ungroup() %>% 
          # tsibble(index = .index, key = .key) %>% filter(year(.index) > 2007) %>% autoplot(.value)
        filter(.model_desc != "ACTUAL") %>% 
          rename(
            predicted_cases = .value, 
            predicted_cases_lo = .conf_lo,
            predicted_cases_hi = .conf_hi
          ) %>% 
          filter(year(.index) >= year(the_date)) %>%
          summarize(MAE(predicted_cases - total_cases)) %>% pull()
        select(city, .model, yearweek, predicted_cases, predicted_cases_lo, predicted_cases_hi) %>% 
          mutate(
            across(c(predicted_cases, predicted_cases_lo, predicted_cases_hi), \(x){pmax(x, 0)})
          ) %>% #duplicates(index = yearweek, key = c(city, .model))
          tsibble(index = yearweek, key = c(city, .model)) %>% View()
        return()
        
      }
    }
  }
}



# SJ ----
{
  ## Recipe ----
  {
    
    # recipe_spec_sj <- train %>%
    #   as_tibble() %>%
    #   filter(city == "sj") %>% 
    #   recipe(case_rate ~ ., data = .) %>%
    #   step_timeseries_signature(week_start_date) %>% 
    #   step_rm(
    #     any_of(setdiff(!!vars.y, "case_rate"))
    #   ) %>%
    #   step_rm(
    #     any_of(setdiff(!!vars.id, c("weekofyear", "week_start_date")))
    #   ) %>% 
    #   step_rm(contains("PC")) %>% 
    #   step_rm(
    #     contains("am.pm"), contains("hour"), contains("minute"),
    #     contains("second"), contains("xts"), contains("day"), contains("susc")
    #   ) %>% 
    #   step_dummy(all_nominal_predictors()) %>%
    #   step_lag(all_numeric_predictors(), lag = 1:10) %>%
    #   step_normalize(all_numeric_predictors()) %>%
    #   step_diff(contains("_sa")) %>% 
    #   step_naomit(all_predictors(), case_rate) %>% 
    #   step_logit(case_rate, offset = .00001)
    
    recipe_spec_sj <- train %>%
      as_tibble() %>%
      filter(city == "sj") %>% 
      recipe(case_rate ~ ., data = .) %>%
      step_timeseries_signature(week_start_date) %>% 
      step_rm(
        any_of(setdiff(!!vars.y, "case_rate")), population_sa
      ) %>%
      step_rm(
        any_of(setdiff(!!vars.id, c("weekofyear", "week_start_date")))
      ) %>% 
      step_rm(contains("PC")) %>% 
      step_rm(
        contains("am.pm"), contains("hour"), contains("minute"),
        contains("second"), contains("xts"), contains("day"), contains("susc")
      ) %>% 
      timetk::step_slidify_augment(contains("ndvi"), period = c(4, 8, 12, 52), .f = mean) %>%
      step_normalize(all_numeric_predictors()) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_lag(
        all_numeric_predictors(), -population,
        -contains("week"), -contains("year"), -contains("date"),
        lag = 1:10
      ) %>%
      step_diff(
        all_numeric_predictors(),
        -population, -contains("week"), -contains("year"), -contains("date"),
        -contains("cut")
      ) %>%
      step_naomit(all_predictors(), case_rate) %>% 
      # step_logit(case_rate, offset = .0001) %>%
      step_log(case_rate, offset = .000001) %>%
      step_nzv() %>% 
      step_fourier(week_start_date, K = 3, period = 365/52)
    
    recipe_spec_sj %>% prep() %>% juice() %>% #select(week_start_date, contains("cases")) #%>% View()
      colnames() %>% sort()
    
    recipe_spec_sj$var_info %>% View()
    
    
    model_spec_sj <- prophet_boost(mtry = tune(), tree_depth = tune(), learn_rate = tune(), min_n = tune()) %>%
      set_engine("prophet_xgboost", yearly.seasonality = TRUE, weekly.seasonality = TRUE) 
    
    workflow_fit_proph_boost_sj <- workflow() %>% 
      add_model(model_spec_sj) %>% 
      add_recipe(recipe_spec_sj)
    
  }
  
  
  # Tune ----
  {
    
    
    
    (tuning.grid_sj <- grid_regular(
      mtry(c(50, 300)),
      learn_rate(c(-1, 0)),
      tree_depth(c(100, 300)),
      min_n(c(20,100)),
      levels = c(3,3,3,2)
    ))
    
    (folds_sj <- rolling_origin(
      train %>% filter(city == "sj"), 
      cumulative = F,
      initial = 52*10, assess = 120, skip = 100
    ))
    
    # Tune model
    fit3.tuning_sj <- workflow_fit_proph_boost_sj %>%
      tune_grid(folds_sj, grid = tuning.grid_sj)
    beepr::beep()
    
    # saveRDS(fit3.tuning_sj, "Report Objects/fit3.tuning_sj.RDS")
  }
  
  
  # Inspect ----
  {
    collect_metrics(fit3.tuning_sj) %>%
      filter(.metric == "rmse") %>%
      ggplot(aes(
        x = learn_rate,
        y = mean,
        color = as.factor(mtry)
      )) +
      geom_line() +
      geom_point() +
      geom_errorbar(aes(ymax = mean + std_err, ymin = mean - std_err), alpha = .25) +
      scale_x_log10() +
      facet_grid(tree_depth ~ min_n, scales = "free_x") +
      scale_y_continuous(labels = label_comma()) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    
    collect_metrics(fit3.tuning_sj) %>% 
      filter(.metric == "rmse") %>% 
      rename(RMSE = mean) %>% 
      ggplot(aes(
        x = learn_rate,
        y = mtry, 
        fill = RMSE
      )) +
      geom_raster(interpolate = T) + 
      geom_point(
        data = select_best(fit3.tuning_sj, "rmse"),
        inherit.aes = F,
        aes(x = learn_rate, y = mtry), 
        shape = 3
      ) +
      geom_point(
        data = select_by_one_std_err(fit3.tuning_sj, "rmse"),
        inherit.aes = F,
        aes(x = learn_rate, y = mtry), 
        shape = 4
      ) +
      scale_x_log10() +
      facet_grid(tree_depth ~ min_n, scales = "free_x") +
      scale_y_continuous(labels = label_comma()) +
      scale_fill_viridis_c(option = "B", direction = -1) +
      ggtitle("Heat Map of Cross Validated RMSE facted by tree depth (horizontally) and min_n (vertically)") +
      theme_bw() +
      theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))
  }
  
  
  # Refit ----
  {
    (best.param_sj <- select_best(fit3.tuning_sj, "rmse"))
    
    wf.final_sj <- workflow_fit_proph_boost_sj %>% 
      finalize_workflow(best.param_sj)
    
    fit3.sj <- wf.final_sj %>% 
      fit(train)
    
    
    # saveRDS(fit3.sj, "Report Objects/fit3.sj.RDS")
    
    fit3.orig.sj <- extract_fit_engine(fit3.sj)
    
    xgboost::xgb.importance(model = fit3.orig.sj$models$model_2) %>%
      xgboost::xgb.plot.importance(top_n = 20) 
  }
  
  
  # Forecast ----
  {
    ### Valid ----
    {
      calibration_table_sj <- modeltime_table(fit3.sj) %>%
        modeltime_calibrate(as_tibble(valid) %>% filter(city == "sj"))
      
      fx3.sj_trans <- calibration_table_sj %>%
        modeltime_forecast(
          # new_data = as_tibble(train.all) %>% filter(city == "sj", year >= year(ymd(valid.start.sj)))
          new_data = as_tibble(valid) %>% filter(city == "sj"),
          actual_data = as_tibble(train.all) %>% filter(city == "sj") %>% filter(year > 2002)
        )
      
      modeltime::plot_modeltime_forecast(fx3.sj_trans, .conf_interval_show = T)
      
      # fx3.sj <- fn_standardize_fx(fx3.sj_trans, the_model = 3, the_city = "sj")
      
      ## trans to cases ----
      {
        
        # fx3.sj <- fn_standardize_fx(fx3.sj_trans, the_model = 3, the_city = "sj")
        the_city = "sj"
        the_date <- ymd(valid.start.sj) 
        
        
        fx3.sj_trans %>% 
          group_by(.model_desc) %>%
          arrange(.index) %>%
          mutate(
            city = the_city,
            .model = "fit3",
            yearweek = yearweek(.index)
          ) %>% 
          left_join(train.all %>% select(city, yearweek, total_cases, population), by = c("city", "yearweek")) %>% 
          mutate(
            # .value = (exp(.value) - .00001) * population,
            .value = (1 / (1+exp(-(.value-.00001)))) * population,
            # .value2 = inv_box_cox(.value, .4)
            # .value = exp(.value) - 1,
            .value = ifelse(.key == "prediction", rollmean(.value, k = 12, fill = .value, align = "center"), .value)
          ) %>%
          ungroup() %>% 
          tsibble(index = .index, key = .key) %>% filter(year(.index) > 2003) %>% autoplot(.value)
          filter(.model_desc != "ACTUAL") %>% 
          rename(
            predicted_cases = .value, 
            predicted_cases_lo = .conf_lo,
            predicted_cases_hi = .conf_hi
          ) %>% 
          filter(year(.index) >= year(the_date)) %>%
          summarize(MAE(predicted_cases - total_cases)) %>% pull()
        select(city, .model, yearweek, predicted_cases, predicted_cases_lo, predicted_cases_hi) %>% 
          mutate(
            across(c(predicted_cases, predicted_cases_lo, predicted_cases_hi), \(x){pmax(x, 0)})
          ) %>% #duplicates(index = yearweek, key = c(city, .model))
          tsibble(index = yearweek, key = c(city, .model)) %>% View()
        return()
        
      }
    }
  }
}




# SJ with ARIMA Boost ----
{
  ## Recipe ----
  {
    
    recipe_spec_sj <- train %>%
      as_tibble() %>%
      filter(city == "sj") %>% 
      recipe(case_rate ~ ., data = .) %>%
      step_timeseries_signature(week_start_date) %>% 
      step_rm(
        any_of(setdiff(!!vars.y, "case_rate")), population_sa
      ) %>%
      step_rm(
        any_of(setdiff(!!vars.id, c("weekofyear", "week_start_date")))
      ) %>% 
      step_rm(contains("PC")) %>% 
      step_rm(
        contains("am.pm"), contains("hour"), contains("minute"),
        contains("second"), contains("xts"), contains("day"), contains("susc")
      ) %>% 
      timetk::step_slidify_augment(contains("ndvi"), period = c(4, 8, 12, 52), .f = mean) %>%
      step_normalize(all_numeric_predictors()) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_lag(
        all_numeric_predictors(), -population,
        -contains("week"), -contains("year"), -contains("date"),
        lag = 1:10
      ) %>%
      step_diff(
        all_numeric_predictors(),
        -population, -contains("week"), -contains("year"), -contains("date"),
        -contains("cut")
      ) %>%
      step_naomit(all_predictors(), case_rate) %>% 
      # step_logit(case_rate, offset = .0001) %>%
      step_log(case_rate, offset = .000001) %>%
      step_nzv() %>% 
      step_fourier(week_start_date, K = 3, period = 365/52)
    
    recipe_spec_sj %>% prep() %>% juice() %>% 
      select(week_start_date, contains("case")) %>% ggplot(aes(x = week_start_date, y = case_rate)) + geom_line()
      # colnames() %>% as_tibble() %>% 
      # sort() %>% 
      View()
    
    recipe_spec_sj$var_info %>% View()
    
    
    
    model_spec_sj <- arima_boost(
      seasonal_period = "1 week", 
      seasonal_ar = 0, seasonal_differences = 0, seasonal_ma = 0,
      # seasonal_ar = tune(), seasonal_differences = 0, seasonal_ma = tune(),
      # non_seasonal_ar = tune(), non_seasonal_differences = tune(), non_seasonal_ma = tune(),
      non_seasonal_ar = 3, non_seasonal_differences = 0, non_seasonal_ma = 2,
      mtry = tune(), tree_depth = tune(), learn_rate = tune(), min_n = tune()
      # mtry = 50, tree_depth = 100, learn_rate = 1, min_n = 100
    ) %>%
      set_engine("arima_xgboost") #, yearly.seasonality = TRUE, weekly.seasonality = TRUE
    
    workflow_fit_ar_boost_sj <- workflow() %>% 
      add_model(model_spec_sj) %>% 
      add_recipe(recipe_spec_sj)
    
  }
  
  
  # Tune ----
  {
    
    
    (tuning.grid_sj <- grid_regular(
      mtry(c(50, 100)),
      learn_rate(c(-1, 0)),
      tree_depth(c(1, 10)),
      min_n(c(20,40)),
      levels = c(2,4,5,5)
    ))
    
    # (tuning.grid_sj <- grid_regular(
    #   non_seasonal_ar(c(0,5)),
    #   non_seasonal_ma(c(0,2)),
    #   # seasonal_ar(c(0,1)),
    #   # seasonal_ma(c(0,1)),
    #   non_seasonal_differences(c(0,1)),
    #   seasonal_differences(c(0,1)),
    #   levels = c(6,3, 2, 2)
    # ))
    
    (folds_sj <- rolling_origin(
      train %>% filter(city == "sj"), 
      cumulative = F,
      initial = 52*10, assess = 120, skip = 50
    ))
    
    # Tune model
    fit3.tuning_sj <- workflow_fit_ar_boost_sj %>%
      tune_grid(folds_sj, grid = tuning.grid_sj)
    
    
    # saveRDS(fit3.tuning_sj, "Report Objects/fit3.tuning_sj.RDS")
  }
  
  
  # Inspect ----
  {
    {
      collect_metrics(fit3.tuning_sj) %>%
        filter(.metric == "rmse") %>%
        # filter(non_seasonal_ma != 2) %>% 
        ggplot(aes(
          x = non_seasonal_ar,
          y = mean,
          color = factor(non_seasonal_ma),
          group = non_seasonal_ma
        )) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymax = mean + std_err, ymin = mean - std_err), alpha = .5) +
        # scale_x_log10() +
        # facet_grid(seasonal_ar ~ seasonal_ma, scales = "free_x") +
        facet_grid(non_seasonal_differences ~ seasonal_differences, scales = "free_x") +
        scale_y_continuous(labels = label_comma()) +
        theme_bw() +
        theme(legend.position = "bottom")
    }
    
    
    collect_metrics(fit3.tuning_sj) %>%
      filter(.metric == "rmse") %>%
      filter(learn_rate <= 10^0) %>% 
      ggplot(aes(
        x = tree_depth,
        y = mean,
        color = as.factor(mtry)
      )) +
      geom_line() +
      geom_point() +
      # geom_jitter() +
      # geom_errorbar(aes(ymax = mean + std_err, ymin = mean - std_err), alpha = .25) +
      # scale_x_log10() +
      facet_grid(learn_rate ~ min_n, scales = "free") +
      scale_y_continuous(labels = label_comma()) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    
    collect_metrics(fit3.tuning_sj) %>% 
      filter(.metric == "rmse") %>% 
      filter(learn_rate <= 10^(0)) %>% 
      rename(RMSE = mean) %>% 
      ggplot(aes(
        x = learn_rate,
        y = min_n, 
        fill = (RMSE)
      )) +
      geom_raster(interpolate = T) + 
      geom_point(
        data = select_best(fit3.tuning_sj, "rmse"),
        inherit.aes = F,
        aes(x = learn_rate, y = min_n), 
        shape = 3
      ) +
      geom_point(
        data = select_by_one_std_err(fit3.tuning_sj, "rmse"),
        inherit.aes = F,
        aes(x = learn_rate, y = min_n), 
        shape = 4
      ) +
      scale_x_log10() +
      facet_grid(tree_depth ~ mtry, scales = "free_x") +
      scale_y_continuous(labels = label_comma()) +
      scale_fill_viridis_c(option = "B", direction = -1) +
      ggtitle("Heat Map of Cross Validated RMSE facted by tree depth (rows) and mtry (columns)") +
      theme_bw() +
      theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))
  }
  
  
  # Refit ----
  {
    (best.param_sj <- select_best(fit3.tuning_sj, "rmse"))
    
    
    wf.final_sj <- workflow_fit_ar_boost_sj %>% 
      finalize_workflow(best.param_sj)
    
    fit3.sj <- wf.final_sj %>% 
      fit(train)
    
    
    # saveRDS(fit3.sj, "Report Objects/fit3.sj.RDS")
    
    fit3.orig.sj <- extract_fit_engine(fit3.sj)
    
    xgboost::xgb.importance(model = fit3.orig.sj$models$model_2) %>%
      xgboost::xgb.plot.importance(top_n = 20) 
  }
  
  
  # Forecast ----
  {
    ### Valid ----
    {
      calibration_table_sj <- modeltime_table(fit3.sj) %>%
        modeltime_calibrate(as_tibble(valid) %>% filter(city == "sj"))
      
      fx3.sj_trans <- calibration_table_sj %>%
        modeltime_forecast(
          # new_data = as_tibble(train.all) %>% filter(city == "sj", year >= year(ymd(valid.start.sj)))
          new_data = as_tibble(valid) %>% filter(city == "sj"),
          actual_data = as_tibble(train.all) %>% filter(city == "sj")
        )
      
      modeltime::plot_modeltime_forecast(fx3.sj_trans, .conf_interval_show = T)
      
      # fx3.sj <- fn_standardize_fx(fx3.sj_trans, the_model = 3, the_city = "sj")
      
      ## trans to cases ----
      {
        
        # fx3.sj <- fn_standardize_fx(fx3.sj_trans, the_model = 3, the_city = "sj")
        the_city = "sj"
        the_date <- ymd(valid.start.sj) 
        
        
        fx3.sj_trans %>% 
          group_by(.model_desc) %>%
          arrange(.index) %>%
          mutate(
            city = the_city,
            .model = "fit3",
            yearweek = yearweek(.index)
          ) %>% 
          left_join(train.all %>% select(city, yearweek, total_cases, population), by = c("city", "yearweek")) %>% 
          mutate(
            .value = (exp(.value) - .000001) * population,
            # .value = (1 / (1+exp(-(.value-.00001)))) * population,
            # .value2 = inv_box_cox(.value, .4)
            # .value = exp(.value) - 1,
            .value = ifelse(.key == "prediction", rollmean(.value, k = 1, fill = .value, align = "center"), .value)
          ) %>%
          ungroup() %>% 
          tsibble(index = .index, key = .key) %>% filter(year(.index) > 2003) %>% autoplot(.value)
        filter(.model_desc != "ACTUAL") %>% 
          rename(
            predicted_cases = .value, 
            predicted_cases_lo = .conf_lo,
            predicted_cases_hi = .conf_hi
          ) %>% 
          filter(year(.index) >= year(the_date)) %>%
          summarize(MAE(predicted_cases - total_cases)) %>% pull()
        select(city, .model, yearweek, predicted_cases, predicted_cases_lo, predicted_cases_hi) %>% 
          mutate(
            across(c(predicted_cases, predicted_cases_lo, predicted_cases_hi), \(x){pmax(x, 0)})
          ) %>% #duplicates(index = yearweek, key = c(city, .model))
          tsibble(index = yearweek, key = c(city, .model)) %>% View()
        return()
        
      }
    }
  }
}


parallel::stopCluster(cl)
