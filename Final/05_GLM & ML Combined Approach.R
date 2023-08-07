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


# GLM ----
{
  
  ## Fit ----
  {
    ## Estimate ----
    {
      fit.glm <- train_sub %>% 
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
    }
    
    # Analyze ----
    {
      fit.glm %>% 
        residuals() %>% 
        bind_cols(
          train_sub %>%
            slice_max(n = length(fit.glm$residuals), order_by = yearweek)
        ) %>% 
        rename(.resid = `...1`) %>% 
        tsibble(index = yearweek) %>% fill_gaps() %>% 
        gg_tsdisplay(.resid, plot_type = "partial", lag_max = 100)
    }
    
    # Forecast Validation ----
    {
      
      ### Evaluate ----
      {
        fx.train.all_glm <- train.all_sub %>% 
          predict(
            fit.glm, newdata = .
          ); rmse_vec(fx.train.all_glm, train.all_sub$total_cases)
        
        
        fx.train.all_glm <- train.all_sub %>%
          bind_cols(
            cases_predict = fx.train.all_glm
          ) %>% 
          mutate(
            fx.error = cases_predict - total_cases
          ) %>% 
          relocate(yearweek, city, total_cases, cases_predict, fx.error, everything())
        
        fx.valid_glm <- fx.train.all_glm %>% 
          inner_join(valid_sub %>% select(yearweek, city))
        
        fx.valid_glm %>% 
          filter(year>=2007) %>%
          autoplot(total_cases) +
          autolayer(
            fx.valid_glm %>% filter(year>=2007), cases_predict, 
            color = "red3"
          )
      }
      
    }
  }
  
  
  ## Final Fit & Test Forecast ----
  {
    ### Fit ----
    {
      final.fit.glm <- update(fit.glm, data = as_tibble(train.all_sub))
    }
    
    
    ## Test ----
    {
      (fx.data.all_glm <- data.all_sub %>% 
        predict(
          final.fit.glm, newdata = .
        ))
      
      
      fx.data.all_glm <- data.all_sub %>%
        bind_cols(
          cases_predict = fx.data.all_glm
        ) %>% 
        mutate(
          fx.error = cases_predict - total_cases
        ) %>% 
        relocate(yearweek, city, total_cases, cases_predict, fx.error, everything())
      
      fx.data.all_glm %>% 
        as_tibble() %>% 
        View()
      
      fx.test_glm <- fx.data.all_glm %>% 
        inner_join(test_sub %>% select(yearweek, city))
      
      fx.test_glm %>% 
        filter(year>=2007) %>%
        autoplot(total_cases) +
        autolayer(
          fx.valid_glm %>% filter(year>=2007), cases_predict, 
          color = "red3"
        )
    }
  }
}


# GLM with FPP3 ----
{
  
  ## Fit ----
  {
    train_sub %>% 
      scan_gaps()
    
    ## Estimate ----
    {
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
      
      
      fit.ar <- train_sub %>% 
        filter(cases_cumulative > 10) %>% 
        model(
          m1 = ARIMA(
            box_cox(cases_cumulative, .7) ~ 1 + 
              #fourier(K = 5) + PDQ(D=0,Q=0) +
              lag(precip_4w, n = 5) +
              rollmean(reanalysis_tdtr_k_sa, k = 5, fill = NA, align = "right") +
              lag(hdd_reanalysis_4w_sa * humidity_rel_avg_4w_sa, n = 6)
          ),
          # ETS(log(cases_cumulative) ~ trend("Ad", phi = .5))
        )
    }
    
    # Analyze ----
    {
      fit.ar %>% report()
      
      fit.ar %>% 
        # augment() %>% autoplot(.fitted, color = "red3") + autolayer(train_sub, cases_cumulative, color = "black")
        gg_tsresiduals(lag_max = 100)
        report()
    }
    
    # Forecast Validation ----
    {
      
      ### Evaluate ----
      {
        
        fx.valid_ar <- fit.ar %>% 
          forecast(valid_sub)
        fx.valid_ar %>% #as_tibble() %>% View()
          autoplot(valid_sub, level = NULL) + 
          autolayer(train.all_sub, cases_cumulative) #+
          scale_y_continuous(trans = "log10")
        fx.valid_ar %>% 
          fabletools::accuracy(valid_sub)
        
        fx.valid_ar %>% 
          # hilo(50) %>% 
          mutate(
            predict_cases = difference(.mean),
            .resid = predict_cases - total_cases
          ) %>% 
          select(.model, yearweek, total_cases, predict_cases, cases_cumulative, .resid) %>% #View()
          # as_tibble() %>% summarize(mae = mean(abs(.resid), na.rm = T))
          pivot_longer(c(total_cases, predict_cases)) %>% #View()
          ggplot(aes(x = yearweek, y = value, color = name)) + geom_line()
          filter(!is.na(predict_cases)) %>% 
          MAE(.resid = .$predict_cases - .$total_cases)
        
        
        fx.valid_ar %>% 
          select(yearweek, .mean, total_cases_trans_bc.7) %>% 
          # hilo(1)
          mutate(
            total_cases.pred = (.mean)
          ) %>% 
          right_join(
            y = train.all_sub %>% select(yearweek, total_cases_trans_bc.7)
          ) %>% 
          filter(year(yearweek) >= 2006) %>% 
          ggplot(aes(x = yearweek)) +
          geom_line(aes(y = total_cases.pred), color = "red3") +
          geom_line(aes(y = total_cases_trans_bc.7), color = "black")
        
        
        
        fx.valid_glm %>% 
          filter(year>=2007) %>%
          autoplot() +
          autolayer(
            valid_sub %>% filter(year>=2007), total_cases,
            color = "black"
          )
      }
      
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


# Gradient Boosting ----
{
  ## Get Fable Forecast
  {
    fx_list <- readRDS(file.path("Forecasts", "iq_ARIMA_20230804_MAE_7.05.RDS"))
    
    fx.valid_step1 <- fx_list$fx.valid %>% 
      select(yearweek, total_cases.pred) %>% 
      rename(cases_predicted1 = total_cases.pred)
    
    fx.test_step1 <- fx_list$fx.test %>% 
      select(yearweek, total_cases.pred) %>% 
      rename(cases_predicted1 = total_cases.pred)
    
    fx.fitted_step1 <- fx_list$resid.train.all %>% 
      select(yearweek, .fitted) %>% 
      rename(cases_predicted1 = .fitted) %>% 
      mutate(cases_predicted1 = difference(cases_predicted1))
    
    
    train_sub_with.fx <- train_sub %>% 
      as_tibble() %>% 
      left_join(y = fx.fitted_step1, by = "yearweek") %>% 
      mutate(
        total_cases_resid = cases_predicted1 - total_cases
      )
    
    valid_sub_with.fx <- valid_sub %>% 
      as_tibble() %>% 
      left_join(y = fx.valid_step1, by = "yearweek") %>% 
      mutate(
        total_cases_resid = cases_predicted1 - total_cases
      )
    
    test_sub_with.fx <- test_sub %>% 
      as_tibble() %>% 
      left_join(y = fx.test_step1, by = "yearweek") %>% 
      mutate(
        total_cases_resid = cases_predicted1 - total_cases
      )
    
    train.all_sub_with.fx <- bind_rows(train_sub_with.fx, valid_sub_with.fx) %>% 
      filter(cases_cumulative >= 1) %>% 
      mutate(
        total_cases_trans = log(cases_cumulative) - lag(log(cases_cumulative), default = 0)
      ) 
    
    train_sub_with.fx <- train_sub_with.fx %>% 
      inner_join(
        y = train.all_sub_with.fx %>% select(yearweek, city, total_cases_trans),
        by = c("yearweek", "city")
      )
    valid_sub_with.fx <- valid_sub_with.fx %>% 
      inner_join(
        y = train.all_sub_with.fx %>% select(yearweek, city, total_cases_trans),
        by = c("yearweek", "city")
      )
  }
  
  
  ## Recipe ----
  {
    data.recipe <- train_sub_with.fx %>% 
      recipe(data = ., total_cases_trans ~ .) %>% 
      # step_mutate(total_cases = cumsum(total_cases)) %>% 
      step_filter(cases_cumulative >= 100, skip = FALSE) %>% 
      update_role(
        any_of(c(
          setdiff(vars.y, ""),
          "total_cases_resid", "cases_predicted1"
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
      # step_center(all_numeric()) %>%
      # step_scale(all_numeric()) %>%
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
      update_role(week_start_date, new_role = "ID") %>% 
      step_naomit(cases_predicted1)
    
    data.recipe$var_info %>% View()
    data.recipe
    
    baked.data <- bake(prep(data.recipe), new_data = train_sub_with.fx) #%>% View()
    
    baked.data %>% colnames() %>% sort() %>% length()
    
    baked.data %>% 
      select(yearweek, contains("case")) %>% 
      View()
  }
  
  
  # Model Tuning ----
  {
    # Prep
    {
      model.boost <- boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune()) %>% 
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      # model.boost <- boost_tree(trees = 1000, tree_depth = 2, learn_rate = .01) %>% 
      #   set_engine("xgboost") %>% 
      #   set_mode("regression")
      
      workflow.boost <- workflow() %>% 
        add_model(model.boost) %>% 
        add_recipe(
          data.recipe %>% 
            prep()
        )
      
      # Set up grid for lambda/penalty
      (tuning.grid <- grid_regular(
        learn_rate(range = c(-1, -.25)),
        tree_depth(range = c(1, 2)),
        trees(range = c(10000,30000)), 
        levels = c(2, 2, 2)
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
      tuning.boost <- workflow.boost %>%
        tune_grid(folds, grid = tuning.grid)
      
      collect_notes(tuning.boost)$note[1]# %>% View()
      show_notes(.Last.tune.result)
      
      # Tuning Results
      collect_metrics(tuning.boost) #%>% View()
      collect_metrics(tuning.boost) %>% 
        filter(.metric == "rmse") %>% 
        ggplot(aes(
          x = trees, 
          y = mean, 
          # color = interaction(learn_rate, trees)
          color = as.factor(learn_rate)
        )) +
        geom_line() + 
        geom_point() +
        geom_errorbar(aes(ymax = mean + std_err, ymin = mean - std_err)) +
        scale_x_log10() +
        facet_grid(tree_depth ~ learn_rate, scales = "free_x") +
        scale_y_continuous(labels = label_comma()) +
        theme_bw() +
        theme(legend.position = "bottom")
      
      
      ## Best results ----
      (best.param <- select_best(tuning.boost, "rmse"))
      (best.param <- select_by_one_std_err(tuning.boost, "rmse"))
      
      
      
    }
  }
  
  ### Final Fit ----
  {
    # Final workflow
    wf.boost.final <- workflow.boost %>% 
      finalize_workflow(best.param)
    
    # Final fit
    fit.boost <- wf.boost.final %>% 
      fit(train_sub_with.fx)
    
    fit.boost.orig <- extract_fit_engine(fit.boost)
    
    
    xgboost::xgb.importance(model = fit.boost.orig) %>% #View()
      xgboost::xgb.plot.importance(top_n = 20)
  }
  
  ### Forecast ----
  {
    #### Validation ----
    {
      #### Evaluate ----
      {
        model <- fit.boost
        data.valid <- train.all
        yvar <- "total_cases"
        
        
        
        fn_model.validate.reg(
          fit.boost, 
          valid_sub_with.fx,
          "total_cases_trans"
        )
        
        train.all_sub_with.fx %>% 
          bake(prep(data.recipe), new_data = .) %>% 
          # View()
          tsibble(index = yearweek) %>% autoplot(total_cases)
        
        predict.test <- augment(
          fit.boost, 
          new_data = valid_sub_with.fx #%>% bake(prep(data.recipe), new_data = .)
        ) %>% 
          inner_join(
            y = valid_sub_with.fx %>% select(yearweek, city), 
            by = c("yearweek", "city")
          ) %>% 
          select(-any_of(vars.x)) %>% 
          mutate(
            .pred_adj = pmax(.pred, 0),
            # predicted_cases = cumsum(exp(.pred)-1),
            # fx.error = predicted_cases - total_cases
          )
        
        predicted_cases_cum_vec <- c()
        predicted_cases_cum_vec[1] <- last(train_sub_with.fx$cases_cumulative)
        for(i in 2:(nrow(predict.test)+1)){
          predicted_cases_cum_vec[i] <- predicted_cases_cum_vec[i-1]*exp(predict.test$.pred_adj[i - 1])
        }
        
        predicted_cases_vec <- predicted_cases_cum_vec - lag(predicted_cases_cum_vec)
        
        predict.test <- predict.test %>% 
          mutate(
            predicted_cases_cumulative = predicted_cases_cum_vec[-1],
            predicted_cases = predicted_cases_vec[-1],
            fx.error = predicted_cases - total_cases
          )
        
        predict.test %>% 
          select(
            yearweek,cases_cumulative, total_cases, 
            predicted_cases, cases_predicted1, 
            fx.error, .pred, .pred_adj, total_cases_trans
          ) %>% 
          View()
      }
      
      
      #### Analyze ----
      {
        mae_vec(truth = predict.test$total_cases, estimate = predict.test$predicted_cases)
        rmse_vec(truth = predict.test$total_cases, estimate = predict.test$predicted_cases)
        
        rsq_vec(truth = predict.test$total_cases, estimate = predict.test$predicted_cases)
        
        mape_vec(truth = predict.test$total_cases, estimate = predict.test$predicted_cases)
        
        qplot(
          y = predict.test$total_cases, 
          x = predict.test$predicted_cases
        ) +
          geom_abline(slope = 1, intercept = 0, size = .75) +
          geom_smooth() +
          theme_bw()
        
        
        predict.test %>% 
          ggplot(aes(x = yearweek)) +
          geom_line(aes(y = total_cases)) +
          geom_line(aes(y = predicted_cases), color = "red", size = .5, alpha = .5) +
          geom_line(aes(y = cases_predicted1), color = "blue", size = .5, alpha = .5) +
          # scale_y_continuous(trans = "log10") +
          theme_bw()
        
        predict.test %>% 
          ggplot(aes(x = yearweek)) +
          geom_line(aes(y = total_cases_resid)) +
          geom_line(aes(y = .pred), color = "red", size = .5, alpha = .5) +
          # scale_y_continuous(trans = "log10") +
          theme_bw()
      }
      
      
      valid.agg.fx <- valid.resids %>% 
        select(yearweek, city, total_cases, resids, fitted) %>% 
        left_join(
          y = predict.test %>% select(yearweek, city, .pred), 
          by = c("yearweek", "city")
        ) %>% 
        mutate(
          agg_pred = fitted + .pred
        )
      
      mae_vec(valid.agg.fx$total_cases, valid.agg.fx$agg_pred)
      
      valid.agg.fx %>% 
        ggplot(aes(x = yearweek)) +
        geom_line(aes(y = total_cases)) +
        geom_line(aes(y = agg_pred), color = "red", size = .5, alpha = .5) +
        # scale_y_continuous(trans = "log10") +
        theme_bw()
      
      
    }
    
    
    ### Test ----
    {
      
    }
  }
}




