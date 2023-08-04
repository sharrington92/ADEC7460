# Setup ----
{
  require(tidymodels)
  require(timetk)
  require(randomForest)
  require(xgboost)
  require(doParallel)
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
}

# GLM ----
{
  
  # Fit ----
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
        tsibble(index = yearweek) %>% 
        gg_tsdisplay(.resid, plot_type = "partial")
    }
  }
  
  
  # Forecast ----
  {
    ## Validation ----
    {
      ## Evaluate ----
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
    
    
    # Test ----
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



# Gradient Boosting ----
{
  ## Recipe ----
  {
    data.recipe <- train.resids %>% 
      as_tibble() %>% 
      # select(total_cases, week_start_date, city) %>% 
      recipe(data = ., resids ~ .) %>% 
      step_filter(city == "iq") %>% 
      update_role(
        any_of(vars.y), 
        new_role = "ID"
      ) %>%
      update_role(
        any_of(setdiff(vars.id, "week_start_date")),
        # vars.id,
        new_role = "ID"
      ) %>% 
      # step_mutate(total_cases_bc = log(total_cases + .1527629)) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors()) %>%
      step_nzv() %>%
      timetk::step_smooth(
        # all_numeric_predictors(),
        period = 4,
        precipitation_amt_mm, reanalysis_air_temp_k, reanalysis_avg_temp_k,
        reanalysis_min_air_temp_k, reanalysis_precip_amt_kg_per_m2,
        reanalysis_max_air_temp_k, reanalysis_tdtr_k, reanalysis_relative_humidity_percent
      ) %>%
      step_arrange() %>% 
      step_lag(all_numeric_predictors(), lag = 1:10) %>%
      
      # update_role(total_cases, new_role = "predictor") %>%
      # step_lag(total_cases, lag = 1:10, skip = T) %>%
      # update_role(total_cases, new_role = "outcome") %>%
      
      step_diff(all_numeric_predictors(), lag = 52) %>%
      step_diff(all_numeric_predictors()) %>%
      # step_diff(total_cases) %>%
      timetk::step_fourier(week_start_date, K = 4, period = 365/52) %>%
      step_naomit(all_numeric()) %>%
      step_timeseries_signature(week_start_date) %>%
      step_dummy(all_nominal_predictors()) %>% 
      step_corr(all_numeric_predictors(), threshold = .9) %>% 
      update_role(week_start_date, new_role = "ID")
    
    data.recipe$var_info %>% View()
    data.recipe
    
    bake(prep(data.recipe), new_data = as_tibble(train)) #%>% View()
  }
  
  
  # Model Tuning ----
  {
    # Prep
    {
      model.boost <- boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune()) %>% 
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      model.boost <- boost_tree(trees = 1000, tree_depth = 2, learn_rate = .01) %>% 
        set_engine("xgboost") %>% 
        set_mode("regression")
      
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
        levels = c(3, 2, 3)
      ))
      
      # folds <- vfold_cv(data.train, v = 2)
      (folds <- rolling_origin(
        train %>% filter(city == "iq"), 
        cumulative = F,
        initial = 52*4, assess = 50, skip = 50
      ))
      
      all_cores <- parallel::detectCores(logical = FALSE)
      cl <- makePSOCKcluster(all_cores)
      registerDoParallel(cl)
    }
    
    
    ## Fit ----
    {
      # Tune model
      tuning.boost <- workflow.boost %>%
        tune_grid(folds, grid = tuning.grid)
      
      collect_notes(tuning.boost)$note[1]# %>% View()
      
      
      # Tuning Results
      collect_metrics(tuning.boost) %>% View()
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
        facet_grid(tree_depth ~ ., scales = "free_x") +
        scale_y_continuous(labels = label_comma()) +
        theme_bw() +
        theme(legend.position = "bottom")
      
      
      ## Best results ----
      (best.param <- select_best(tuning.boost, "rmse"))
      (best.param <- select_by_one_std_err(tuning.boost, "rmse"))
      
      
      
    }
  }
  
  # Final Fit ----
  {
    # Final workflow
    wf.boost.final <- workflow.boost #%>% 
    # finalize_workflow(best.param)
    
    # Final fit
    fit.boost <- wf.boost.final %>% 
      fit(train.resids)
    
    fit.boost.orig <- extract_fit_engine(fit.boost)
    
    
    xgboost::xgb.importance(model = fit.boost.orig) %>% #View()
      xgboost::xgb.plot.importance(top_n = 20)
  }
  
  # Forecast ----
  {
    ## Validation ----
    {
      ## Evaluate ----
      {
        model <- fit.boost
        data.valid <- train.all
        yvar <- "total_cases"
        
        
        
        fn_model.validate.reg(
          fit.boost, 
          valid.resids %>% filter(city == "iq"),
          "resids"
        )
        
        
        predict.test <- augment(
          fit.boost, 
          new_data = valid.resids
        ) %>% 
          inner_join(
            y = valid.resids %>% select(yearweek, city) %>% filter(city == "iq"), 
            by = c("yearweek", "city")
          ) %>% 
          mutate(
            fx.error = .pred - total_cases
          )
      }
      
      
      ## Analyze ----
      {
        mae_vec(truth = -predict.test$resids, estimate = predict.test$.pred)
        rmse_vec(truth = -predict.test$resids, estimate = predict.test$.pred)
        
        rsq_vec(truth = predict.test$resids, estimate = predict.test$.pred)
        
        mape_vec(truth = predict.test$resids, estimate = predict.test$.pred)
        
        qplot(x = -predict.test$resids, y = predict.test$.pred) +
          geom_abline(slope = 1, intercept = 0, size = .75) +
          geom_smooth() +
          theme_bw()
        
        
        predict.test %>% 
          ggplot(aes(x = yearweek)) +
          geom_line(aes(y = resids)) +
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
    
    
    # Test ----
    {
      
    }
  }
}




