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
}


# Create recipe ----
{
  data.recipe <- train %>% 
    as_tibble() %>% 
    # select(total_cases, week_start_date, city) %>% 
    recipe(data = ., total_cases ~ .) %>% 
    step_filter(city == "iq") %>% 
    update_role(
      any_of(c(setdiff(vars.y, "total_cases"))), 
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


# Random Forest ----
{
  
  ## Fit and Tune ----
  {
    {
      model.rf <- rand_forest(mtry = tune(), trees = tune()) %>% 
        set_engine("randomForest") %>% 
        set_mode("regression")
      
      workflow.rf <- workflow() %>% 
        add_model(model.rf) %>% 
        add_recipe(
          data.recipe %>% 
            prep()
        )
    }
    
    # Fit ----
    {
      # Set up grid for lambda/penalty
      (tuning.grid <- grid_regular(
        mtry(range = c(7,25)), trees(range = c(100, 500)), 
        levels = c(3,4)
      ))
      
      # folds <- vfold_cv(data.train, v = 2)
      (folds <- rolling_origin(
        train %>% filter(city == "iq"), 
        cumulative = F,
        initial = 52*4, assess = 50, skip = 50
      ))
      folds %>% dim()
      
      all_cores <- parallel::detectCores(logical = FALSE)
      cl <- makePSOCKcluster(all_cores)
      registerDoParallel(cl)
      
      # Tune model
      rf.tuning <- workflow.rf %>%
        tune_grid(folds, grid = tuning.grid)
      
      collect_notes(rf.tuning)$note[1]# %>% View()
      
      # Tuning Results
      collect_metrics(rf.tuning) %>% 
        ggplot(aes(x = mtry, y = mean, color = .metric)) +
        geom_line() + 
        geom_point() +
        scale_x_log10() +
        facet_grid(.metric ~ ., scales = "free") +
        theme_bw() +
        theme(legend.position = "bottom")
      
      
      # Best results
      best.param <- select_best(rf.tuning, "rmse")
      
      
      
      }
  }
  
  
  # Fit final model
  {
    # Final workflow
    wf.rf.final <- workflow.rf %>% 
      finalize_workflow(best.param)
    
    # Final fit
    rf.fit <- wf.rf.final %>% 
      fit(data.train)
    
    varImpPlot(extract_fit_engine(rf.fit))
  }
  
  {
    fn_model.validate.reg <- function(model, data.valid){
      
      model.pred <- augment(model, data.valid)
      
      fx.rmse <- rmse_vec(truth = model.pred$tgb.vibr.total, estimate = model.pred$.pred)
      
      fx.rsq <- rsq_vec(truth = model.pred$tgb.vibr.total, estimate = model.pred$.pred)
      
      fx.mape <- mape_vec(truth = model.pred$tgb.vibr.total, estimate = model.pred$.pred)
      
      fx.plot <- qplot(x = model.pred$tgb.vibr.total, y = model.pred$.pred) +
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
    
    
    
    rf.fit
    
    rf.fit %>% 
      extract_fit_parsnip() 
    
    rf.fit.orig <- extract_fit_engine(rf.fit)
    summary(rf.fit.orig)
    
    # Model 2: Differenced
    {
      # rf.fit.diff <- rf.fit.orig
      
      qplot(x = rf.fit.diff$y, y = rf.fit.diff$predicted, geom = "point") +
        geom_abline(intercept = 0, slope = 1) +
        theme_bw()
      
      predict.test.diff <- predict(rf.fit.diff, newdata = data.test)
      
      
      
      rmse_vec(truth = data.test$tgb.vibr.total.diff, estimate = predict.test.diff)
      
      rsq_vec(truth = data.test$tgb.vibr.total.diff, estimate = predict.test.diff)
      
      mape_vec(truth = data.test$tgb.vibr.total.diff, estimate = predict.test.diff)
      
      qplot(x = data.test$tgb.vibr.total.diff, y = predict.test.diff) +
        geom_abline(slope = 1, intercept = 0, size = .75) +
        theme_bw()
      
      
      data.test %>% 
        mutate(
          predicted = (predict.test.diff) + data.test$tgb.vibr.total[1]
        ) %>% 
        # filter((time) %within% interval(ymd("2022-02-09", ymd("2022-02-14"))))
        filter(time >= ymd_h("2022-02-9 00")) %>%
        filter(time <= ymd_h("2022-02-15 24")) %>% 
        ggplot(aes(x = time)) +
        geom_line(aes(y = tgb.vibr.total)) +
        geom_line(aes(y = predicted), color = "red", size = .5, alpha = .5) +
        theme_bw()
      
      
      varImpPlot(rf.fit.diff)
    }
    
  }
}



# Gradient Boosting ----
{
  
  ## Fit and Tune ----
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
  
  
  ## Fit final model ----
  {
    # Final workflow
    wf.boost.final <- workflow.boost #%>% 
      # finalize_workflow(best.param)
    
    # Final fit
    fit.boost <- wf.boost.final %>% 
      fit(train)
    
    fit.boost.orig <- extract_fit_engine(fit.boost)
    
    
    xgboost::xgb.importance(model = fit.boost.orig) %>% #View()
      xgboost::xgb.plot.importance(top_n = 20)
  }
  
  {
    model <- fit.boost
    data.valid <- train.all
    yvar <- "total_cases"
    
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
    
    fn_model.validate.reg(
      fit.boost, 
      valid %>% filter(city == "iq"),
      "total_cases"
    )
    
    
    predict.test <- augment(
      fit.boost, 
      new_data = train.all
    ) %>% 
      inner_join(
        y = valid %>% select(yearweek, city) %>% filter(city == "iq"), 
        by = c("yearweek", "city")
      ) %>% 
      mutate(
        fx.error = .pred - total_cases
      )
    
    # train.all %>% 
    #   bake(prep(data.recipe), new_data = .)
    
    
    
    
    mae_vec(truth = predict.test$total_cases, estimate = predict.test$.pred)
    rmse_vec(truth = predict.test$total_cases, estimate = predict.test$.pred)
    
    rsq_vec(truth = predict.test$total_cases, estimate = predict.test$.pred)
    
    mape_vec(truth = predict.test$total_cases, estimate = predict.test$.pred)
    
    qplot(x = predict.test$total_cases, y = predict.test$.pred) +
      geom_abline(slope = 1, intercept = 0, size = .75) +
      geom_smooth() +
      theme_bw()
    
    
    predict.test %>% 
      ggplot(aes(x = yearweek)) +
      geom_line(aes(y = total_cases)) +
      geom_line(aes(y = .pred), color = "red", size = .5, alpha = .5) +
      # scale_y_continuous(trans = "log10") +
      theme_bw()
    
    
    # ARIMA Forecast Errors
    {
      fx.error <- predict.test %>% 
        tsibble(index = yearweek, key = city) %>% 
        select(yearweek, city, fx.error) %>% 
        rename(.pred = fx.error)
      
      fx.error %>% 
        autoplot(fx.error)
      
      resids.boost <- augment(
        fit.boost, new_data = train %>% filter(city == "iq")
      ) %>% 
        select(yearweek, city, total_cases, .pred) %>% 
        tsibble(index = yearweek, key = city)
      
      resids.boost %>% 
        gg_season(.pred)
      
      resids.boost %>% 
        autoplot(.pred) +
        autolayer(fx.error, .pred, color = "red")
      
      fit.resid.ar <- resids.boost %>% 
        model(ARIMA(.pred))
      
      fit.resid.ar %>% 
        report()
      
      fit.resid.ar %>% 
        forecast(
          new_data = fx.error
        ) %>% #View()
        autoplot(level = NULL) 
        autolayer(fx.error, .pred)
    }
  }
}


# Linear Regression ----
{
  
  ## Fit and Tune ----
  {
    # Prep
    {
      model.lm <- linear_reg() %>% 
        set_engine("lm") %>% 
        set_mode("regression")
      
      workflow.lm <- workflow() %>% 
        add_model(model.lm) %>% 
        add_recipe(
          data.recipe %>% 
            prep()
        )
      
      # # Set up grid for lambda/penalty
      # (tuning.grid <- grid_regular(
      #   learn_rate(range = c(-1, -.25)),
      #   tree_depth(range = c(1, 2)),
      #   trees(range = c(10000,30000)), 
      #   levels = c(3, 2, 3)
      # ))
      
      # folds <- vfold_cv(data.train, v = 2)
      (folds <- rolling_origin(
        train %>% filter(city == "iq"), 
        cumulative = F,
        initial = 52*4, assess = 50, skip = 50
      ))
      
      # all_cores <- parallel::detectCores(logical = FALSE)
      # cl <- makePSOCKcluster(all_cores)
      # registerDoParallel(cl)
    }
    
    {
      FORECAST_HORIZON <- nrow(valid %>% filter(city == "iq"))
      
      train_extended <- train %>%
        filter(city == "iq") %>% 
        as_tibble() %>% 
        future_frame(
          .date_var = week_start_date,
          .length_out = FORECAST_HORIZON,
          .bind_data  = TRUE
        ) %>% 
        tk_augment_lags(total_cases, .lags = 1:FORECAST_HORIZON)
      
      train_extended %>% View()
    }
    
    ## Fit ----
    {
      
      
      # Tune model
      tuning.lm <- workflow.lm %>%
        tune_grid(folds)
      
      collect_notes(tuning.lm)$note[1]# %>% View()
      
      
      # Tuning Results
      collect_metrics(tuning.lm) #%>% View()
      # collect_metrics(tuning.lm) %>% 
      #   filter(.metric == "rmse") %>% 
      #   ggplot(aes(
      #     x = trees, 
      #     y = mean, 
      #     # color = interaction(learn_rate, trees)
      #     color = as.factor(learn_rate)
      #   )) +
      #   geom_line() + 
      #   geom_point() +
      #   geom_errorbar(aes(ymax = mean + std_err, ymin = mean - std_err)) +
      #   scale_x_log10() +
      #   facet_grid(tree_depth ~ ., scales = "free_x") +
      #   scale_y_continuous(labels = label_comma()) +
      #   theme_bw() +
      #   theme(legend.position = "bottom")
      
      
      ## Best results ----
      (best.param <- select_best(tuning.lm, "rmse"))
      (best.param <- select_by_one_std_err(tuning.lm, "rmse"))
      
      
      
    }
  }
  
  
  ## Fit final model ----
  {
    
    lag_roll_transformer <- function(data){
      data %>%
        # Lags
        tk_augment_lags(value, .lags = 1:12) %>%
        # Rolling Features
        mutate(rolling_mean_12 = lag(slide_dbl(
          value, .f = mean, .before = 12, .complete = FALSE
        ), 1))
    }
    
    # Final fit
    fit.lm <- workflow.lm %>% 
      fit(train_extended) %>% 
      recursive(
        transform = lag_roll_transformer,
        train_tail = tail(as_tibble(train), FORECAST_HORIZON)
      )
    
    fit.lm.orig <- extract_fit_engine(fit.lm)
    
    summary(fit.lm.orig)
  
  }
  
  {
    model <- fit.lm
    data.valid <- train.all
    yvar <- "total_cases"
    
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
    
    fn_model.validate.reg(
      fit.lm, 
      valid %>% filter(city == "iq"),
      "total_cases"
    )
    
    
    predict.test <- augment(
      fit.lm, 
      new_data = train_extended
    ) %>% 
      inner_join(y = valid %>% select(yearweek, city), by = c("yearweek", "city"))
    
    train.all %>% 
      as_tibble() %>% 
      bake(prep(data.recipe), new_data = .) %>% 
      View()
    
    
    
    
    mae_vec(truth = valid$total_cases, estimate = predict.test$.pred)
    rmse_vec(truth = valid$total_cases, estimate = predict.test$.pred)
    
    rsq_vec(truth = valid$total_cases, estimate = predict.test$.pred)
    
    mape_vec(truth = valid$total_cases, estimate = predict.test$.pred)
    
    qplot(x = valid$total_cases, y = predict.test$.pred) +
      geom_abline(slope = 1, intercept = 0, size = .75) +
      geom_smooth() +
      theme_bw()
    
    
    predict.test %>% 
      ggplot(aes(x = yearweek)) +
      geom_line(aes(y = total_cases)) +
      geom_line(aes(y = .pred), color = "red", size = .5, alpha = .5) +
      # scale_y_continuous(trans = "log10") +
      theme_bw()
    
  }
}




# AR Boosting ----
{
  
  ## Fit and Tune ----
  {
    # Prep
    {
      # model.arboost <- arima_boost(
      #   seasonal_period = "1 week",
      #   non_seasonal_ar = tune(), non_seasonal_ma = tune(), 
      #   seasonal_ar = tune(), #non_seasonal_differences = 1,
      #   trees = 10000, tree_depth = 1, learn_rate = tune()
      # ) %>% 
      #   set_engine("auto_arima_xgboost") %>% 
      #   set_mode("regression")
      
      model.arboost <- arima_boost(
        seasonal_period = "1 week",
        non_seasonal_ar = 2, non_seasonal_ma = 1, 
        seasonal_ar = 0, #non_seasonal_differences = 1,
        trees = 100, tree_depth = 1, learn_rate = .1
      ) %>% 
        set_engine("auto_arima_xgboost") %>% 
        set_mode("regression")
      
      workflow.arboost <- workflow() %>% 
        add_model(model.arboost) %>% 
        add_recipe(
          data.recipe %>% 
            prep()
        )
      
      # Set up grid for lambda/penalty
      (tuning.grid <- grid_regular(
        learn_rate(range = c(-1, -.25)),
        # tree_depth(range = c(1, 2)),
        # trees(range = c(1000,10000)), 
        non_seasonal_ar(), 
        non_seasonal_ma(),
        seasonal_ar(c(0:1)),
        levels = c(2, 3, 3, 2)
      ))
      
      # folds <- vfold_cv(data.train, v = 2)
      (folds <- rolling_origin(
        train %>% filter(city == "iq"), 
        cumulative = F,
        initial = 52*4, assess = 50, skip = 100
      ))
      
      all_cores <- parallel::detectCores(logical = FALSE)
      cl <- makePSOCKcluster(all_cores)
      registerDoParallel(cl)
    }
    
    ## Fit ----
    {
      
      
      # Tune model
      tuning.arboost <- workflow.arboost %>%
        tune_grid(folds, grid = tuning.grid)
      
      collect_notes(tuning.arboost)$note[1]# %>% View()
      
      
      # Tuning Results
      collect_metrics(tuning.arboost)# %>% View()
      collect_metrics(tuning.arboost) %>% 
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
      (best.param <- select_best(tuning.arboost, "rmse"))
      (best.param <- select_by_one_std_err(tuning.arboost, "rmse"))
      
      
      
    }
  }
  
  
  ## Fit final model ----
  {
    # Final workflow
    wf.arboost.final <- workflow.arboost %>% 
      finalize_workflow(best.param)
    
    # Final fit
    fit.arboost <- wf.arboost.final %>% 
      fit(train)
    
    fit.arboost <- arima_boost(
      min_n = 2,
      learn_rate = 0.015
    ) %>%
      set_engine(engine = "auto_arima_xgboost") %>%
      fit(
        total_cases ~ week_start_date,
        data = train
      )
    
    fit.arboost <- workflow.arboost %>%
      fit(as_tibble(train))
    
    fit.arboost.orig <- extract_fit_engine(fit.arboost)
    
    
    xgboost::xgb.importance(model = fit.arboost.orig) %>% #View()
      xgarboost::xgb.plot.importance(top_n = 20)
  }
  
  {
    model <- fit.arboost
    data.valid <- train.all
    yvar <- "total_cases"
    
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
    
    fn_model.validate.reg(
      fit.arboost, 
      valid %>% filter(city == "iq"),
      "total_cases"
    )
    
    
    predict.test <- augment(
      fit.arboost, 
      new_data = as_tibble(train.all)
    ) %>% 
      inner_join(y = valid %>% select(yearweek, city), by = c("yearweek", "city"))
    
    train.all %>% 
      bake(prep(data.recipe), new_data = .)
    
    
    
    
    mae_vec(truth = valid$total_cases, estimate = predict.test$.pred)
    rmse_vec(truth = valid$total_cases, estimate = predict.test$.pred)
    
    rsq_vec(truth = valid$total_cases, estimate = predict.test$.pred)
    
    mape_vec(truth = valid$total_cases, estimate = predict.test$.pred)
    
    qplot(x = valid$total_cases, y = predict.test$.pred) +
      geom_abline(slope = 1, intercept = 0, size = .75) +
      geom_smooth() +
      theme_bw()
    
    
    predict.test %>% 
      ggplot(aes(x = yearweek)) +
      geom_line(aes(y = total_cases)) +
      geom_line(aes(y = .pred), color = "red", size = .5, alpha = .5) +
      # scale_y_continuous(trans = "log10") +
      theme_bw()
    
  }
}