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
    # filter(city == "iq") %>% 
    # filter(cases_cumulative > 0) %>%
    recipe(data = ., total_cases ~ .) %>% 
    step_filter(city == "iq") %>% 
    update_role(
      c(setdiff(vars.y, "total_cases")), 
      new_role = "ID"
    ) %>%
    update_role(
      # setdiff(vars.id, "weekofyear"), 
      vars.id,
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
    step_lag(all_numeric_predictors(), lag = 1:10) %>%
    # step_diff(all_numeric_predictors(), lag = 52) %>%
    step_diff(all_numeric_predictors()) %>%
    timetk::step_fourier(week_start_date, K = 4, period = 365/52) %>% 
    step_naomit(all_numeric()) %>% 
    step_corr(all_numeric_predictors(), threshold = .9)
  
  data.recipe$var_info #%>% View()
  data.recipe
  
  bake(prep(data.recipe), new_data = train) #%>% View()
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
    {
      model.boost <- boost_tree(trees = tune(), tree_depth = 1, learn_rate = tune()) %>% 
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      workflow.boost <- workflow() %>% 
        add_model(model.boost) %>% 
        add_recipe(
          data.recipe %>% 
            prep()
        )
    }
    
    ## Fit ----
    {
      # Set up grid for lambda/penalty
      (tuning.grid <- grid_regular(
        learn_rate(range = c(-2, -.5)),
        # tree_depth(range = c(1, 2)),
        trees(range = c(10000,20000)), 
        levels = c(4, 3)
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
      
      # Tune model
      tuning.boost <- workflow.boost %>%
        tune_grid(folds, grid = tuning.grid)
      
      collect_notes(tuning.boost)$note[1]# %>% View()
      
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
        facet_grid(learn_rate ~ ., scales = "free") +
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
    wf.boost.final <- workflow.boost %>% 
      finalize_workflow(best.param)
    
    # Final fit
    fit.boost <- wf.boost.final %>% 
      fit(train)
    
    fit.boost.orig <- extract_fit_engine(fit.boost)
    
    xgboost::xgb.importance(model = fit.boost.orig) %>% #View()
      xgboost::xgb.plot.importance(top_n = 20)
  }
  
  {
    model <- fit.boost
    data.valid <- valid
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
      fit.boost, valid %>% filter(city == "iq"),
      "total_cases"
    )
    
    
    predict.test <- augment(
      fit.boost, new_data = train.all
    ) %>% 
      inner_join(y = valid %>% select(yearweek, city), by = c("yearweek", "city"))
    
    
    
    
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