# Setup ----
{
  library(tidymodels)
  library(timetk)
  
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


# Create recipe
{
  data.recipe <- train %>% 
    filter(city == "iq") %>% 
    recipe(data = ., total_cases ~ .) %>% 
    # step_rm(tgb.vibr.total.diff) %>% 
    update_role(
      setdiff(vars.y, "total_cases"), 
      new_role = "ID"
    ) %>%
    update_role(
      setdiff(vars.id, "weekofyear"), 
      new_role = "ID"
    ) %>% 
    # step_log() %>%
    step_center(all_numeric_predictors()) %>% 
    step_scale(all_numeric_predictors()) %>% 
    step_nzv() %>% 
    # step_rm(tgb.vibr.total, ggb.vibr.total, ggb.vibr.x, ggb.vibr.y, tgb.vibr.x, tgb.vibr.y) %>%
    step_diff(all_numeric_predictors()) %>%
    step_naomit(all_predictors()) %>% 
    # step_BoxCox(total_cases, lambdas = lambda.iq) %>% 
    timetk::step_fourier(week_start_date, K = 4, period = 365/52) %>% 
    step_lag(all_numeric_predictors(), lag = 1:10) %>% 
    step_naomit(all_numeric())
  
  data.recipe$var_info #%>% View()
  data.recipe
  
  bake(prep(data.recipe), new_data = train) #%>% View()
}


# Random Forest
{
  
  # Fit and Tune
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
    
    # Fit
    {
      # Set up grid for lambda/penalty
      tuning.grid <- grid_regular(
        mtry(range = c(7,10)), trees(range = c(100, 1000)), levels = 4
      )
      
      # folds <- vfold_cv(data.train, v = 5)
      folds <- rolling_origin(
        as_tibble(train), cumulative = T,
        initial = 52*5, assess = 52*2#, skip = 3000
      )
      folds %>% dim()
      doParallel::registerDoParallel()
      
      # Tune model
      rf.tuning <- workflow.rf %>%
        tune_grid(folds, grid = tuning.grid)
      
      /collect_notes(rf.tuning)$note[1]# %>% View()
      
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



# Gradient Boosting
{
  
  # Fit and Tune
  {
    model.boost <- boost_tree(trees = tune(), tree_depth = tune(), learn_rate = .1) %>% 
      set_engine("xgboost") %>% 
      set_mode("regression")
    
    workflow.boost <- workflow() %>% 
      add_model(model.boost) %>% 
      add_recipe(
        data.recipe %>% 
          prep()
      )
    
    # Fit
    {
      # Set up grid for lambda/penalty
      tuning.grid <- grid_regular(
        # learn_rate(),
        tree_depth(range = c(1, 2)),
        trees(range = c(500,5000)), 
        levels = c(2, 7)
      )
      
      # folds <- vfold_cv(data.train, v = 2)
      folds <- rolling_origin(
        data.train, cumulative = F,
        initial = 1440*5, assess = 240*5, skip = 3000
      )
      
      doParallel::registerDoParallel()
      
      # Tune model
      tuning.boost <- workflow.boost %>%
        tune_grid(folds, grid = tuning.grid)
      
      collect_notes(tuning.boost)$note[1]# %>% View()
      
      # Tuning Results
      collect_metrics(tuning.boost) #%>% View()
      collect_metrics(tuning.boost) %>% 
        ggplot(aes(x = trees, y = mean, color = .metric)) +
        geom_line() + 
        geom_point() +
        scale_x_log10() +
        facet_grid(.metric ~ tree_depth, scales = "free") +
        theme_bw() +
        theme(legend.position = "bottom")
      
      
      # Best results
      best.param <- select_best(tuning.boost, "rmse")
      best.param <- select_by_one_std_err(tuning.boost, "rmse")
      
      
      
      }
  }
  
  
  # Fit final model
  {
    # Final workflow
    wf.boost.final <- workflow.boost %>% 
      finalize_workflow(best.param)
    
    # Final fit
    fit.boost <- wf.boost.final %>% 
      fit(data.train)
    
    fit.boost.orig <- extract_fit_engine(fit.boost)
    
    xgboost::xgb.importance(model = fit.boost.orig) %>% 
      xgboost::xgb.plot.importance()
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
    
    fn_model.validate.reg(fit.boost, data.test)
    
    
    predict.test <- augment(fit.boost, new_data = data.test)
    
    
    
    rmse_vec(truth = data.test$tgb.vibr.total.diff, estimate = predict.test$.pred)
    
    rsq_vec(truth = data.test$tgb.vibr.total.diff, estimate = predict.test$.pred)
    
    mape_vec(truth = data.test$tgb.vibr.total.diff, estimate = predict.test$.pred)
    
    qplot(x = data.test$tgb.vibr.total.diff, y = predict.test$.pred) +
      geom_abline(slope = 1, intercept = 0, size = .75) +
      geom_smooth() +
      theme_bw()
    
    
    data.test %>% 
      mutate(
        predicted = (predict.test$.pred) + data.test$tgb.vibr.total[1]
      ) %>% 
      # filter((time) %within% interval(ymd("2022-02-09", ymd("2022-02-14"))))
      # filter(time >= ymd_h("2022-02-9 00")) %>%
      # filter(time <= ymd_h("2022-02-15 24")) %>% 
      ggplot(aes(x = time)) +
      geom_line(aes(y = tgb.vibr.total)) +
      geom_line(aes(y = predicted), color = "red", size = .5, alpha = .5) +
      theme_bw()
    
    
    varImpPlot(rf.fit.diff)
    
  }