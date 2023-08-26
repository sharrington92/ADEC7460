
library(reticulate)
path_to_python <- file.path("C:\\Users\\sharrington\\AppData\\Local\\Programs\\Python\\Python311\\python.exe")
# path_to_python <- install_python()
virtualenv_create("r-reticulate", python = path_to_python)

library(tensorflow)
# install_tensorflow(envname = "r-reticulate")
install_tensorflow()

# install.packages("keras")
# library(keras)
# install_keras(envname = "r-reticulate")

# library(tensorflow)
tf$constant("Hello Tensorflow!")


# ISLR2 Example
{
  library(ISLR2)
  library(keras)
  library(tidyverse)
  
  Gitters <- na.omit(ISLR2::Hitters)
  n <- nrow(Gitters)
  set.seed(13)
  ntest <- trunc(n / 3)
  testid <- sample(1:n, ntest)
  
  x <- model.matrix(Salary ~ . - 1, data = Gitters) #%>% scale()
  y <- Gitters$Salary
  
  modnn <- keras_model_sequential() %>%
    layer_dense(
      units = 50, activation = "relu",
      input_shape = ncol (x)
    ) %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 1)
  
  modnn %>% 
    compile(loss = "mse", optimizer = optimizer_rmsprop(), metrics = list("mean_absolute_error"))
  
  
  history <- modnn %>% 
    fit(
      x[-testid,], y[-testid], epochs = 1500, batch_size = 32,
      validation_data = list(x[testid,], y[testid])
    )
  
  plot(history)
}




{
  {
    if(!stringr::str_detect(basename(getwd()), "Time Series") & stringr::str_detect(dirname(getwd()), "Time Series")){
      repeat{
        setwd("../")
        if(stringr::str_detect(basename(getwd()), "Time Series")){
          break
        }
      }
    }
    
    if(basename(getwd()) != "Final") setwd(file.path(getwd(), "Final"))
    
    library(doParallel)
    (all_cores <- parallel::detectCores())
    (cl <- makePSOCKcluster(all_cores))
    registerDoParallel(cl)
    
    source(file.path("00_Setup.R"))
  }
  
  
  # Data Preparation
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
      step_fourier(week_start_date, K = 3, period = 365/52) %>% 
      step_normalize(case_rate)
    
    
    
    train.prep <- as_tibble(train) %>% 
      filter(city == "sj") %>% 
      bake(prep(recipe_spec_sj), new_data = .) %>% 
      drop_na()
    
    train.x <- train.prep %>% 
      model.matrix(case_rate ~ ., data = .)
    
    valid.prep <- as_tibble(train.all) %>% 
      filter(city == "sj") %>% 
      filter(week_start_date >= ymd(valid.start.sj) - years(1)) %>%
      bake(prep(recipe_spec_sj), new_data = .) %>% 
      drop_na() 
    valid.x <- valid.prep %>% 
      model.matrix(case_rate ~ ., data = .)
  }
  
  modnn <- keras_model_sequential() %>%
    layer_dense(
      units = 1000, activation = "relu",
      input_shape = ncol(train.x)
    ) %>%
    layer_dropout(rate = 0.6) %>%
    # layer_dense(units = 100, activation = "relu") %>% 
    # layer_dropout(rate = .4) %>% 
    layer_dense(units = 50, activation = "relu") %>% 
    layer_dropout(rate = .2) %>% 
    layer_dense(units = 1)
  
  # modnn <- keras_model_sequential() %>%
  #   layer_simple_rnn(
  #     units = 12,
  #     input_shape = list (ncol(train.x), 3),
  #     dropout = 0.1, recurrent_dropout = 0.1
  #   ) %>%
  #   layer_dense(units = 1)
  
  modnn %>% 
    compile(
      loss = "mse", 
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  
  history <- modnn %>% 
    fit(
      train.x, train.prep$case_rate, epochs = 100, batch_size = 100,
      validation_data = list(
        valid.x, 
        valid.prep$case_rate
      )
    )
  
  plot(history)
  
  npred <- predict(modnn, valid.x)
  mean(abs(valid.prep$case_rate - npred))
  
  valid.prep %>% 
    select(week_start_date, case_rate) %>% 
    bind_cols(tibble(predicted = npred)) %>% 
    pivot_longer(-week_start_date, names_to = "series", values_to = "value") %>% 
    ggplot(aes(x = week_start_date, y = value, color = series)) +
    geom_line()
}






















