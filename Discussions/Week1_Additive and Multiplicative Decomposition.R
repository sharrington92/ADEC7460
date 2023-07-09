# Setup
{
  library(tidyverse)
  library(readxl)
  library(fpp3)
  
  # https://data.world/us-doe-gov/f047b651-160c-447b-8c96-1da78719f914/workspace/file?filename=energy-consumption-1.xls
  data <- read_xlsx("Discussions/energy-consumption-1.xlsx", skip = 9) %>% 
    filter(!is.na(Month)) %>% 
    mutate(across(-Month, as.numeric)) %>% 
    mutate(Month = tsibble::yearmonth(ymd(Month))) %>% 
    tsibble() %>% 
    select(1:3) %>% 
    rename_with(~c("month", "residential.primary", "residential.total"))
}



# Classical Decomposition
{
  # Additive Decomposition
  {
    data %>% 
      model(
        classical_decomposition(residential.total, type = "additive")
      ) %>% 
      components() %>% 
      autoplot() +
      theme_bw()
  }
  
  
  # Multiplicative Decomposition
  {
    data %>% 
      model(
        classical_decomposition(residential.total, type = "multiplicative")
      ) %>% 
      components() %>% 
      autoplot() +
      theme_bw()
  }
}


# STL Decomposition
{
  # Multiplicative Decomposition
  {
    data %>% 
      filter(year(month) < 2020) %>% 
      model(
        STL(box_cox(residential.total, .5) ~ trend(window = 39) + season(window = 13), robust = TRUE)
      ) %>% 
      components() %>% 
      autoplot() +
      theme_bw()
  }
}

{
  data.annual <- data %>% 
    as_tibble() %>% 
    mutate(year = year(month)) %>% 
    group_by(year) %>% 
    summarize(
      max = max(residential.total),
      min = min(residential.total),
      sd = sd(residential.total)
    ) %>% 
    mutate(diff = max - min) %>% 
    filter(year < 2020) 
  
  data.annual %>% 
    ggplot(aes(x = year, y = diff)) +
    geom_line(linetype = "dotted") + 
    geom_point(size = 2) +
    geom_smooth() +
    theme_bw() +
    ggtitle("Difference between maximum and minimum monthly consumption in a calendar year")
  
  data.annual %>% 
    select(year, max, min) %>% 
    pivot_longer(-year, names_to = "type", values_to = "value") %>% 
    ggplot(aes(x = year, y = value, color = type)) +
    geom_line(linetype = "dotted") + 
    geom_point(size = 2) +
    theme_bw() +
    ggtitle("Chart of Annual Max & Min")
  
  data.annual %>% 
    select(year, max, min) %>% 
    ggplot(aes(x = year)) +
    geom_ribbon(aes(
      ymin = min, ymax = max
    )) +
    theme_bw() +
    ggtitle("Ribbon chart of Annual Max & Min")
}