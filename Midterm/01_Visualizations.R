# Setup
{
  library(tidyverse)
  library(fpp3)
  
  data <- read_csv("Midterm/Columbia River Flows.csv") %>% 
    mutate(
      date = as.Date(datetime),
      year = year(date),
      month = month(date)
    )
}

# 
{
  data %>% 
    filter(year(datetime) >= 2018) %>% 
    ggplot(aes(x = datetime, y = flow_cfs)) +
    geom_line() +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_bw()
}

{
  data %>% 
    group_by(date, year, month) %>% 
    summarize(flow_cfs = mean(flow_cfs)) %>% 
    filter(year(date) >= 2015) %>%
    ggplot(aes(x = date, y = flow_cfs)) +
    geom_line() +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_bw()
}


{
  data %>% 
    group_by(year, month) %>% 
    summarize(flow_cfs = mean(flow_cfs)) %>% 
    mutate(date = ymd(paste(year, month, 1, sep = "-"))) %>% 
    # filter(year(date) >= 2015) %>%
    ggplot(aes(x = date, y = flow_cfs)) +
    facet_grid(. ~ month) +
    geom_line() +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_bw()
}