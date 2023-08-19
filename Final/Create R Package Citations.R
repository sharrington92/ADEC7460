

# "https://bookdown.org/yihui/rmarkdown-cookbook/write-bib.html"



knitr::write_bib(
  c("tidyverse", "ggplot2", "fpp3", "modeltime", "tidymodels", "timetk", "xgboost"), 
  "packages.bib"
)
