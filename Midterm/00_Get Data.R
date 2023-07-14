library(RCurl)
library(tidyverse)

# https://waterdata.usgs.gov/monitoring-location/12472800/#parameterCode=00060&period=P7D
# https://waterservices.usgs.gov/rest/IV-Test-Tool.html

# Columbia River Below PRD: 12472800
# Columbia River at International Boundary: 12399500
# Columbia River at Grand Coulee: 12436000
# Crab Creek Near Beverly, WA: 12472600


# RDB
{
  
  
  the_url <- "https://nwis.waterservices.usgs.gov/nwis/iv/?format=rdb&sites=12472800&startDT=2010-01-01&endDT=2023-07-02&parameterCd=00060,00065&siteStatus=all"
  
  
  content <- getBinaryURL(the_url)
  data <- read_delim(content, skip = 29, delim = "\t") %>% 
    filter(agency_cd != "5s") %>%
    rename_with(~c("agency_cd", "site_no", "datetime", "tz", "flow_cfs", "flow_cfs_cd", "gage_feet", "gage_feet_cd")) %>% 
    select(-contains("cd")) %>% 
    mutate(
      datetime = ymd_hm(datetime),
      across(c(flow_cfs, gage_feet), as.numeric), 
      site_name = "Below PRD"
    )
  
  write_csv(data, file.path("Midterm", "Columbia River Flows.csv"))
}