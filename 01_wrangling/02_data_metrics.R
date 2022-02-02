library(tidyverse)
library(readxl)
library(lubridate)
library(tidyquant)
library(fs)

# 1.1 Load the data and source the function ----
gdp_industry_wrangled_tbl <- read_rds(file = "00_data/gdp_industry_wrangled.rds")
source(file = "00_functions/all_function.R")



# Getting the metrics ----
metrics_data <- gdp_industry_wrangled_tbl %>% 
    time_picker_gdp(time_unit = "quarter") 

write_rds(metrics_data, file = "00_data/metrics_data.rds")
metrics_data <- read_rds(file = "00_data/metrics_data.rds") 

## First table ----

### Metric 1 - Industry GDP ----
metrics_data %>% 
    filter(year == "2005" & quarter == "1") %>% 
    filter(industry %in% "Information") %>% 
    pull(gdp) %>% 
    scales::number(big.mark = ",", suffix = "M", prefix = "$")
    
### Metric 2 - Industry contribution to GDP ----
metrics_data %>% 
    filter(year == "2005" & quarter == "1") %>% 
    filter(industry %in% "Information") %>% 
    pull(ind_cont_percent) 

### Metric 3 - Contribution rank ----
metrics_data %>% 
    filter(year == "2005" & quarter == "1") %>% 
    filter(industry %in% "Information") %>% 
    pull(rank)

### Metric 4 - Growth Rate ----
metrics_data %>% filter(industry %in% "Information") %>% 
    select(industry, gdp, format_date, quarter) %>%
    group_by(industry) %>%
    mutate(lag_1 = lag(gdp, n = 1)) %>%
    mutate(lag_1 = case_when(is.na(lag_1) ~ gdp,
                             TRUE ~ lag_1)) %>%
    summarise(
        rate = (gdp - lag_1) / lag_1,
        row                   = row_number(),
        format_date           = format_date[row],
        quarter               = quarter[row],
    ) %>%
    ungroup() %>%
    mutate(rate_text = scales::percent(rate, accuracy = 0.1)) %>% 
    mutate(year = year(format_date)) %>%
    filter(year == "2005" & quarter == "2") %>% 
    
    pull(rate_text)







# Trial and Error ----

year_quarter <- function(data, time_year = "2005", time_quarter = "1"){
    
    if(!time_quarter %in% time_quarter){
        data %>% 
            filter(year == time_year) %>% 
            filter(industry %in% "Information") %>% 
            pull(gdp) %>% 
            mean()
        
    } else {
        
        data %>% 
            filter(year == time_year & quarter == time_quarter) %>% 
            filter(industry %in% "Information") %>% 
            pull(gdp)
        
    }
    
}

metrics_data %>% year_quarter(time_year = "2005", time_quarter = "5")


metrics_data %>% 
    filter(year == "2005") %>% 
    filter(industry %in% "Information") %>% 
    pull(gdp) %>% 
    mean()


metrics_data %>% 
    filter(year == "2005" & quarter == "1") %>% 
    filter(industry %in% "Information") %>% 
    pull(gdp)

return(trial_data)
















