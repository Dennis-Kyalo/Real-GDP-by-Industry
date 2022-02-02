# Libraries ----

library(tidyverse)
library(readxl)
library(lubridate)
library(tidyquant)
library(fs)

# 1.Data Wrangling ----
## 1.0 GDP by Industry ----

gdp_industry_all_tbl <- read_xlsx(path = "00_data/GDP_by_Industry.xlsx",sheet = "Sheet1")


wrangle_industry_tbl <- function(data) {
    gdp_industry_tbl <- data %>%
        
        
        # renaming ...2 column to industry
        rename("industry" = "...2") %>%
        
        # filter the major categories
        filter( 
            industry %in% c(
                "Gross domestic product",
                "Agriculture, forestry, fishing, and hunting",
                "Mining",
                "Utilities",
                "Construction",
                "Manufacturing",
                "Wholesale trade",
                "Retail trade",
                "Transportation and warehousing",
                "Information",
                "Finance, insurance, real estate, rental, and leasing",
                "Professional and business services",
                "Educational services, health care, and social assistance",
                "Arts, entertainment, recreation, accommodation, and food services",
                "Other services, except government",
                "Government (Federal and State)"
            )
        ) %>%
        
        # gathering the date
        pivot_longer(
            cols      = c(-Line,-industry),
            names_to  = "year",
            values_to = "GDP"
        ) %>%
        
        # cleaning the column names
        janitor::clean_names(.) %>%
        
        # Separating the year column to year and quarter
        separate(col  = "year",
                 into = c("year", "quarter"),
                 sep  = "[^\\d{4}]") %>%
        
        # converting the quarters into quarterly dates
        mutate(
            quarterly_date = case_when(
                quarter    == 1 ~ "01-01",
                quarter    == 2 ~ "04-01",
                quarter    == 3 ~ "07-01",
                quarter    == 4 ~ "10-01"
            )
        )  %>% 
        
        # change the manufacturing name
        mutate(industry = case_when(
          industry == "Gross domestic product" ~ "National GDP",    
          industry == "Government" ~ "Government",
          industry == "Agriculture, forestry, fishing, and hunting" ~ "Agriculture",
          industry == "Finance, insurance, real estate, rental, and leasing" ~ "Finance, Insurance, Real estate",
          industry == "Professional and business services" ~ "Professional & business services",
          industry == "Educational services, health care, and social assistance" ~ "Education & health care",
          industry == "Arts, entertainment, recreation, accommodation, and food services" ~ "Entertainment & Accommodation",
          industry == "Other services, except government" ~ "Other services",
          industry == "Government (Federal and State)" ~ "Government",
          TRUE ~ industry
            
        )) %>%
        
        # combining the quarterly dates with year and renaming year column
        mutate(year = year %>% str_c(... = quarterly_date, sep = "-")) %>%
        mutate(year = ymd(year)) %>%
        rename(gdp_date = year) %>%
        
        # removing the line and quarterly date
        select(-line,-quarterly_date) %>% 
        mutate(year = year(gdp_date)) %>% 
        relocate(quarter, .after = year) %>% 
        mutate(gdp = as.double(gdp))

    return(gdp_industry_tbl)
    
}

?str_match

# first wrangled data
gdp_industry_wrangled_tbl <- wrangle_industry_tbl(gdp_industry_all_tbl) 

gdp_industry_wrangled_tbl %>% 
    write_rds("00_data/gdp_industry_wrangled.rds")


# Checking for any duplicates in the industry column
gdp_industry_wrangled_tbl %>% group_by(industry) %>%
    filter(n() > 1) %>% summarize(n = n())


# getting yearly gdp - mean of the quarterly gdp
gdp_by_year_tbl <- gdp_industry_wrangled_tbl %>% 
    mutate(gdp_date = floor_date(gdp_date, unit = "year")) %>% 
    group_by(industry, gdp_date) %>% 
    summarise(total_gdp = round(mean(gdp), 0)) %>% 
    ungroup() 


# fs::dir_create(path = "02_visualization")

   
    
                                                      
                                                   
                                                 
 
   
