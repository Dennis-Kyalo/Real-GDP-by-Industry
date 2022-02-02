# Libraries

library(tidyverse)
library(plotly)
library(tidyquant)

# 1.1 Load the data ----

gdp_industry_wrangled_tbl <- read_rds(file = "00_data/gdp_industry_wrangled.rds")

# 1.2 Create a function that takes time as quarter and year ----

time_picker_gdp <- function(data, time_unit = "quarter") {
        
    if (time_unit %in% "quarter") {
        formatted_quarterly_tbl <- data %>%
            # date formatted quarterly

            mutate(format_date = floor_date(gdp_date, unit = time_unit)) %>%
            
            # we find the contribution of each category based on the total gdp
            group_by(format_date) %>%
            mutate(first_gdp = first(x = gdp)) %>%
            
            # to retain the industry column
            summarise(
                row                   = row_number(),
                industry_contribution = gdp / first_gdp,
                industry              = industry[row],
                gdp                   = gdp[row],
                quarter               = quarter[row]
                
            ) %>%
            ungroup() %>%
            
            select(-row) %>%
            select(industry, gdp, everything()) %>%
            mutate(
                industry_contribution = dplyr::case_when(
                    industry_contribution == 1 ~ NA_real_,
                    TRUE ~ as.numeric(industry_contribution)
                )
            ) %>%
            
            group_by(format_date) %>%
            mutate(rank = dense_rank(desc(industry_contribution))) %>%
            ungroup() %>%
            mutate(year     = year(format_date)) %>%
            mutate(gdp_text = scales::dollar(gdp)) %>%
            mutate(ind_cont_percent = industry_contribution %>% scales::percent(accuracy = 0.1)) %>%
            mutate(
                ind_cont_perc_text = str_glue(
                    "Industry: {industry}
                                         Rank: {rank}
                                         Industry GDP : {gdp_text} Million
                                         Contribution to National GDP : {ind_cont_percent}
                                         Duration: {year}-Q{quarter}"
                )
            )
        
        return(formatted_quarterly_tbl)
        
    } else if (time_unit %in% "year") {
        formatted_yearly_tbl <- data %>%
            # we find the contribution of each category based on the total gdp
            mutate(format_date = floor_date(gdp_date, unit = time_unit)) %>%
            
            
            group_by(industry, format_date) %>%
            summarise(yearly_gdp = mean(gdp)) %>%
            ungroup() %>%
            
            
            # make "Gross domestic product" first
            pivot_wider(names_from = industry, values_from = yearly_gdp) %>%
            select(format_date, `Gross domestic product`, everything()) %>%
            pivot_longer(
                cols      = -format_date,
                names_to  = "industry",
                values_to = "yearly_gdp"
            ) %>%
            select(industry, everything()) %>%
            
            # group and find the percent contribution yearly
            group_by(format_date) %>%
            mutate(first_gdp = first(x = yearly_gdp)) %>%
            
            # to retain the industry column
            summarise(
                row                   = row_number(),
                industry_contribution = yearly_gdp / first_gdp,
                industry              = industry[row],
                yearly_gdp            = yearly_gdp[row],
                quarter               = quarter[row]
                
            ) %>%
            ungroup() %>%
            
            select(-row) %>%
            rename(gdp = yearly_gdp) %>%
            select(industry, gdp, everything()) %>%
            mutate(
                industry_contribution     = dplyr::case_when(
                    industry_contribution == 1 ~ NA_real_,
                    TRUE ~ as.numeric(industry_contribution)
                )
            ) %>%
            
            group_by(format_date) %>%
            mutate(rank = dense_rank(desc(industry_contribution))) %>%
            ungroup() %>%
            mutate(year = year(format_date)) %>%
            mutate(gdp_text         = scales::dollar(gdp)) %>%
            mutate(ind_cont_percent = industry_contribution %>% scales::percent(accuracy = 0.1)) %>%
            mutate(
                ind_cont_perc_text  = str_glue(
                    "Industry: {industry}
                                         Rank : {rank}
                                         Industry GDP : {scales::dollar(gdp)} Million
                                         Contribution to National GDP : {ind_cont_percent}
                                         Duration: {year}"
                )
            )
        
        return(formatted_yearly_tbl)
    }
    
}  

# 1.3 Create a directory to dump the function ----
fs::dir_create("00_functions")
dump(list = c("wrangle_industry_tbl", "time_picker_gdp"), file = "00_functions/all_functions.R")

time_unit = "year"
time_picker_gdp(gdp_industry_wrangled_tbl, time_unit = "quarter") %>% head()
   

# 1.4 GGplot based on one Industry GDP (Time-series) ----
g1 <- time_picker_gdp(gdp_industry_wrangled_tbl, time_unit = "quarter") %>%
    filter(industry %in% "Mining") %>%
    ggplot(aes(x = format_date, y = gdp)) +
    geom_line(color = "#2c3e50") +
    geom_point(aes(text = ind_cont_perc_text),
               color = "#2c3e50",
               size = 0.01) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_log10(labels = scales::dollar_format())
    
ggplotly(g1, tooltip = "text")


# 1.5 GGplot based on one industry rate of change ----
g2 <- time_picker_gdp(gdp_industry_wrangled_tbl, time_unit = "quarter") %>%
    select(industry, gdp, format_date) %>%
    group_by(industry) %>%
    mutate(lag_1 = lag(gdp, n = 1)) %>%
    mutate(lag_1 = case_when(
        is.na(lag_1) ~ gdp,
        TRUE ~ lag_1
    )) %>% 
    summarise(
        rate = (gdp - lag_1) / lag_1,
        row                   = row_number(),
        format_date           = format_date[row],
        
    ) %>% 
    ungroup() %>%
    mutate(rate_text = scales::percent(rate, accuracy = 0.1)) %>% 
    mutate(ind_cont_perc_text = str_glue("Industry : {industry}
                                          Growth Rate : {rate_text}")) %>% 
    filter(industry %in% c("Mining")) %>% 
    mutate(pos = rate >= 0) %>% 
    ggplot(aes(x = format_date, y = rate, fill = pos)) +
    geom_col() +
    # geom_line(col = "grey") +
    geom_point(aes(text = ind_cont_perc_text),
               color = "#2c3e50",
               size = 0.01) +
    # geom_smooth(method = "loess", span = 0.1, col = "gray", size = 0.5) +
    
    theme_tq() +
    expand_limits(y = 0) +
    theme(legend.position = "none")


ggplotly(g2, tooltip = "text")


# 1.6 GGplot based on two Industry GDP (Time-series) ----
g3 <- time_picker_gdp(gdp_industry_wrangled_tbl, time_unit = "quarter") %>%
    filter(industry %in% c("Mining", "Information")) %>%
    ggplot(aes(x = format_date, y = gdp, col = industry)) +
    geom_line() +
    geom_point(aes(text = ind_cont_perc_text),
               color = "#2c3e50",
               size = 0.01) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_log10(labels = scales::dollar_format())

ggplotly(g1, tooltip = "text")


# 1.7 GGplot based on two industry rate of change ----
g4 <- time_picker_gdp(gdp_industry_wrangled_tbl, time_unit = "quarter") %>%
    select(industry, gdp, format_date) %>%
    group_by(industry) %>%
    mutate(lag_1 = lag(gdp, n = 1)) %>%
    mutate(lag_1 = case_when(
        is.na(lag_1) ~ gdp,
        TRUE ~ lag_1
    )) %>% 
    summarise(
        rate = (gdp - lag_1) / lag_1,
        row                   = row_number(),
        format_date           = format_date[row],
        
    ) %>% 
    ungroup() %>%
    mutate(rate_text = scales::percent(rate, accuracy = 0.1)) %>% 
    mutate(ind_cont_perc_text = str_glue("Industry   : {industry}
                                         Growth rate : {rate_text}")) %>% 
    filter(industry %in% c("Mining", "Information")) %>% 
    
    ggplot(aes(x = format_date, y = rate, col = industry)) +
    geom_col() +
    geom_line() +
    geom_point(aes(text = ind_cont_perc_text),
               color = "#2c3e50",
               size = 0.01) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    expand_limits(y = 0)


ggplotly(g4, tooltip = "text")


# 1.8 GGstatplot on correlation between industries ----
time_picker_gdp(gdp_industry_wrangled_tbl, time_unit = "quarter") %>%
    filter(industry %in% c("Gross domestic product", "Mining")) %>%
    select(industry, format_date, gdp) %>%
    pivot_wider(names_from = industry, values_from = gdp) %>%
    ggstatsplot::ggscatterstats(
        data = data1,
        y = `Gross domestic product`,
        x = `Mining`,
        ggtheme = tidyquant::theme_tq()
        
    )
    





























