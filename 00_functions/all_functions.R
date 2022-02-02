wrangle_industry_tbl <-
function(data) {
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
time_picker_gdp <-
function(data, time_unit = "quarter") {
        
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
