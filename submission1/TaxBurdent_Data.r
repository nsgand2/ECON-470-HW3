# Meta --------------------------------------------------------------------

## Title:         CDC Tax Burden on Tobacco
## Author:        Nikhita Gandhe
## Date Created:  3/17/2025
## Date Edited:   3/17/2025
## Description:   Clean and analyze CDC data 


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

cig.data <- read_csv("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW3/data/input/The_Tax_Burden_on_Tobacco__1970-2019_20250317.csv", col_names = TRUE)

cpi.data <- read_xlsx("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW3/data/input/SeriesReport-20250317212101_229e65.xlsx", skip = 11)

# Clean tobacco data --------------------------------------------------------------
cig.data <- cig.data %>%
  mutate(measure = case_when(
    SubMeasureDesc == "Average Cost per pack" ~ "cost_per_pack",
    SubMeasureDesc == "Cigarette Consumption (Pack Sales Per Capita)" ~ "sales_per_capita",
    SubMeasureDesc == "Federal and State tax as a Percentage of Retail Price" ~ "tax_percent",
    SubMeasureDesc == "Federal and State Tax per pack" ~ "tax_dollar",
    SubMeasureDesc == "Gross Cigarette Tax Revenue" ~ "tax_revenue",
    SubMeasureDesc == "State Tax per pack" ~ "tax_state"
  )) %>%
  select(state_abb = LocationAbbr, 
         state = LocationDesc, 
         Year, 
         value=Data_Value, 
         measure)


final.data <- pivot_wider(cig.data, 
                         id_cols = c("state","Year"),
                         names_from = "measure",
                         values_from = "value") %>%
  arrange(state, Year)

# Clean CPI data --------------------------------------------------------------
cpi.data <- pivot_longer(cpi.data, 
                         cols=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         names_to="month",
                         values_to="index")
cpi.data <- cpi.data %>%
  group_by(Year) %>%
  summarize(index=mean(index, na.rm=TRUE))



# Form final dataset ------------------------------------------------------
# adjust to 2012 dollars

final.data <- final.data %>%
  left_join(cpi.data, by="Year") %>%
  mutate(price_cpi=cost_per_pack*(230/index))

write_tsv(final.data,"data/output/TaxBurden_Data.txt",append=FALSE,col_names=TRUE)
write_rds(final.data,"data/output/TaxBurden_Data.rds")
