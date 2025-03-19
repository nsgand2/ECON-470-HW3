# Load Required Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest)

# Load the cleaned dataset
tax_burden_final <- readRDS("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW3/data/output/TaxBurden_Data.rds")

## 1. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.

# Identify tax changes per state-year
tax_burden_final_changes <- tax_burden_final %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = ifelse(Year == 1970, FALSE, 
                              ifelse(is.na(lag(tax_state)) | tax_state != lag(tax_state), TRUE, FALSE))) %>%
  ungroup()

# Calculate the proportion of states with tax changes each year
proportion_changes <- tax_burden_final_changes %>%
  group_by(Year) %>%
  summarize(proportion_changed = mean(tax_change, na.rm = TRUE)) %>%
  filter(Year >= 1970 & Year <= 1985)

# Plot the bar graph
ggplot(proportion_changes, aes(x = Year, y = proportion_changed)) +
  geom_bar(stat = "identity", fill = "#D8BFD8") +
  labs(
    title = "Proportion of States with Cigarette Tax Changes (1970–1985)",
    x = "Year",
    y = "Proportion of States with Tax Change"
  ) +
  theme_minimal()

print("Proportion of States with Cigarette Tax Changes (1970–1985)")
ggsave("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW3/plots/Proportion_of_States_with_Cigarette_Tax_Changes.png")

## 2. Present a line graph showing the average tax and price of a pack of cigarettes in 2012 dollars from 1970 to 2018.
# Aggregate the data by Year
aggregated_data <- tax_burden_final %>%
  group_by(Year) %>%
  summarise(
    avg_tax_dollar = mean(tax_dollar, na.rm = TRUE),
    avg_cost_per_pack = mean(cost_per_pack, na.rm = TRUE)
  )

# Create the plot
ggplot(aggregated_data, aes(x = Year)) + 
  geom_line(aes(y = avg_tax_dollar, color = "Tax"), size = 1) + 
  geom_line(aes(y = avg_cost_per_pack, color = "Cost per pack"), size = 1) + 
  labs(
    title = "Average Tax and Price of a Pack of Cigarettes (1970-2018)",
    x = "Year",
    y = "Value (2012 Dollars)"
  ) + 
  scale_color_manual(values = c("Tax" = "#D8BFD8", "Cost per pack" = "lightblue")) + 
  theme_minimal()
print("Average Tax and Price of a Pack of Cigarettes (1970-2018)")
ggsave("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW3/plots/Average_Tax_and_Price_of_a_Pack_of_Cigarettes.png")

## 3. Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

price_change <- tax_burden_final %>%
  group_by(state) %>%
  filter(Year == 1970 | Year == 2018) %>%
  summarise(price_change = cost_per_pack[Year == 2018] - cost_per_pack[Year == 1970])

top_states <- price_change %>%
  arrange(desc(price_change)) %>%
  head(5)

print(paste("States with the highest increases in cigarette prices:", paste(top_states$state, collapse = ", ")))


top_states_final <- tax_burden_final %>%
  filter(state %in% top_states$state)


ggplot(top_states_final, aes(x = Year, y = sales_per_capita, color = state)) +
  geom_line(size = 1) +
  labs(
    title = "Average Number of Packs Sold per Capita for the Top 5 States with the Highest Price Increase (1970-2018)",
    x = "Year",
    y = "Packs Sold per Capita",
    color = "State"
  ) +
  theme_minimal()
  print("Average Number of Packs Sold per Capita for the Top 5 States with the Highest Price Increase (1970-2018)")
ggsave("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW3/plots/Average_Number_of_Packs_Sold_per_Capita_Top_5_States.png")

## 4. Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

low_states <- price_change %>%
  arrange(price_change) %>%
  head(5)

print(paste("States with the lowest increases in cigarette prices:", paste(low_states$state, collapse = ", ")))

low_states_final <- tax_burden_final %>%
  filter(state %in% low_states$state)


ggplot(low_states_final, aes(x = Year, y = sales_per_capita, color = state)) +
  geom_line(size = 1) +
  labs(
    title = "Average Number of Packs Sold per Capita for the Top 5 States with the Highest Price Increase (1970-2018)",
    x = "Year",
    y = "Packs Sold per Capita",
    color = "State"
  ) +
  theme_minimal()
  print("Average Number of Packs Sold per Capita for the Top 5 States with the Highest Price Increase (1970-2018)")
ggsave("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW3/plots/Average_Number_of_Packs_Sold_per_Capita_Low_5_States.png")

## 5. Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.

#Combine both datasets for plotting
high_low_combined <- bind_rows(
  top_states_final %>% mutate(group = "Top 5 States (Highest Price Increase)"),
  low_states_final %>% mutate(group = "Top 5 States (Lowest Price Increase)")
)

# Step 5: Plot the trends for sales_per_capita in both groups using ggplot
ggplot(high_low_combined, aes(x = Year, y = sales_per_capita, color = state)) +
  geom_line(size = 1) +
  facet_wrap(~group, scales = "free_y") +  # Separate the plots by group
  labs(
    title = "Trends in Cigarette Sales per Capita: Highest vs. Lowest Price Increases (1970-2018)",
    x = "Year",
    y = "Cigarettes Sold per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

## The number of cigarettes sold has significantly declined in both the states with the highest and lowest price increases, reflecting broader cultural shifts away from smoking. Missouri and North Dakota contribute to a slight rise in the average cost per pack among the lower-priced states. Currently, both groups see approximately 50 packs sold per capita. However, the lower-priced states exhibit greater variability over time, with North Carolina reaching around 250 packs sold per capita in the mid-1970s.

print("Trends in Cigarette Sales per Capita: Highest vs. Lowest Price Increases (1970-2018)")
ggsave("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW3/plots/Trends_in_Cigarette_Sales_per_Capita_Highest_vs_Lowest_Price_Increases.png")

## 6.Focusing only on the time period from 1970 to 1990, regress log sales on log prices to estimate the price elasticity of demand over that period. Interpret your results.

# Filter data for the years 1970 to 1990 (if not already done)
tax_burden_1970_1990 <- tax_burden_final %>% filter(Year >= 1970 & Year <= 1990)

# Create log-transformed variables for sales and price
cig.data_1970_1990 <- tax_burden_1970_1990 %>% mutate(ln_sales=log(sales_per_capita),
                                ln_price_cpi=log(price_cpi),
                                ln_price=log(cost_per_pack),
                                tax_cpi=tax_state*(230/index),
                                total_tax_cpi=tax_dollar*(230/index),
                                ln_total_tax=log(total_tax_cpi),                             
                                ln_state_tax=log(tax_cpi))
# Run the regression
ols <- lm(ln_sales ~ ln_price, data=cig.data_1970_1990)


# Display the summary of the regression model
summary(ols)

# Interpretation of the results
# The coefficient for ln_price is -0.1715, indicating that a 1% increase in the price of cigarettes is associated with a 0.17% decrease in sales per capita, holding other factors constant. The relationship is statistically significant (p < 0.001), suggesting that cigarette demand is price elastic, though not highly so. The R-squared value of 0.1258 implies that price explains about 12.6% of the variation in cigarette sales, meaning other factors also play a significant role in determining demand.

## 7. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. Interpret your results and compare your estimates to those without an instrument. Are they different? If so, why?

# Run the instrumental variable regression
ivs <- feols(ln_sales ~ 1 | ln_price ~ ln_total_tax, data = cig.data_1970_1990)

# Display the summary of the IV regression results
summary(ivs)

# Interpretation of the results
# The instrumental variables (IV) regression estimates the effect of cigarette prices on sales, using total cigarette tax (ln_total_tax) as an instrument for price (ln_price). The coefficient for fit_ln_price is 0.5024, suggesting that a 1% increase in price is associated with a 0.50% increase in cigarette sales. This counterintuitive result likely indicates endogeneity issues—for example, taxes might be correlated with unobserved factors that also affect cigarette consumption.

# The Wu-Hausman test (p < 0.001) confirms that ordinary least squares (OLS) estimates are biased, meaning price is likely endogenous and IV is necessary. However, the negative adjusted R² (-1.81869) suggests that the model does not explain the variation in cigarette sales well. The weak explanatory power could indicate issues with the instrument's validity or other omitted variables affecting demand.


## 8. Show the first stage and reduced-form results from the instrument.

#First Stage Regression (Instrumental Variables)
# In the first stage, we regress ln_price (log of price) on ln_total_tax (log of total tax) to obtain predicted values of price.
step1 <- lm(ln_price ~ ln_total_tax, data = cig.data_1970_1990)

# Display the summary of the first stage regression
cat("First Stage: Regression of ln_price on ln_total_tax\n")
summary(step1)

# Predict the fitted values (pricehat) based on the first stage regression
pricehat <- predict(step1)

# Display the predicted values for ln_price (pricehat)
cat("\nPredicted values of ln_price (pricehat) from the first stage regression:\n")
print(head(pricehat))  # Display the first few predicted values

# Reduced-form (Second Stage) Regression
# In the second stage, we regress ln_sales (log of sales) on the predicted values of ln_price (pricehat).
step2 <- lm(ln_sales ~ pricehat, data = cig.data_1970_1990)

# Display the summary of the second stage regression
cat("\nSecond Stage: Regression of ln_sales on predicted ln_price (pricehat)\n")
summary(step2)

# using an instrument
step1 <- lm(ln_price ~ ln_total_tax, data=(
    cig.data_1970_1990 %>% filter(Year >= 1970 & Year <= 1990)))
pricehat <- predict(step1)
summary(step1)

step2 <- lm(ln_sales ~ pricehat, data=(
    df %>% filter(Year >= 1970 & Year <= 1990)))
summary(step2)

## Question 9

step3 <- lm(ln_price ~ ln_total_tax, data=(
    cig.data_1970_1990 %>% filter(Year >= 1991 & Year <= 2015)))
pricehat <- predict(step3)
summary(step3)

step4 <- lm(ln_sales ~ pricehat, data=(
    cig.data_1970_1990 %>% filter(Year >= 1991 & Year <= 2015)))
summary(step4)
