## ----setup, include = FALSE------------------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)


## --------------------------------------------------------------------------------------

# loading necessary packages

library(knitr)
library(here)
library(gridExtra)
library(readr)
library(tidyr)
library(stargazer)
library(dplyr)
library(plm)
library(lubridate)
library(foreign)
library(ggplot2)
library(texreg)
library(ggpubr)
library(lmtest)
library(sandwich)
library(broom)
library(estimatr)
library(car)
library(psych)
library(fixest)
library(lmtest)
library(gt)
options(warn = -1) # To supress warnings

# Import the dataset
full_data <- read.dta(here("data", "servicesdataset 2.dta"))

# Filter the data for the TSP sector
tsp_data <- full_data[full_data$sector == "TSP", ]

# Display the first few rows of the filtered data
head(tsp_data)


## --------------------------------------------------------------------------------------

# Select the variables of interest and exclude rows where trade equals zero
tps_select_sum <- tsp_data %>%
  select(trade, dist, gdp_imp, gdp_exp, contig, comlang_off, comcol, colony)

tps_select_NZ <- tps_select_sum %>%
  filter_at(vars("trade", "dist", "gdp_imp", "gdp_exp"),
            all_vars(. != 0))

# Generate the summary statistics using stargazer
stargazer(tps_select_NZ, type = "text",
          title = "Descriptive Statistics of Selected Variables",
          digits = 2,
          summary.stat = c("n", "mean", "sd", "min", "max"),
          header = FALSE)



## --------------------------------------------------------------------------------------

# Standardize GDP values by dividing by 1,000,000 and adjust variable names in the select function
tps_adjusted <- tps_select_NZ %>%
  mutate(
    gdp_imp_M = gdp_imp / 1000000,  # Standardizing GDP of importer
    gdp_exp_M = gdp_exp / 1000000   # Standardizing GDP of exporter
  ) %>%
  select(trade, dist, gdp_imp_M, gdp_exp_M, contig, comlang_off, comcol, colony)

# Generate the summary statistics using stargazer
stargazer(tps_adjusted, type = "text",
          title = "Descriptive Statistics of Selected Variables",
          digits = 2,
          summary.stat = c("n", "mean", "sd", "min", "max"), 
          header = FALSE) 



## --------------------------------------------------------------------------------------

# Filter data for the year 2005, non-zero trade, and no NA in GDP columns
tsp_2005 <- tsp_data %>%
  filter(year == 2005, trade > 0) %>%
  filter(!is.na(gdp_imp), !is.na(gdp_exp))

# Standardize the order of country pairs and remove duplicates
unique_country_pairs <- tsp_2005 %>%
  mutate(
    sorted_imp = pmin(imp, exp),
    sorted_exp = pmax(imp, exp)
  ) %>%
  distinct(sorted_imp, sorted_exp)

# Count unique country pairs
num_country_pairs <- nrow(unique_country_pairs)

# Count unique countries involved in trade
countries_trading <- unique(c(unique_country_pairs$sorted_imp, unique_country_pairs$sorted_exp))
num_countries_trading <- length(countries_trading)

# Calculate the share of countries trading Transport Services
total_countries <- length(unique(c(tsp_data$imp, tsp_data$exp)))  
share_trading_transport <- num_countries_trading / total_countries * 100

# Display the results
cat("Number of countries trading in 2005:", num_countries_trading, "\n")
cat("Number of country pairs trading in 2005:", num_country_pairs, "\n")
cat("Share of countries trading Transport Services in 2005:", share_trading_transport, "%\n")



## --------------------------------------------------------------------------------------

# Filter data for the year 2005 and non-zero trade
tsp_2005 <- tsp_data %>%
  filter(year == 2005, trade > 0)

# Standardize the order of country pairs and remove duplicates
unique_country_pairs <- tsp_2005 %>%
  mutate(
    sorted_imp = pmin(imp, exp),
    sorted_exp = pmax(imp, exp)
  ) %>%
  distinct(sorted_imp, sorted_exp)

# Count unique country pairs
num_country_pairs <- nrow(unique_country_pairs)

# Count unique countries involved in trade
countries_trading <- unique(c(unique_country_pairs$sorted_imp, unique_country_pairs$sorted_exp))
num_countries_trading <- length(countries_trading)

# Calculate the share of countries trading Transport Services
total_countries <- length(unique(c(tsp_data$imp, tsp_data$exp)))  
share_trading_transport <- num_countries_trading / total_countries * 100

# Display the results
cat("Number of countries trading in 2005:", num_countries_trading, "\n")
cat("Number of country pairs trading in 2005:", num_country_pairs, "\n")
cat("Share of countries trading Transport Services in 2005:", share_trading_transport, "%\n")



## --------------------------------------------------------------------------------------

# Create a new variables
tps_select_NZ <- tps_select_NZ %>%
  mutate(log_gdp_combined = log(gdp_exp * gdp_imp),
         log_trade = log(trade),
         log_distance = log(dist),
         log_gdp_imp = log(gdp_imp),
         log_gdp_exp = log(gdp_exp))



## --------------------------------------------------------------------------------------

# Adjusting plot themes to change axis titles and legend text size
custom_theme <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10)
    )
}

# Define colors for contiguity and official language
contiguity_colors <- c("red", "blue")  
language_colors <- c("red", "blue")    

###########################################################################################################################################################################################

# Plot 1: Log of Trade vs Log of Distance 
p1 <- ggplot(tps_select_NZ, aes(x = log_distance, y = log_trade)) +
  geom_point(alpha = 0.3) +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatter Plot of Trade on Distance", x = "Log (Distance)", y = "Log (Trade)") +
  custom_theme()  

# Display the plot
print(p1)

# Save Plot 1
ggsave(filename = here("data", "viz", "plot1_logtrade_vs_logdistance.jpg"), plot = p1, width = 8, height = 6)

###########################################################################################################################################################################################

# Plot 2: Log of Trade vs Log of Combined GDP 

p2 <- ggplot(tps_select_NZ, aes(x = log_gdp_combined, y = log_trade)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Scatter Plot of Trade on Combined GDP", x = "Log(Combined GDP)", y = "Log(Trade)") +
  custom_theme()  

# Display the plot
print(p2)

# Save Plot 2
ggsave(filename = here("data", "viz", "plot2_logtrade_vs_loggdpcomb.jpg"), plot = p2, width = 8, height = 6)

###########################################################################################################################################################################################

# Plot 3: Trade vs Distance by contiguity

p3 <- ggplot(tps_select_NZ, aes(x = log_distance, y = log_trade, color = factor(contig))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = contiguity_colors, labels = c("Not Contiguous", "Contiguous")) +
  labs(
    title = "Log(Trade) vs Log(Distance) by Contiguity",
    x = "Log(Distance)",
    y = "Log(Trade)",
    color = "Contiguity"  # Rename the color legend to "Contiguity"
  ) +
  custom_theme()

# Print the plot
print(p3)

# Save Plot 3
ggsave(filename = here("data", "viz", "plot3_contiguity_log_trade_distance.jpg"), plot = p3, width = 8, height = 6)


###########################################################################################################################################################################################

# Plot 4: Trade vs Log(Combined GDP) by Contiguity
p4 <- ggplot(tps_select_NZ, aes(x = log_gdp_combined, y = log_trade, color = factor(contig))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = contiguity_colors, 
                     labels = c("Not Contiguous", "Contiguous")) +  
  labs(title = "log(Trade) vs Log(Combined GDP) by Contiguity", 
       x = "Log(Combined GDP)", 
       y = "Log(Trade)",
       color = "Contiguity") +  # Rename the color legend to "Contiguity"
  custom_theme()

# Print the plot
print(p4)

# Save Plot 4
ggsave(filename = here("data", "viz", "plot4_contiguity_log_trade_gdp.jpg"), plot = p4, width = 8, height = 6)

###########################################################################################################################################################################################

# Plot 5: Trade vs Log(Distance) by common official language

p5 <- ggplot(tps_select_NZ, aes(x = log_distance, y = log_trade, color = factor(comlang_off))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +  # Add best fit line
  scale_color_manual(values = language_colors, 
                     labels = c("No Common Language", "Common Language")) +  
  labs(title = "Log(Trade) vs Log(Distance) by Official Language", 
       x = "Log(Distance)", 
       y = "Log(Trade)",
       color = "Common Official Language") +  # Rename the color legend to "Official Language"
  custom_theme()

# Print the plot
print(p5)

# Save Plot 5
ggsave(filename = here("data", "viz", "plot5_log_trade_distance_language.jpg"), plot = p5, width = 8, height = 6)

###########################################################################################################################################################################################


# Plot 6: Log(Trade) vs Log(Combined GDP) by Official Language

p6 <- ggplot(tps_select_NZ, aes(x = log_gdp_combined, y = log_trade, color = factor(comlang_off))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +  
  scale_color_manual(values = language_colors, labels = c("No Common Language", "Common Language")) +
  labs(
    title = "Log(Trade) vs Log(Combined GDP) by Official Language",
    x = "Log(Combined GDP)",
    y = "Log(Trade)",
    color = "Common Official Language"  # Rename the color legend to "Official Language"
  ) +
  custom_theme() 

# Print the plot
print(p6)

# Save Plot 6
ggsave(filename = here("data", "viz", "plot6_trade_gdp_language.jpg"), plot = p6, width = 8, height = 6)

###########################################################################################################################################################################################


# Arranging plots using gridExtra
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)



## --------------------------------------------------------------------------------------

# Filter the data to include only rows where trade is not equal zero
tsp_corr <- tps_select_NZ %>%
  select(trade, dist, gdp_exp, gdp_imp)  # Ensure only the necessary columns are included

# Calculate the correlation matrix for the selected variables
tsp_corr <- cor(tsp_corr, use = "complete.obs")

# Use stargazer to format and output the correlation matrix
stargazer(tsp_corr, type = "text", title = "Correlation Matrix of Trade Variables")

###########################################################################################################################################################################################

# Filter the data to include only rows where trade is not equal zero
tsp_corr_log <- tps_select_NZ %>%
  select(log_trade, log_distance, log_gdp_exp, log_gdp_imp, log_gdp_combined)  # Ensure only the necessary columns are included

# Calculate the correlation matrix for the selected variables
tsp_corr_log <- cor(tsp_corr_log, use = "complete.obs")

# Use stargazer to format and output the correlation matrix
stargazer(tsp_corr_log, type = "text", title = "Correlation Matrix of Trade Variables")



## --------------------------------------------------------------------------------------

# Data preparation and transformation
tps_select_NZ <- tps_select_NZ %>%
  filter(!is.na(log_trade) & !is.na(log_gdp_imp) & !is.na(log_gdp_exp) & !is.na(log_distance))

# Fit the linear model
mod1 <- lm(log_trade ~ log_gdp_imp + log_gdp_exp + log_distance + contig + comlang_off + comcol + colony, data = tps_select_NZ)

summary(mod1)

# Calculate robust standard errors, clustering by 'distance'
tps_select_NZ$log_distance <- as.factor(tps_select_NZ$log_distance)
robust_se <- vcovHC(mod1, type = "HC1", cluster = ~log_distance)

# Display the model coefficients with robust standard errors
mod2 <- coeftest(mod1, vcov. = robust_se)

# Print the robust results
print(mod2)

# stargazer to format result
stargazer(mod1, mod2, type = "text",
          title = "Regression Results (Determinants of Trade)",
          column.labels = c("Model 1 (SE)", "Model 2 (RSE)"),
          dep.var.labels = "Log(Trade)",
          covariate.labels = c("Log(GDP Importer)", "Log(GDP Exporter)", "Log(Distance)", 
                               "Contiguity", "CommLang(Official)", "Colony", "Comcol"),
          digits = 4)



## --------------------------------------------------------------------------------------

# Test the joint hypothesis for Dichotomous variable
ftest1 <- linearHypothesis(mod1, 
                               c("contig = 0", "comlang_off = 0", "comcol = 0"),
                               test = "F")

print(ftest1)

###########################################################################################################################################################################################

# Test for GDP close to unity
ftest2 <- linearHypothesis(mod1, 
                               c("log_gdp_exp = 1", "log_gdp_imp = 1"),
                               test = "F")

print(ftest2)


## --------------------------------------------------------------------------------------
# Select the variables of interest and exclude rows where trade equals zero
tpsDF <- tsp_data %>%
  filter_at(vars("trade", "dist", "gdp_imp", "gdp_exp"),
            all_vars(. != 0))

# Merging etcr_exp and etcr_imp from tps_data to tps_select_NZ
tps_DFS <- tpsDF %>%
  left_join(tps_select_NZ, by = c("trade", "contig", "dist", "gdp_imp", "gdp_exp", "comlang_off", "comcol", "colony")) %>%
  mutate(OECD_dummy = ifelse(!is.na(etcr_exp) & !is.na(etcr_imp), 1, 0))



## --------------------------------------------------------------------------------------

p7 <- ggplot(tps_DFS, aes(x = log(dist), y = log(trade), color = factor(OECD_dummy))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("red", "blue"), labels = c("Non-OECD", "OECD")) +
  labs(title = "Log(Trade) vs Log(Distance) by OECD Membership",
       x = "Log(Distance)", y = "Log(Trade)",
       color = "OECD Membership") +
  theme_minimal()

# Show plot
print(p7)

# Save Plot 
ggsave(filename = here("data", "viz", "plot7_trade_distance_oecd.jpg"), plot = p7, width = 8, height = 6)



## --------------------------------------------------------------------------------------
  
# Ensure tps_DFS contains all necessary variables and then filter out rows with NAs in key variables
tps_DFS <- tps_DFS %>%
  filter(!is.na(log(trade)) & !is.na(log(gdp_imp)) & !is.na(log(gdp_exp)) & !is.na(log(dist)))

# Fit the linear model with additional variable 'OECD_dummy'
model3 <- lm(log(trade) ~ log(gdp_imp) + log(gdp_exp) + log(dist) + 
             contig + comlang_off + comcol + colony + OECD_dummy, data = tps_DFS)

# Print the robust results
summary(model3)


# Calculate robust standard errors, clustering by 'dist' transformed into a factor
tps_DFS$log_dist <- as.factor(tps_DFS$log_dist)
robust_se_model3 <- vcovHC(model3, type = "HC1", cluster = ~log_dist)

# Use coeftest to display the model coefficients with robust standard errors
model4 <- coeftest(model3, vcov. = robust_se_model3)

# Print the robust results
print(model4)

# Format and display the model results using stargazer
stargazer(model3, model4, type = "text",
          title = "Determininants of Trade (Augmented Model)",
          column.labels = c("Model 3 (SE)", "Model 4 (RSE)"),
          dep.var.labels = "Log(Trade)",
          covariate.labels = c("Log(GDP Importer)", "Log(GDP Exporter)", "Log(Distance)", 
                               "Contiguity", "CommLang(Official)", "Commcol","Colony", "OECD Membership"),
          digits = 4)



## --------------------------------------------------------------------------------------
# Make sure tps_DFS is cleaned and contains no NAs in the columns to be used
tps_DFSX <- tps_DFS %>%
  filter(trade != 0, dist != 0) %>%
  filter(!is.na(trade), !is.na(dist), !is.na(contig), !is.na(comlang_off), !is.na(colony), !is.na(comcol), !is.na(OECD_dummy), !is.na(exp), !is.na(imp))

# Fit the robust linear model
mod5 <- lm_robust(
  log(trade) ~ log(dist) + contig + comlang_off + colony + comcol + OECD_dummy + factor(exp) + factor(imp),
  data = tps_DFSX,
  clusters = tps_DFSX$dist,  # Use the dist column directly from tps_DFSX for clustering
  se_type = "stata"
)

# Print the summary of the model
summary(mod5)



## --------------------------------------------------------------------------------------

mod6 <- feols(
          log(trade) ~ 1 + log(dist) + contig + comlang_off + colony + comcol + OECD_dummy | exp + imp, data = tps_DFSX, 
)
mod6 
summary(mod6, cluster = ~dist)


## --------------------------------------------------------------------------------------
tps_DFSX <- tps_DFS [tps_DFS$trade!=0 & tps_DFS$dist != 0,]

mod7 <- lm_robust(log(trade) ~ log(dist) + contig + comlang_off + colony + comcol + factor(exp) + factor(imp), cluster =dist, data = tps_DFSX, se_type = "stata")

summary(mod7)


## --------------------------------------------------------------------------------------

texreg::screenreg(
list(mod2, model4, mod5, mod7),
omit.coef = c('factor'),
include.ci = FALSE,
caption = '',
custom.note = "Note: robust standard errors",
custom.gof.rows = list("Country Imp/Exp FE" = c("NO", "NO", "YES", "YES"))
)

options(warn = -1)

