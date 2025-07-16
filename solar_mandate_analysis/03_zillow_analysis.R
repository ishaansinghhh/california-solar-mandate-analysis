# Zillow Home Price Analysis
# Solar Mandate Impact on Housing Costs

library(tidyverse)
library(fixest)
library(lubridate)

# ===== STEP 1: DOWNLOAD AND CLEAN ZILLOW DATA =====

# Zillow Home Value Index URL (county-level, all homes)
zillow_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

# Download the data
download.file(zillow_url, "data/raw/zillow_county_data.csv")

# Your target counties (same as permits analysis)
target_counties <- c(
  # California
  "Los Angeles County", "San Diego County", "Orange County", 
  "Alameda County", "Sacramento County",
  # Texas  
  "Harris County", "Dallas County", "Travis County", 
  "Bexar County", "Tarrant County"
)

# Process Zillow data using county names instead of FIPS
zillow_clean <- zillow_raw %>%
  # Filter to target counties by name
  filter(region_name %in% target_counties) %>%
  # Convert from wide to long format
  pivot_longer(
    cols = starts_with("x"),
    names_to = "date_str", 
    values_to = "home_price"
  ) %>%
  # Clean date column
  mutate(
    date_str = str_remove(date_str, "^x"),
    date = ymd(date_str),
    # Create FIPS code from state and municipal codes
    fips = paste0(state_code_fips, municipal_code_fips),
    # Create state identifier
    state = ifelse(state == "CA", "California", "Texas")
  ) %>%
  # Filter to analysis period
  filter(date >= ymd("2018-01-01"), date <= ymd("2022-12-31")) %>%
  # Remove missing values
  filter(!is.na(home_price)) %>%
  # Select final columns
  select(fips, region_name, state, date, home_price) %>%
  # Add year and month
  mutate(
    year = year(date),
    month = month(date),
    county_name = region_name
  )

# ===== STEP 3: CREATE DiD VARIABLES =====

zillow_analysis <- zillow_clean %>%
  mutate(
    # Treatment variables (same as permits analysis)
    treated = ifelse(state == "California", 1, 0),
    post = ifelse(date >= ymd("2020-01-01"), 1, 0),
    treated_post = treated * post,
    
    # Log home price (main outcome variable)
    log_home_price = log(home_price),
    
    # Identifiers for fixed effects
    county_id = paste(state, county_name, sep = "_"),
    time_id = paste(year, sprintf("%02d", month), sep = "-")
  ) %>%
  # Ensure complete data
  filter(!is.na(log_home_price))

# ===== STEP 4: RUN DiD REGRESSION  =====

# Run DiD regression
did_prices <- feols(log_home_price ~ treated_post | county_id + time_id,
                    data = zillow_analysis,
                    cluster = ~county_id)

etable(did_prices)

# ===== STEP 5: ROBUSTNESS CHECK  =====

# Add state-specific time trends
zillow_analysis <- zillow_analysis %>%
  mutate(time_trend = as.numeric(date - min(date)) / 365.25)

did_prices_trend <- feols(log_home_price ~ treated_post + treated:time_trend | county_id + time_id,
                          data = zillow_analysis,
                          cluster = ~county_id)

trend_coef <- coef(did_prices_trend)["treated_post"]
trend_se <- se(did_prices_trend)["treated_post"]
trend_percent <- (exp(trend_coef) - 1) * 100

cat("With state-specific trends:\n")
cat("Coefficient:", sprintf("%.4f", trend_coef), "\n")
cat("% Effect:   ", sprintf("%.1f%%", trend_percent), "\n")

# ===== STEP 6: VISUALIZATION =====
  
# Time series plot
price_trends <- zillow_analysis %>%
  group_by(date, state) %>%
  summarise(avg_price = mean(home_price, na.rm = TRUE), .groups = "drop")

price_plot <- price_trends %>%
  ggplot(aes(x = date, y = avg_price/1000, color = state)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = ymd("2020-01-01"), linetype = "dashed", color = "red") +
  labs(
    title = "Home Prices: California vs Texas",
    subtitle = "Solar mandate effect on housing costs",
    x = "Date",
    y = "Average Home Price ($1000s)",
    color = "State"
  ) +
  scale_color_manual(values = c("California" = "blue", "Texas" = "orange")) +
  theme_minimal()

print(price_plot)

# ===== STEP 7: COMPARISON WITH PERMITS RESULTS =====

# Calculate price_percent from your regression results
price_coef <- coef(did_prices)["treated_post"]
price_pval <- pvalue(did_prices)["treated_post"]
price_percent <- (exp(price_coef) - 1) * 100

# Update these with your actual permits results
permits_effect <- -19.9  # Your permits percentage effect
permits_significant <- FALSE  # Your permits significance


# Create comparison
comparison_data <- data.frame(
  outcome = c("Building Permits", "Home Prices"),
  effect = c(permits_effect, price_percent),
  significant = c(permits_significant, price_pval < 0.05)
)

comparison_plot <- comparison_data %>%
  ggplot(aes(x = outcome, y = effect, fill = significant)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Solar Mandate Effects: Permits vs Prices",
    x = "Outcome",
    y = "Percentage Effect (%)",
    fill = "Significant"
  ) +
  scale_fill_manual(values = c("lightgray", "steelblue")) +
  theme_minimal()

print(comparison_plot)


# ===== SAVE ZILLOW ANALYSIS RESULTS =====

# Extract home price results
price_results <- data.frame(
  outcome = "Home Prices",
  coefficient = coef(did_prices)["treated_post"],
  std_error = se(did_prices)["treated_post"],
  p_value = pvalue(did_prices)["treated_post"],
  percent_effect = (exp(coef(did_prices)["treated_post"]) - 1) * 100,
  significant = pvalue(did_prices)["treated_post"] < 0.05
)

# Save home price results
write_csv(price_results, "data/processed/home_price_did_results.csv")

# Save robustness check for prices
price_robustness <- data.frame(
  specification = c("Main Model", "With State Trends"),
  coefficient = c(
    coef(did_prices)["treated_post"],
    trend_coef
  ),
  std_error = c(
    se(did_prices)["treated_post"],
    trend_se
  ),
  percent_effect = c(
    (exp(coef(did_prices)["treated_post"]) - 1) * 100,
    trend_percent
  )
)

write_csv(price_robustness, "data/processed/price_robustness_results.csv")

# ===== SAVE COMBINED PERMITS + PRICES RESULTS =====

# Combine permits and price results
combined_results <- data.frame(
  outcome = c("Building Permits", "Home Prices"),
  coefficient = c(-0.2225, coef(did_prices)["treated_post"]),  # Update with your actual permits coefficient
  percent_effect = c(permits_effect, price_percent),
  p_value = c(0.16, price_pval),  # Update with your actual permits p-value
  significant = c(permits_significant, price_pval < 0.05)
)

write_csv(combined_results, "data/processed/combined_permits_prices_results.csv")

# ===== SAVE PROFESSIONAL TABLES =====

# HTML table for home prices
modelsummary(did_prices,
             output = "output/home_price_results_table.html",
             title = "Effects of California Solar Mandate on Home Prices")

# CSV table for home prices
modelsummary(did_prices,
             output = "output/home_price_results_table.csv")

# Combined table (you'll need to load permits results)
# modelsummary(list("Building Permits" = did_total, "Home Prices" = did_prices),
#              output = "output/combined_results_table.html",
#              title = "California Solar Mandate: Effects on Housing Market")

# ===== SAVE PLOTS =====

ggsave("output/home_price_trends.png", price_plot, width = 10, height = 6)
ggsave("output/permits_vs_prices_comparison.png", comparison_plot, width = 8, height = 6)

# ===== SAVE ANALYSIS DATASETS =====

write_csv(zillow_analysis, "data/processed/home_price_analysis_data.csv")
write_csv(comparison_data, "data/processed/permits_vs_prices_comparison.csv")


dir.create("figures", showWarnings = FALSE)

# Copy all your plots to figures folder
file.copy("output/permits_vs_prices_comparison.png", "figures/")
file.copy("output/parallel_trends_check.png", "figures/")
file.copy("output/timeseries_did_plot.png", "figures/")
file.copy("output/home_price_trends.png", "figures/")
file.copy("output/classic_did_plot.png", "figures/")
file.copy("output/counterfactual_plot.png", "figures/")

cat("✅ All figures copied to figures/ folder\n")
