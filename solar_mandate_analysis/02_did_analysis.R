# ===== DIFFERENCES-IN-DIFFERENCES ANALYSIS =====

library(tidyverse)
library(fixest)
library(modelsummary)
library(lubridate)

# ===== STEP 1: LOAD AND PREPARE COMBINED DATA FOR ANALYSIS =====

combined_data <- read_csv("data/processed/ca_tx_combined.csv")

analysis_data <- combined_data %>% 
  mutate(
    log_total_permits = log(total_permits + 1),
    log_single_family = log(single_family + 1),
    log_multifamily = log(multifamily + 1),
    
    # Create county identifier for county fixed effects (permanent differences between counties)
    county_id = paste(state, county_name, sep="_"), 
    
    # Create time identifier for time fixed effects (national, time-varying trends affecting all counties)
    time_id = paste(year, sprintf("%02d", month), sep = "-"),
    
    # Treatment effect
    treated_post = treated * post
  ) %>%
  # Filter NA values
  filter(!is.na(total_permits), !is.na(date))

# ===== STEP 2: RUNNING DID REGRESSION =====

did_total <- feols(log_total_permits ~ treated_post | county_id + time_id, 
                   data = analysis_data, cluster = ~county_id)
did_single <- feols(log_single_family ~ treated_post | county_id + time_id,
                    data = analysis_data, cluster = ~county_id)
did_multi <- feols(log_multifamily ~ treated_post | county_id + time_id,
                   data = analysis_data, cluster = ~county_id)

etable(did_total, did_single, did_multi,
       title = "California Solar Mandate Effects",
       headers = c("Total Permits", "Single Family", "Multifamily"))

# Robustness check

# Add linear time trends by state
analysis_data <- analysis_data %>%
  mutate(time_trend = as.numeric(date - min(date)) / 365.25)

did_total_trend <- feols(log_total_permits ~ treated_post + treated:time_trend | county_id + time_id,
                         data = analysis_data,
                         cluster = ~county_id)

trend_coef <- coef(did_total_trend)["treated_post"]
trend_se <- se(did_total_trend)["treated_post"]

cat("Total Permits with State-Specific Trends:\n")
cat("Coefficient:", sprintf("%.4f", trend_coef), "\n")
cat("Std Error:  ", sprintf("%.4f", trend_se), "\n")

# Alternative specification: levels instead of logs
did_levels <- feols(total_permits ~ treated_post | county_id + time_id,
                    data = analysis_data, 
                    cluster = ~county_id)

levels_coef <- coef(did_levels)["treated_post"]
levels_se <- se(did_levels)["treated_post"]

cat("\nTotal Permits in Levels (not logs):\n")
cat("Coefficient:", sprintf("%.1f", levels_coef), "\n")
cat("Std Error:  ", sprintf("%.1f", levels_se), "\n")




# ===== 1. CLASSIC DiD PLOT (Pre/Post Means) =====

# Calculate pre/post averages by state
did_summary <- analysis_data %>%
  mutate(period = ifelse(post == 1, "Post-Mandate\n(2020-2022)", "Pre-Mandate\n(2018-2019)")) %>%
  group_by(state, period) %>%
  summarise(
    avg_permits = mean(total_permits, na.rm = TRUE),
    se_permits = sd(total_permits, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Classic DiD plot
classic_did <- did_summary %>%
  ggplot(aes(x = period, y = avg_permits, color = state, group = state)) +
  geom_point(size = 4) +
  geom_line(size = 1.5, alpha = 0.8) +
  geom_errorbar(aes(ymin = avg_permits - 1.96*se_permits, 
                    ymax = avg_permits + 1.96*se_permits), 
                width = 0.1, alpha = 0.7) +
  labs(
    title = "Difference-in-Differences: Building Permits",
    subtitle = "California Solar Mandate Effect vs Texas Control",
    x = "Time Period",
    y = "Average Monthly Permits per County",
    color = "State",
    caption = "Error bars show 95% confidence intervals"
  ) +
  scale_color_manual(values = c("California" = "blue", "Texas" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(classic_did)


# ===== 2. TIME SERIES WITH TREATMENT LINE =====

monthly_trends <- analysis_data %>%
  group_by(date, state) %>%
  summarise(avg_permits = mean(total_permits, na.rm = TRUE), .groups = "drop")

timeseries_did <- monthly_trends %>%
  ggplot(aes(x = date, y = avg_permits, color = state)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_vline(xintercept = ymd("2020-01-01"), 
             linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = ymd("2020-01-01"), y = max(monthly_trends$avg_permits) * 0.9,
           label = "Solar Mandate\nEffective", hjust = -0.1, color = "red", size = 3.5) +
  labs(
    title = "Building Permits Over Time: California vs Texas",
    subtitle = "Monthly averages across selected counties",
    x = "Date",
    y = "Average Monthly Permits per County",
    color = "State"
  ) +
  scale_color_manual(values = c("California" = "blue", "Texas" = "orange")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(timeseries_did)


# ===== 3. COUNTERFACTUAL PLOT =====

# Calculate what CA would have looked like without treatment
# Using the pre-period trend and TX's post-period change

pre_period <- analysis_data %>% 
  filter(year <= 2019) %>%
  group_by(state) %>%
  summarise(pre_avg = mean(total_permits, na.rm = TRUE), .groups = "drop")

post_period <- analysis_data %>%
  filter(year >= 2020) %>%
  group_by(state) %>%
  summarise(post_avg = mean(total_permits, na.rm = TRUE), .groups = "drop")

# Calculate TX change
tx_change <- post_period$post_avg[post_period$state == "Texas"] - 
  pre_period$pre_avg[pre_period$state == "Texas"]

# CA counterfactual = CA pre-period + TX change
ca_counterfactual <- pre_period$pre_avg[pre_period$state == "California"] + tx_change
ca_actual <- post_period$post_avg[post_period$state == "California"]

counterfactual_data <- data.frame(
  period = rep(c("Pre-Mandate", "Post-Mandate"), each = 3),
  group = rep(c("California (Actual)", "California (Counterfactual)", "Texas"), 2),
  permits = c(
    # Pre-period
    pre_period$pre_avg[pre_period$state == "California"],
    pre_period$pre_avg[pre_period$state == "California"], 
    pre_period$pre_avg[pre_period$state == "Texas"],
    # Post-period  
    ca_actual,
    ca_counterfactual,
    post_period$post_avg[post_period$state == "Texas"]
  )
) %>%
  mutate(
    line_type = case_when(
      group == "California (Counterfactual)" ~ "dashed",
      TRUE ~ "solid"
    ),
    color = case_when(
      str_detect(group, "California") ~ "blue",
      TRUE ~ "orange"
    )
  )

counterfactual_plot <- counterfactual_data %>%
  ggplot(aes(x = period, y = permits, color = color, group = group, linetype = line_type)) +
  geom_point(size = 4) +
  geom_line(size = 1.5, alpha = 0.8) +
  labs(
    title = "Counterfactual Analysis: What Would Have Happened Without the Mandate?",
    subtitle = "Actual vs predicted California trends",
    x = "Time Period",
    y = "Average Monthly Permits per County",
    caption = "Counterfactual assumes CA would follow TX's trend without mandate"
  ) +
  scale_color_identity() +
  scale_linetype_identity() +
  annotate("text", x = 1.5, y = ca_counterfactual + 50, 
           label = "Treatment Effect", hjust = 0.5, color = "red", size = 4) +
  annotate("segment", x = 2, xend = 2, 
           y = ca_actual, yend = ca_counterfactual,
           arrow = arrow(length = unit(0.3, "cm"), ends = "both"),
           color = "red", size = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

print(counterfactual_plot)

# ===== SAVE REGRESSION RESULTS =====

# Extract coefficients and create results table
permits_results <- data.frame(
  outcome = c("Total Permits", "Single Family", "Multifamily"),
  coefficient = c(
    coef(did_total)["treated_post"],
    coef(did_single)["treated_post"],
    coef(did_multi)["treated_post"]
  ),
  std_error = c(
    se(did_total)["treated_post"],
    se(did_single)["treated_post"],
    se(did_multi)["treated_post"]
  ),
  p_value = c(
    pvalue(did_total)["treated_post"],
    pvalue(did_single)["treated_post"],
    pvalue(did_multi)["treated_post"]
  ),
  percent_effect = c(
    (exp(coef(did_total)["treated_post"]) - 1) * 100,
    (exp(coef(did_single)["treated_post"]) - 1) * 100,
    (exp(coef(did_multi)["treated_post"]) - 1) * 100
  )
)

# Save results as CSV
write_csv(permits_results, "data/processed/permits_did_results.csv")

# Save robustness check results
robustness_results <- data.frame(
  specification = c("Main Model", "With State Trends", "Levels"),
  coefficient = c(
    coef(did_total)["treated_post"],
    trend_coef,
    levels_coef
  ),
  std_error = c(
    se(did_total)["treated_post"],
    trend_se,
    levels_se
  )
)

write_csv(robustness_results, "data/processed/robustness_check_results.csv")

# ===== SAVE PROFESSIONAL RESULTS TABLE =====

# Create HTML table
modelsummary(list("Total" = did_total, "Single Family" = did_single, "Multifamily" = did_multi),
             output = "output/permits_results_table.html",
             title = "Effects of California Solar Mandate on Building Permits")

# Save as CSV too
modelsummary(list("Total" = did_total, "Single Family" = did_single, "Multifamily" = did_multi),
             output = "output/permits_results_table.csv")

# ===== SAVE PLOTS =====

ggsave("output/classic_did_plot.png", classic_did, width = 10, height = 6)
ggsave("output/timeseries_did_plot.png", timeseries_did, width = 12, height = 6)
ggsave("output/counterfactual_plot.png", counterfactual_plot, width = 10, height = 6)

# ===== SAVE ANALYSIS DATA =====

write_csv(analysis_data, "data/processed/did_analysis_data.csv")
