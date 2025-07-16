# ===== PREPARING AND PROCESSING COUNTY DATA FOR ANALYSIS =====

# ===== STEP 1: MERGING COUNTY CSV FILES FOR CA =====

library(tidyverse)
library(janitor)
library(ggplot2)

# Create folders if needed
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# List all CSV files in data/raw/
csv_files <- list.files("data/raw/", pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No CSV files found in data/raw/. Please download them first.")
}

cat("Found", length(csv_files), "CSV files\n")
print(csv_files)

all_data <- map_dfr(csv_files, function(file) {
  
  cat("Reading:", basename(file), "\n")
  
  # Read the CSV - it's already in long format!
  data <- read_csv(file, show_col_types = FALSE) %>%
    clean_names() %>%
    # Add file name as county identifier
    mutate(county_file = basename(file)) %>%
    # Clean up the series names to match what we want
    mutate(
      permit_type = case_when(
        str_detect(series, "Total") ~ "total_permits",
        str_detect(series, "Single") ~ "single_family", 
        str_detect(series, "Multi") ~ "multifamily",
        TRUE ~ "other"
      )
    ) %>%
    # Keep only the columns we need
    select(county_file, location, year, month, permit_type, permits)
  
  return(data)
})

# Check the result
print(head(all_data))
print(unique(all_data$permit_type))

# Basic cleaning - handle duplicates
ca_final_data <- all_data %>%
  # Filter to main permit types
  filter(permit_type %in% c("total_permits", "single_family", "multifamily")) %>%
  # Create county names from file names
  mutate(
    county_name = str_remove(county_file, "\\.csv$"),
    county_name = str_replace_all(county_name, "_", " "),
    county_name = str_to_title(county_name)
  ) %>%
  # Handle duplicates by summing permits for same county/year/month/permit_type
  group_by(county_name, location, year, month, permit_type) %>%
  summarise(permits = sum(permits, na.rm = TRUE), .groups = "drop") %>%
  # Now convert to wide format
  spread(key = permit_type, value = permits, fill = 0) %>%
  # Add basic variables
  mutate(
    treated = 1,  # All California counties are treated
    post = ifelse(year >= 2020, 1, 0)  # Post indicator based on year
  )
ca_final_data <- ca_final_data %>%
  mutate(date = ymd(paste(year, month, "01")))

# Save the California data
write_csv(ca_final_data, "data/processed/ca_permits_final.csv")

# Data check 

cat("Counties:", n_distinct(ca_final_data$county_name), "\n")
cat("Years:", min(ca_final_data$year), "to", max(ca_final_data$year), "\n")
cat("Total rows:", nrow(ca_final_data), "\n")

# ===== SUMMARY STATISTICS AND VISUALIZATIONS FOR CA DATA =====

ca_summary <- final_data %>%
  group_by(county_name) %>%
  summarise(
    observations = n(),
    avg_total_permits = round(mean(total_permits, na.rm = TRUE), 1),
    avg_single_family = round(mean(single_family, na.rm = TRUE), 1),
    avg_multifamily = round(mean(multifamily, na.rm = TRUE), 1),
    .groups = "drop"
  )

print("California Summary by County:")
print(ca_summary)

# Preliminary visualization for CA Counties
ca_plot <- ca_final_data %>%
  group_by(date) %>%
  summarise(total_permits = sum(total_permits, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = date, y = total_permits)) +
  geom_line(size = 1, color = "blue") +
  geom_vline(xintercept = ymd("2020-01-01"), linetype = "dashed", color = "red") +
  labs(title = "California Building Permits Over Time",
       subtitle = "Red line = Title 24 mandate",
       x = "Date", y = "Total Monthly Permits") +
  theme_minimal()

print(ca_plot)


# ===== STEP 2: MERGING COUNTY CSV FILES FOR TX =====

# List Texas CSV files
tx_csv_files <- list.files("data/raw/", pattern = "^tx_.*\\.csv$", full.names = TRUE)

# Process Texas data (exact same as CA)
tx_all_data <- map_dfr(tx_csv_files, function(file) {
  cat("Reading:", basename(file), "\n")
  data <- read_csv(file, show_col_types = FALSE) %>%
    clean_names() %>%
    mutate(county_file = basename(file)) %>%
    mutate(
      permit_type = case_when(
        str_detect(series, "Total") ~ "total_permits",
        str_detect(series, "Single") ~ "single_family", 
        str_detect(series, "Multi") ~ "multifamily",
        TRUE ~ "other"
      )
    ) %>%
    select(county_file, location, year, month, permit_type, permits)
  return(data)
})

# Clean Texas data (exact same as CA)
tx_final_data <- tx_all_data %>%
  filter(permit_type %in% c("total_permits", "single_family", "multifamily")) %>%
  mutate(
    county_name = str_remove(county_file, "\\.csv$"),
    county_name = str_replace_all(county_name, "_", " "),
    county_name = str_to_title(county_name)
  ) %>%
  group_by(county_name, location, year, month, permit_type) %>%
  summarise(permits = sum(permits, na.rm = TRUE), .groups = "drop") %>%
  spread(key = permit_type, value = permits, fill = 0) %>%
  mutate(
    treated = 0,  # Texas is control
    post = ifelse(year >= 2020, 1, 0)
  )
tx_final_data <- tx_final_data %>%
  mutate(date = ymd(paste(year, month, "01")))


# Save the Texas data
write_csv(tx_final_data, "data/processed/tx_permits_final.csv")

# ===== SUMMARY STATISTICS AND VISUALIZATIONS FOR TX DATA =====

tx_summary <- tx_final_data %>%
  group_by(county_name) %>%
  summarise(
    observations = n(),
    avg_total_permits = round(mean(total_permits, na.rm = TRUE), 1),
    avg_single_family = round(mean(single_family, na.rm = TRUE), 1),
    avg_multifamily = round(mean(multifamily, na.rm = TRUE), 1),
    min_date = min(date),
    max_date = max(date),
    .groups = "drop"
  )

print(tx_summary)

# Texas time series plot
tx_plot <- tx_final_data %>%
  group_by(date) %>%
  summarise(total_permits = sum(total_permits, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = date, y = total_permits)) +
  geom_line(size = 1, color = "orange") +
  geom_vline(xintercept = ymd("2020-01-01"), linetype = "dashed", color = "red", alpha = 0.7) +
  labs(
    title = "Texas Building Permits Over Time",
    subtitle = "Combined total for selected counties (red line = CA mandate date for reference)",
    x = "Date",
    y = "Total Monthly Permits"
  ) +
  theme_minimal()

print(tx_plot)


# ===== STEP 3: COMBINING AND STANDARDIZING CA AND TX DATASETS =====

# LOADING DATASETS
ca_data <- read_csv("data/processed/ca_permits_final.csv")
tx_data <- read_csv("data/processed/tx_permits_final.csv")

# COMBINING DATASETS

# Make sure both datasets have the same structure
ca_clean <- ca_data %>%
  mutate(
    state = "California",
    treated = 1,
    post = ifelse(year >= 2020, 1, 0),
    treated_post = treated * post,
    date = ymd(paste(year, month, "01"))
  ) %>%
  # Make sure we have consistent column names
  select(county_name, state, year, month, date, treated, post, treated_post,
         total_permits, single_family, multifamily)

tx_clean <- tx_data %>%
  mutate(
    state = "Texas", 
    treated = 0,
    post = ifelse(year >= 2020, 1, 0),
    treated_post = treated * post,
    date = ymd(paste(year, month, "01"))
  ) %>%
  # Make sure we have consistent column names
  select(county_name, state, year, month, date, treated, post, treated_post,
         total_permits, single_family, multifamily)

combined_data <- bind_rows(ca_clean, tx_clean) %>%
  filter(!is.na(date), year >= 2018, year <= 2022) %>%
  arrange(state, county_name, date)

# Save combined dataset
write_csv(combined_data, "data/processed/ca_tx_combined.csv")

# ===== STEP 4: PRELIMINARY COMPARITIVE VISUALIZATIONS =====

# Overall comparison (aggregated)
comparison_total <- combined_data %>%
  group_by(date, state) %>%
  summarise(
    total_permits = sum(total_permits, na.rm = TRUE),
    avg_permits_per_county = mean(total_permits, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = date, y = avg_permits_per_county, color = state)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = ymd("2020-01-01"), linetype = "dashed", color = "red", alpha = 0.8) +
  labs(
    title = "Building Permits: California vs Texas",
    subtitle = "Average monthly permits per county (red line = CA Title 24 mandate)",
    x = "Date",
    y = "Average Monthly Permits per County",
    color = "State"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("California" = "blue", "Texas" = "orange")) +
  theme(legend.position = "bottom")

print(comparison_total)

# ===== STEP 5: PARALLEL TRENDS CHECK =====

# Pre-treatment period only (2018-2019)
parallel_trends <- combined_data %>%
  filter(year <= 2019) %>%
  group_by(date, state) %>%
  summarise(avg_total_permits = mean(total_permits, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = date, y = avg_total_permits, color = state)) +
  geom_line(size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", alpha = 0.3) +
  labs(
    title = "Parallel Trends Check: Pre-Treatment Period (2018-2019)",
    subtitle = "Do CA and TX follow similar trends before the mandate?",
    x = "Date",
    y = "Average Monthly Permits per County",
    color = "State"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("California" = "blue", "Texas" = "orange")) +
  theme(legend.position = "bottom")

print(parallel_trends)

ggsave("figures/parallel_trends_check.png", parallel_trends, width = 10, height = 6)



# Create US map highlighting CA and TX
library(ggplot2)
library(maps)
library(dplyr)

# Get US state data
us_states <- map_data("state")

# Create treatment indicator
us_states <- us_states %>%
  mutate(
    treatment_group = case_when(
      region == "california" ~ "Treatment (California)",
      region == "texas" ~ "Control (Texas)",
      TRUE ~ "Other States"
    )
  )

# Create the map
us_map <- ggplot(us_states, aes(x = long, y = lat, group = group, fill = treatment_group)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "Treatment (California)" = "blue",
      "Control (Texas)" = "orange", 
      "Other States" = "lightgray"
    ),
    name = "Study Areas"
  ) +
  labs(
    title = "Treatment and Control States",
    subtitle = "California (Treatment) vs Texas (Control)"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  coord_fixed(1.3)

# Save the map
ggsave("figures/treatment_control_map.png", us_map, width = 10, height = 6)

cat("✅ US map saved to figures/treatment_control_map.png\n")
