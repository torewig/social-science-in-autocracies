# ============================================================================
# Synth_Serbia_CSSmean.R
# Synthetic control for Serbia: cssmean ~ autocracy
# ============================================================================

if (!requireNamespace("Synth", quietly = TRUE)) install.packages("Synth")
library(Synth)
library(dplyr)
library(ggplot2)

# Load operationalized data
input_path <- "Data/cy_operationalized.rds"
data <- readRDS(input_path)

# Filter for Serbia and donor pool (countries with no autocracy transition)
data <- data %>% arrange(country, year)

target_country <- data %>% filter(country == "Serbia")

# Identify Serbia's transition year (first year autocracy == 1 after 0)
target_country <- target_country %>% mutate(autocracy_lag = lag(autocracy))
transition_year <- target_country$year[which(target_country$autocracy_lag == 0 & target_country$autocracy == 1)[1]]
if (is.na(transition_year)) stop("No autocracy transition found for Serbia.")

cat("Transition year for Serbia:", transition_year, "\n")

# Define pre- and post-treatment periods
pre_period <- min(target_country$year):(transition_year - 1)
post_period <- transition_year:max(target_country$year)

# Donor pool: countries that never experience autocracy == 1
donor_countries <- data %>% group_by(country) %>% summarise(max_autocracy = max(autocracy, na.rm = TRUE)) %>% filter(max_autocracy == 0) %>% pull(country)

# Prepare data for Synth
synth_data <- data %>% filter(country %in% c("Serbia", donor_countries))

# Create numeric country id for Synth - create a stable mapping
country_id_map <- data.frame(country = unique(synth_data$country), stringsAsFactors = FALSE)
country_id_map$country_id <- seq_len(nrow(country_id_map))
synth_data <- left_join(synth_data, country_id_map, by = "country")

# Store the treated unit ID before any filtering
serbia_id <- unique(synth_data$country_id[synth_data$country == "Serbia"])

# --- Balance the panel for Synth ---
# Get the full set of years in the analysis window
all_years <- min(pre_period):max(post_period)
all_countries <- unique(synth_data$country)

# Create a complete panel (country x year)
complete_panel <- expand.grid(country = all_countries, year = all_years, stringsAsFactors = FALSE)

# Merge with synth_data to fill in missing years with NA
synth_data_balanced <- merge(complete_panel, synth_data, by = c("country", "year"), all.x = TRUE)

# Add country_id back using the stable mapping (remove if it exists from merge)
synth_data_balanced$country_id <- NULL
synth_data_balanced <- left_join(synth_data_balanced, country_id_map, by = "country")

# Remove countries with too many missing pre-period cssmean (e.g., more than 2 missing in pre-period)
pre_period_mask <- synth_data_balanced$year %in% pre_period
country_missing_pre <- synth_data_balanced %>%
  filter(pre_period_mask) %>%
  group_by(country) %>%
  summarise(n_missing = sum(is.na(cssmean)))

# Keep only countries with <=2 missing pre-period cssmean
good_countries <- country_missing_pre %>% filter(n_missing <= 2) %>% pull(country)
synth_data_balanced <- synth_data_balanced %>% filter(country %in% good_countries)

# --- Ensure fully balanced panel for pre-period ---
pre_period_years <- pre_period

# Identify countries with complete cssmean in all pre-period years
country_years <- synth_data_balanced %>%
  filter(year %in% pre_period_years) %>%
  group_by(country) %>%
  summarise(n_years = n(), n_nonmiss = sum(!is.na(cssmean)))

n_pre_years <- length(pre_period_years)
# For donor pool: require complete data
good_donor_countries <- country_years %>% filter(n_years == n_pre_years & n_nonmiss == n_pre_years) %>% pull(country)

# Update donor_countries to only those with complete pre-period data
donor_countries <- intersect(donor_countries, good_donor_countries)

# Keep Serbia and the good donor countries
synth_data_balanced <- synth_data_balanced %>% filter(country %in% c("Serbia", donor_countries))

cat("Number of donor countries:", length(donor_countries), "\n")

# --- Use pre-treatment cssmean as predictors ---
# We'll use the mean cssmean in the pre-period as a single predictor
pre_means <- synth_data_balanced %>%
  filter(year %in% pre_period) %>%
  group_by(country) %>%
  summarise(pre_cssmean = mean(cssmean, na.rm = TRUE))

# Convert NaN to NA and check
pre_means$pre_cssmean[is.nan(pre_means$pre_cssmean)] <- NA

# Remove pre_cssmean if it already exists before joining
if ("pre_cssmean" %in% names(synth_data_balanced)) {
  synth_data_balanced$pre_cssmean <- NULL
}
synth_data_balanced <- left_join(synth_data_balanced, pre_means, by = "country")

# Check if Serbia is still in the dataset
if (!"Serbia" %in% synth_data_balanced$country) {
  stop("Serbia was removed during data preparation. Check Serbia data availability.")
}

# Check if Serbia has valid pre_cssmean
serbia_check <- synth_data_balanced %>% filter(country == "Serbia") %>% select(pre_cssmean) %>% distinct()
if (nrow(serbia_check) == 0 || all(is.na(serbia_check$pre_cssmean))) {
  stop("Serbia has no valid pre_cssmean values. All cssmean values in pre-period are missing.")
}

# Run dataprep with pre_cssmean as predictor
# Get the control IDs (all donor countries that remain after filtering)
control_ids <- unique(synth_data_balanced$country_id[synth_data_balanced$country %in% donor_countries])

# Verify we have a single treated unit
treated_id <- unique(synth_data_balanced$country_id[synth_data_balanced$country == "Serbia"])
cat("Number of treated IDs found:", length(treated_id), "\n")
if (length(treated_id) != 1) {
  cat("Serbia entries in synth_data_balanced:\n")
  print(table(synth_data_balanced$country == "Serbia"))
  cat("Unique country_id values for Serbia:\n")
  print(synth_data_balanced$country_id[synth_data_balanced$country == "Serbia"])
  stop("Multiple or no treated unit IDs found for Serbia")
}

cat("Treated unit ID (Serbia):", treated_id, "\n")
cat("Number of control units:", length(control_ids), "\n")

dataprep.out <- dataprep(
  foo = synth_data_balanced,
  predictors = "pre_cssmean",
  predictors.op = "mean",
  dependent = "cssmean",
  unit.variable = "country_id",
  time.variable = "year",
  treatment.identifier = treated_id,
  controls.identifier = control_ids,
  time.predictors.prior = pre_period,
  time.optimize.ssr = pre_period,
  unit.names.variable = "country",
  time.plot = c(pre_period, post_period)
)

synth.out <- synth(dataprep.out)

# Plot results
synth.plot <- path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
                       Ylab = "CSS Mean", Xlab = "Year",
                       Legend = c("Serbia", "Synthetic Serbia"),
                       Legend.position = "bottomright")

# Save plot
png("synth_serbia_cssmean.png", width = 800, height = 500)
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "CSS Mean", Xlab = "Year",
          Legend = c("Serbia", "Synthetic Serbia"),
          Legend.position = "bottomright")
dev.off()
cat("Synthetic control plot saved as synth_serbia_cssmean.png\n")

# --- Custom plot with treatment-year vertical line and zoom ---
# Determine appropriate zoom range
plot_years_start <- max(min(pre_period), transition_year - 15)
plot_years_end <- min(max(post_period), transition_year + 10)
plot_years <- plot_years_start:plot_years_end

actual <- dataprep.out$Y1plot[which(dataprep.out$tag$time.plot %in% plot_years)]
synth <- dataprep.out$Y0plot %*% synth.out$solution.w[ , 1]
synth <- synth[which(dataprep.out$tag$time.plot %in% plot_years)]
years <- plot_years

treat_year <- 2014

plot_df <- data.frame(
  year = years,
  Actual = as.numeric(actual),
  Synthetic = as.numeric(synth)
)
library(tidyr)
plot_df_long <- pivot_longer(plot_df, cols = c("Actual", "Synthetic"), names_to = "Series", values_to = "CSSmean")

p <- ggplot(plot_df_long, aes(x = year, y = CSSmean, color = Series)) +
  geom_line(size = 1) +
  geom_vline(xintercept = treat_year, linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(limits = c(plot_years_start, plot_years_end), breaks = seq(plot_years_start, plot_years_end, 2)) +
  labs(title = "Synthetic Control: Serbia (CSSmean)",
       subtitle = paste("Vertical line: treatment year", treat_year),
       x = "Year", y = "CSS Mean") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Actual" = "steelblue", "Synthetic" = "darkorange"))

print(p)
ggsave("synth_serbia_cssmean_zoomed.png", plot = p, width = 8, height = 5, dpi = 300)
cat("Zoomed synthetic control plot saved as synth_serbia_cssmean_zoomed.png\n")

# Print unit weights
cat("\nDonor country weights:\n")
weights_df <- data.frame(
  country = dataprep.out$names.and.numbers$unit.names[dataprep.out$names.and.numbers$unit.numbers %in% dataprep.out$tag$controls.identifier],
  weight = synth.out$solution.w
)
weights_df <- weights_df %>% filter(weight > 0.001) %>% arrange(desc(weight))
print(weights_df, row.names = FALSE)

