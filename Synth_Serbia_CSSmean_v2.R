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

serbia <- data %>% filter(country == "Serbia")

# Identify Serbia's transition year (first year autocracy == 1 after 0)
serbia <- serbia %>% mutate(autocracy_lag = lag(autocracy))
transition_year <- serbia$year[which(serbia$autocracy_lag == 0 & serbia$autocracy == 1)[1]]
if (is.na(transition_year)) stop("No autocracy transition found for Serbia.")

cat("Transition year for Serbia:", transition_year, "\n")

# Define pre- and post-treatment periods
pre_period <- min(serbia$year):(transition_year - 1)
post_period <- transition_year:max(serbia$year)

cat("Pre-period:", min(pre_period), "to", max(pre_period), "\n")
cat("Post-period:", min(post_period), "to", max(post_period), "\n")

# Donor pool: countries that never experience autocracy == 1
donor_countries <- data %>% 
  group_by(country) %>% 
  summarise(max_autocracy = max(autocracy, na.rm = TRUE)) %>% 
  filter(max_autocracy == 0) %>% 
  pull(country)

cat("Initial donor pool size:", length(donor_countries), "\n")

# Prepare data for Synth
synth_data <- data %>% filter(country %in% c("Serbia", donor_countries))

# Create numeric country id for Synth
synth_data$country_id <- as.numeric(as.factor(synth_data$country))

# --- Balance the panel for Synth ---
# Get the full set of years in the analysis window
all_years <- min(pre_period):max(post_period)
all_countries <- unique(synth_data$country)

# Create a complete panel (country x year)
complete_panel <- expand.grid(country = all_countries, year = all_years, stringsAsFactors = FALSE)

# Merge with synth_data to fill in missing years with NA
synth_data_balanced <- merge(complete_panel, synth_data, by = c("country", "year"), all.x = TRUE)

# Recreate country_id
synth_data_balanced$country_id <- as.numeric(as.factor(synth_data_balanced$country))

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
good_countries <- country_years %>% filter(n_years == n_pre_years & n_nonmiss == n_pre_years) %>% pull(country)

# Restrict to only these countries
synth_data_balanced <- synth_data_balanced %>% filter(country %in% good_countries)

# Update donor_countries to only those with complete pre-period data
donor_countries <- intersect(donor_countries, good_countries)

# Remove Serbia from donor pool if it somehow got included
donor_countries <- donor_countries[donor_countries != "Serbia"]

cat("Final donor pool size:", length(donor_countries), "\n")

# --- Use pre-treatment cssmean as predictors ---
# We'll use the mean cssmean in the pre-period as a single predictor
pre_means <- synth_data_balanced %>%
  filter(year %in% pre_period) %>%
  group_by(country) %>%
  summarise(pre_cssmean = mean(cssmean, na.rm = TRUE))

synth_data_balanced <- left_join(synth_data_balanced, pre_means, by = "country")

# Now run Synth with pre_cssmean as the predictor
# Remove any units with missing pre_cssmean
synth_data_balanced <- synth_data_balanced %>% filter(!is.na(pre_cssmean))

# Verify Serbia is still in the data
if (!"Serbia" %in% synth_data_balanced$country) {
  stop("Serbia was removed during filtering. It likely has missing cssmean data in the pre-period.")
}

cat("Countries remaining after filtering:", length(unique(synth_data_balanced$country)), "\n")

# Run dataprep with pre_cssmean as predictor
dataprep.out <- dataprep(
  foo = synth_data_balanced,
  predictors = "pre_cssmean",
  predictors.op = "mean",
  dependent = "cssmean",
  unit.variable = "country_id",
  time.variable = "year",
  treatment.identifier = unique(synth_data_balanced$country_id[synth_data_balanced$country == "Serbia"]),
  controls.identifier = unique(synth_data_balanced$country_id[synth_data_balanced$country %in% donor_countries]),
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

treat_year <- transition_year

plot_df <- data.frame(
  year = years,
  Serbia = as.numeric(actual),
  Synthetic = as.numeric(synth)
)
library(tidyr)
plot_df_long <- pivot_longer(plot_df, cols = c("Serbia", "Synthetic"), names_to = "Series", values_to = "CSSmean")

p <- ggplot(plot_df_long, aes(x = year, y = CSSmean, color = Series)) +
  geom_line(size = 1) +
  geom_vline(xintercept = treat_year, linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(limits = c(plot_years_start, plot_years_end), breaks = seq(plot_years_start, plot_years_end, 2)) +
  labs(title = "Synthetic Control: Serbia (CSSmean)",
       subtitle = paste("Vertical line: treatment year", treat_year),
       x = "Year", y = "CSS Mean") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Serbia" = "steelblue", "Synthetic" = "darkorange"))

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
