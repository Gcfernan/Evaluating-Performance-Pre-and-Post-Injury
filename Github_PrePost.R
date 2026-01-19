# Load required libraries 
library(dplyr)      
library(tidyr)      
library(ggplot2)    
library(mgcv)       
library(emmeans)    
library(readr)      

# ========================================
# Data Import and Combination
# ========================================
# Read in pre- and post-event datasets 
pre_data <- read_csv("data/pre_injury_data.csv")
post_data <- read_csv("data/post_injury_data.csv")

# Combine the datasets and add a column indicating pre- or post-event status.
combined_data <- bind_rows(
  pre_data %>% mutate(status = "Pre-Injury"),
  post_data %>% mutate(status = "Post-Injury")
)

# ========================================
# Data Cleaning and Transformation
# ========================================
# Rename columns to generic, easy-to-follow names.
cleaned_data <- combined_data %>%
  rename(
    game_id = game_id,                 # Game identifier
    pitcher = pitcher,                 # Athlete or pitcher identifier
    pitch_type = final_pitch_type,     # Type of pitch thrown
    date = date,                       # Date of the game/event
    pitch_count = game_pitch_count,    # Count of pitches
    pitch_velocity = velocity,         # Measured pitch velocity
    vertical_break = vert_break,       # Vertical movement of the pitch
    horizontal_break = horiz_break,    # Horizontal movement of the pitch
    max_velocity = max_velocity,       # Maximum pitch velocity in a game
    top_5_mean_velocity = top5_mean_velocity,  # Mean of top 5 velocities in a game
    team = team,                       # Team identifier
    league = league,                   # League identifier
    week = week                        # Week of the season
  ) %>%
  mutate(
    pitch_type = factor(pitch_type),
    team = factor(team),
    league = factor(league),
    status = factor(status)
  )

# Convert date strings to Date objects, order events for each pitcher,
# and calculate the number of weeks from the first post-event (e.g., injury) date.
cleaned_data <- cleaned_data %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  group_by(pitcher) %>%
  arrange(pitcher, date) %>%
  mutate(
    appearance = dense_rank(date),
    event_date = min(date[status == "Post-Injury"], na.rm = TRUE),  # First post-event date per pitcher
    weeks_from_event = as.numeric(difftime(date, event_date, units = "weeks"))
  ) %>%
  ungroup()

# Define parts of the season (Beginning, Middle, End)
season_date_ranges <- cleaned_data %>%
  group_by(league) %>%
  summarise(min_date = min(date), max_date = max(date))

cleaned_data <- cleaned_data %>%
  left_join(season_date_ranges, by = "league") %>%
  mutate(date_category = case_when(
    date <= min_date + (max_date - min_date) * 0.33 ~ "Beginning",
    date > min_date + (max_date - min_date) * 0.33 &
      date <= min_date + (max_date - min_date) * 0.66 ~ "Middle",
    date > min_date + (max_date - min_date) * 0.66 ~ "End",
    TRUE ~ NA_character_
  )) %>%
  mutate(date_category = factor(date_category, levels = c("Beginning", "Middle", "End")))

# ========================================
# Exploratory Data Analysis (EDA)
# ========================================
# Compute summary statistics for pitch metrics grouped by status and pitch type.
summary_stats <- cleaned_data %>%
  group_by(status, pitch_type) %>%
  summarise(
    mean_velocity = mean(pitch_velocity, na.rm = TRUE),
    sd_velocity = sd(pitch_velocity, na.rm = TRUE),
    var_velocity = var(pitch_velocity, na.rm = TRUE),
    mean_vertical_break = mean(vertical_break, na.rm = TRUE),
    sd_vertical_break = sd(vertical_break, na.rm = TRUE),
    var_vertical_break = var(vertical_break, na.rm = TRUE),
    mean_horizontal_break = mean(horizontal_break, na.rm = TRUE),
    sd_horizontal_break = sd(horizontal_break, na.rm = TRUE),
    var_horizontal_break = var(horizontal_break, na.rm = TRUE)
  )

# ========================================
# Adding Event Timing Data 
# ========================================
# Create a data frame with generic identifiers and event timing values.
event_time_data <- data.frame(
  pitcher = c("ID1", "ID2", "ID3", "ID4", "ID5", "ID6", "ID7", "ID8", "ID9", "ID10"),
  event_time = c(14, 24, 33, 53, 65, 10, 23, 42, 9, 11)
)

# Merge the event timing data with the main dataset.
cleaned_data <- cleaned_data %>%
  left_join(event_time_data, by = "pitcher")

# Check for missing event_time values and ensure the column is numeric.
if (any(is.na(cleaned_data$event_time))) {
  cat("Warning: There are missing values in the event_time column.\n")
}
cleaned_data$event_time <- as.numeric(cleaned_data$event_time)
event_time -> Injury_Time
# ========================================
# Visualization: Density Plots
# ========================================
# Plot density curves for pitch velocity comparing pre- and post-event statuses.
ggplot(cleaned_data, aes(x = pitch_velocity, fill = status)) +
  geom_density(alpha = 0.5) +
  labs(title = "Pitch Velocity: Pre vs. Post Event",
       x = "Velocity (mph)", y = "Density")

# Additional density plots can be made for vertical and horizontal break.

# ========================================
# Statistical Modeling with GAMs
# ========================================
# Set "Post-Injury" as the reference level for the status factor.
cleaned_data$status <- relevel(cleaned_data$status, ref = "Post-Injury")
cleaned_data$pitcher <- as.factor(cleaned_data$pitcher)
cleaned_data$appearance <- as.factor(cleaned_data$appearance)

# Fit a GAM for pitch velocity using smooth functions for non-linear effects.
gam_velocity <- gam(pitch_velocity ~ injury_status + s(pitch_count)  + 
                      s(vertical_break) + s(horizontal_break) + s(pitcher, bs = "re")+
                      Injury_Time + ti(Injury_Time, by = injury_status) + s(Injury_Time, pitcher, bs = "re"),
                    data = cleaned_data, method = "REML")
velocity_summary <- summary(gam_velocity)



# ========================================
# Sensitivity Analysis
# ========================================
# Create alternative metrics such as the maximum and top-5 mean velocity per game.
sensitivity_data <- cleaned_data %>%
  group_by(game_id, pitcher) %>%
  mutate(
    max_velocity = max(pitch_velocity, na.rm = TRUE),
    top_5_mean_velocity = mean(sort(pitch_velocity, decreasing = TRUE)[1:5], na.rm = TRUE),
    max_vertical_break = max(vertical_break, na.rm = TRUE),
    max_horizontal_break = max(horizontal_break, na.rm = TRUE)
  )

# Fit a GAM using the maximum velocity metric as an example.
gam_max_velocity <- ggam(max_velocity ~ injury_status + pitch_count  + 
                           s(vertical_break) + s(horizontal_break) + s(pitcher, bs = "re")+
                           Injury_Time + ti(Injury_Time, by = injury_status) + s(Injury_Time, pitcher, bs = "re"),
                         data = sensitivity_data, method = "REML")
max_velocity_summary <- summary(gam_max_velocity)

# ========================================
# Estimated Marginal Means (EMMs) and Plotting
# ========================================
# Calculate estimated marginal means for pitch velocity by status.
emm_velocity <- emmeans(gam_velocity, ~ status)
emm_velocity_CI <- confint(emm_velocity, level = 0.95)
print(emm_velocity_CI)

# Plot the EMMs with confidence intervals.
plot_velocity <- plot(emm_velocity, comparisons = TRUE, 
                      main = "Estimated Marginal Means: Pitch Velocity",
                      xlab = "Status", ylab = "Estimated Velocity (mph)", 
                      ci.style = "bars", add.points = TRUE) + 
  theme_minimal()
ggsave("emm_velocity_plot.png", plot = plot_velocity)

# ========================================
# Residual Analysis and Variance Decomposition
# ========================================
# Create a residual plot to assess model fit.
plot(resid(gam_velocity), main = "Residuals: Velocity GAM", 
     ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)

# Generate a QQ plot to check normality of residuals.
qqnorm(resid(gam_velocity))
qqline(resid(gam_velocity), col = "red")



# ========================================
# Extra Visualizations
# ========================================
# Reshape the data to a long format for plotting multiple metrics over time.
long_data <- cleaned_data %>%
  select(pitcher, weeks_from_event, pitch_velocity, vertical_break, horizontal_break, pitch_type) %>%
  gather(key = "Metric", value = "Value", pitch_velocity, vertical_break, horizontal_break)

# Focus on a specific pitch type
fastball_data <- long_data %>%
  filter(pitch_type == "Fastball")

# Loop through each metric to generate time series plots.
metrics <- c("pitch_velocity", "vertical_break", "horizontal_break")
for (metric in metrics) {
  metric_plot <- ggplot(fastball_data %>% filter(Metric == metric), 
                        aes(x = weeks_from_event, y = Value, group = pitcher, color = pitcher)) +
    geom_line() +  
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
    geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "black", linewidth = 1.2) + 
    labs(title = paste(metric, "Over Weeks from Event (Fastballs)"),
         x = "Weeks from Event",
         y = metric) +
    theme_minimal() +
    theme(legend.position = "none")
  print(metric_plot)
}

# ========================================
# Analysis of Changes in Pitch Metrics
# ========================================
# Calculate the change in pitch velocity from pre- to post-event for each pitcher.
velocity_change <- cleaned_data %>%
  group_by(pitcher) %>%
  summarize(
    pre_velocity = mean(pitch_velocity[status == "Pre-Injury"], na.rm = TRUE),
    post_velocity = mean(pitch_velocity[status == "Post-Injury"], na.rm = TRUE),
    velocity_change = post_velocity - pre_velocity
  )
print(velocity_change)

# ========================================
# Extracting and Visualizing Random Effects
# ========================================
# Obtain fitted values from the velocity GAM and compute random effects.
fitted_values <- predict(gam_velocity, type = "response")
fixed_effects_pred <- predict(gam_velocity, type = "terms")
random_effects <- fitted_values - rowSums(fixed_effects_pred)
random_effects_df <- data.frame(
  pitcher = cleaned_data$pitcher,  
  random_effect = random_effects
)

# Filter the random effects for selected pitchers 
selected_pitchers <- c("ID1", "ID3", "ID4", "ID5")
random_effects_selected <- random_effects_df %>%
  filter(pitcher %in% selected_pitchers)
print(random_effects_selected)

# Visualize the random effects with a bar plot.
ggplot(random_effects_selected, aes(x = pitcher, y = random_effect)) +
  geom_bar(stat = "identity") +
  labs(title = "Random Effects for Selected Pitchers",
       x = "Pitcher",
       y = "Random Effect") +
  theme_minimal()

