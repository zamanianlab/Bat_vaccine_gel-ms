# Libraries
lapply(
  c("dplyr", "tidyr", "scales", "patchwork", "readr", "rstan",
    "ggplot2", "ggpattern", "purrr", "truncnorm", "lubridate",
    "ggforce", "grid", "tibble", "ggtext", "cowplot"),
  library,
  character.only = TRUE
)

# Load data
bat_data <- read.csv("Jirosto_capture_data.csv", stringsAsFactors = FALSE)
rfid_folder <- "RFID/Jirosto/"

################################################################################

bat_data <- bat_data %>%
  # Convert and rename
  mutate(capture_date = as.Date(Capture.Date, format = "%m/%d/%Y")) %>%
  rename(
    bat_id       = Unique.bat,
    hair_sample  = Hair.Sample,
    treatment_applied = Gel.Applied,
    previous_rfid = Previous.RFID
  ) %>%
  # Filter out missing or invalid data
  filter(
    !is.na(capture_date),
    !is.na(Age) & Age != ""
  ) %>%
  # Create Cohorts
  mutate(
    Age = ifelse(Age %in% c("SA", "J"), "J", "A"), # group SAs and Js
    Age = factor(Age, levels = c("A", "J")),
    Sex      = factor(Sex, levels = c("M", "F")),
    Cohort   = case_when(
      Age == "A" & Sex == "M" ~ "AM",
      Age == "A" & Sex == "F" ~ "AF",
      Age == "J" & Sex == "M" ~ "JM",
      Age == "J" & Sex == "F" ~ "JF",
      TRUE ~ NA_character_
    )
  )

# Identify capture dates
capture_dates <- sort(unique(bat_data$capture_date))

# Count unique bats per date by Cohort
capture_summary <- bat_data %>%
  distinct(capture_date, bat_id, Cohort) %>%
  group_by(capture_date, Cohort) %>%
  summarise(Count = n(), .groups = "drop")

# Sum of bats captured per capture date
total_captures <- capture_summary %>%
  group_by(capture_date) %>%
  summarise(TotalBats = sum(Count), .groups = "drop") %>%
  arrange(capture_date)

# Value of C for mark-recap calculation
c <- total_captures[[1, "TotalBats"]] 

total_recaptures <- bat_data %>%
  filter(capture_date == capture_dates[1], previous_rfid == 1) %>%
  summarise(Count = n()) %>%
  pull(Count)

# Value of R for mark-recap calculation
r <- total_recaptures 

##### Read RFID data ###########################################################

log_files <- list.files(path = rfid_folder, pattern = "\\.csv$", full.names = TRUE)
all_dfs <- list()

for (file_path in log_files) {
  lines <- readLines(file_path)
  
  # Remove leading/trailing quotes from each line
  lines <- gsub('^"|"$', '', lines)
  
  # Filter only lines that start with TAG:
  tag_lines <- grep("^TAG:", lines, value = TRUE)
    if (length(tag_lines) == 0) next
  
  # Split each TAG line by space
  split_lines <- strsplit(tag_lines, " ")
  
  # Convert each line to a data frame row
  df <- do.call(rbind, lapply(split_lines, function(x) {
    data.frame(
      Value = x[2],
      Date = x[3],
      Time = x[4],
      TagID = x[5],
      SourceFile = basename(file_path),
      stringsAsFactors = FALSE
    )
  }))
  
  all_dfs[[length(all_dfs) + 1]] <- df
}

# Combine all data frames into one combined file
combined_rfid_df <- do.call(rbind, all_dfs)

# Count total number of unique IDs
# total_unique_ids <- length(unique(combined_rfid_df$TagID))
# print(paste("Total unique IDs detected:", total_unique_ids))

#### Count unique rfid detections per Night ####################################

# Ensure DateTime is parsed correctly and assign Night values
combined_rfid_df <- combined_rfid_df %>%
  mutate(
    DateTime = as.POSIXct(
      paste(Date, Time),
      format = "%m/%d/%Y %H:%M:%OS",
      tz = "America/Mexico_City"
    ),
    Hour = hour(DateTime),
    Night = case_when(
      Hour < 10 ~ as.Date(DateTime) - days(1),   
      Hour >= 17.5 ~ as.Date(DateTime) - days(1),
      TRUE ~ NA_Date_                             
    )
  )

# Remove test tag and system messages
night_df <- combined_rfid_df %>%
  filter(!is.na(Night)) %>%
  filter(!TagID %in% c("999.000000007425", "989.001040476348"))

# Count unique bats detected per night
night_summary <- night_df %>%
  group_by(Night) %>%
  summarise(UniqueTagCount = n_distinct(TagID), .groups = "drop") %>%
  arrange(Night)

print(night_summary)

####### Population estimate and colony demographics ############################

# Number of unique tags the night before first capture
# Value of M for mark-recapture calculation
m <- night_summary %>%
  filter(Night == (capture_dates[1] - 1)) %>%
  pull(UniqueTagCount)

print(capture_dates[1] - 1)
print(m)

# Calculate colony population estimate
pop_est <- m*c/r

# Calculate variance and standard error of population estimate
var_pop_est <- (m+1)*(c+1)*(m-r)*(c-r)/((r+1)^2*(r+2))
se_pop_est <- sqrt(var_pop_est)

### Adjustment for mortalities at Jirosto
pop_est <-  pop_est - 17

# Count unique bats captured per cohort
unique_bats <- bat_data %>%
  distinct(bat_id, Cohort) %>%  # unique bat IDs per cohort
  count(Cohort, name = "Unique_Bats")

print(unique_bats)

################################################################################
#### Build summary df of treatment application and hair samples ################
################################################################################

# Bat ID, cohort and treatment values of bats on first capture
treatment_df <- bat_data %>%
  group_by(bat_id) %>%
  slice_min(order_by = capture_date, n = 1) %>%
  select(bat_id, Cohort, treatment_applied) %>%
  ungroup() %>%
  mutate(
    treatment_applied = replace_na(treatment_applied, 0),
    treatment_applied = as.numeric(replace(treatment_applied, treatment_applied == "", 0))
  )

# Summarize treated counts separately for RB and R110
treated_counts <- treatment_df %>%
  group_by(Cohort) %>%
  summarise(
    RB_Treated   = sum(treatment_applied == 1),
    R110_Treated = sum(treatment_applied == 2),
    .groups = "drop"
  )

print(treated_counts)

# Hair samples from all captures
# 0 - Negative; 1 - RB positive; 2 - R110 positive; 3 - Both positive; 4 = Not collected
hair_df <- bat_data %>%
  filter(hair_sample %in% c(0, 1, 2, 3, 4)) %>%   
  select(bat_id, Cohort, hair_sample) %>%          
  distinct(bat_id, Cohort, hair_sample) %>%       # ensure each bat counted once per result
  left_join(treatment_df, by = c("bat_id", "Cohort"))  # merge treatment info

# Summarize per bat (only one row per bat_id)
hair_summary <- hair_df %>%
  group_by(bat_id) %>%
  summarise(
    Cohort = first(Cohort),              
    treatment_applied = first(treatment_applied),   
    RB_Pos = any(hair_sample == 1),
    R110_Pos = any(hair_sample == 2),
    Both_Pos = any(hair_sample == 3),
    Not_Collected = (any(hair_sample == 4)) & (first(treatment_applied) == 0),
    .groups = "drop"
  ) %>%
  group_by(Cohort) %>%
  summarise(
    TotalUniqueBats = n(),
    RB_Treated = sum(treatment_applied == 1),
    R110_Treated = sum(treatment_applied == 2),
    RB_Pos_Only = sum(RB_Pos & treatment_applied != 1 & !R110_Pos),
    R110_Pos_Only = sum(R110_Pos & treatment_applied != 2 & !RB_Pos),
    Both_Positive = sum(Both_Pos & treatment_applied != 1 & treatment_applied != 2),
    Not_Collected = sum(Not_Collected),
    Negative = TotalUniqueBats - RB_Treated - R110_Treated - RB_Pos_Only - R110_Pos_Only - Both_Positive - Not_Collected,
    .groups = "drop"
  )

print(hair_summary)
 
################################################################################
#### Colony simulation propagating uncertainty in population size ##############
################################################################################

### Adjustment to known_bats for mortalities at Jirosto
unique_bats <- unique_bats %>%
 mutate(
   Unique_Bats = case_when(
     Cohort == "AF" ~ Unique_Bats - 14,
     Cohort == "AM" ~ Unique_Bats - 3,
     TRUE ~ Unique_Bats
   )
 )

# Count number of known bats
known_bats <- setNames(as.integer(unique_bats$Unique_Bats), unique_bats$Cohort)
n_known <- sum(known_bats)

# Create dataframe of known bats
known_df <- data.frame(
  bat_id = seq_len(n_known),
  cohort = rep(names(known_bats), times = known_bats),
  Source = "Known"
)

# Adjust hair summary by joining total bats treated and untreated bats
hair_summary_adjusted <- hair_summary %>%
  left_join(
    treated_counts %>%
      rename(Total_RB_Treated  = RB_Treated,
             Total_R110_Treated = R110_Treated),
    by = "Cohort"
  ) %>%
  mutate(
    ColonySize = known_bats[Cohort],
    untreated_bats = ColonySize - coalesce(Total_RB_Treated, 0) - coalesce(Total_R110_Treated, 0)
  )

# Categories for multinomial model
categories <- c("RB_Pos_Only", "R110_Pos_Only", "Both_Positive", "Negative")

# Extract counts matrix of hair samples in categories for Stan
counts_matrix <- as.matrix(hair_summary_adjusted[, categories])
rownames(counts_matrix) <- hair_summary_adjusted$Cohort
print(counts_matrix)

# Stan model code
stan_code <- "
data {
  int<lower=1> N;
  int<lower=2> K;
  int y[N,K];
}
parameters {
  simplex[K] theta[N];
}
model {
  for (n in 1:N)
    y[n] ~ multinomial(theta[n]);
}
generated quantities {
  int y_rep[N,K];
  for (n in 1:N)
    y_rep[n] = multinomial_rng(theta[n], sum(y[n]));
}
"

# Prepare data for Stan ---
stan_data <- list(
  N = nrow(counts_matrix),
  K = ncol(counts_matrix),
  y = counts_matrix
)

# Fit Stan model
fit <- stan(model_code = stan_code, data = stan_data,
            iter = 8000, warmup = 2000, chains = 4, seed = 123)

# Extract posterior draws
posterior <- rstan::extract(fit)
theta_draws <- posterior$theta  # draws x N x K
cat("\nPosterior theta draws dimensions: ", dim(theta_draws), "\n")

# Compute per-bat mean probabilities ---
per_bat_mean <- apply(theta_draws, c(2,3), mean)
colnames(per_bat_mean) <- categories
rownames(per_bat_mean) <- hair_summary_adjusted$Cohort
cat("\nSample per-bat probabilities (posterior mean):\n")
print(round(per_bat_mean, 3))

################################################################################
#### Assign unknown bats to cohorts ############################################
################################################################################

# Number of simulations
n_sims <- dim(theta_draws)[1]

# Simulate estimated colony sizes (NOW with truncnorm)
set.seed(123)
sim_total_colony <- round(rtruncnorm(
  n = n_sims,
  a = n_known,   # left truncation
  b = Inf,       # no upper bound
  mean = pop_est,
  sd = se_pop_est
))

summary_stats <- data.frame(
  mean   = mean(sim_total_colony),
  median = median(sim_total_colony),
  sd     = sd(sim_total_colony),
  q2.5   = quantile(sim_total_colony, 0.025),
  q97.5  = quantile(sim_total_colony, 0.975)
)

print(summary_stats)

# Assign unknown bats to cohorts using probabilities from known bats
cohort_probs <- prop.table(known_bats)

unknown_matrix <- matrix(
  0,
  nrow = nrow(hair_summary_adjusted),
  ncol = n_sims,
  dimnames = list(hair_summary_adjusted$Cohort, NULL)
)

for (i in seq_len(n_sims)) {
  n_unknown_draw <- sim_total_colony[i] - n_known
  
  if (n_unknown_draw > 0) {
    # sample unknown bats into cohorts according to cohort_probs
    unknown_matrix[, i] <- rmultinom(
      n = 1,
      size = n_unknown_draw,
      prob = cohort_probs
    )
  }
  # if n_unknown_draw == 0, column stays all zeros
}

# Adjust colony size by removing treated bats and adding unknown bats
untreated_colony_total <- hair_summary_adjusted$untreated_bats + unknown_matrix

# Bats with known hair samples are removed to calculate totals of unsampled bats
# Using fit, hair sample results will be predicted for these unsampled bats
unsampled_bats <- untreated_colony_total - rowSums(counts_matrix)

# For each simulation, multiply per-bat posterior probabilities by the number of unsampled bats,
# then add back the observed counts from sampled bats
adjusted_colony_draws <- array(NA, dim = c(n_sims, nrow(hair_summary_adjusted), length(categories)),
                               dimnames = list(NULL, hair_summary_adjusted$Cohort, categories))

for (i in seq_len(n_sims)) {
  predicted_unsampled <- sweep(theta_draws[i,,], 1, unsampled_bats[,i], `*`)
  adjusted_colony_draws[i,,] <- predicted_unsampled + counts_matrix
}

# Summarize posterior draws as the mean colony and 2.5% and 97.5% boundaries
adjusted_colony_pred <- apply(adjusted_colony_draws, c(2,3), median)
colnames(adjusted_colony_pred) <- categories
rownames(adjusted_colony_pred) <- hair_summary_adjusted$Cohort

colony_q025 <- apply(adjusted_colony_draws, c(2,3), quantile, probs = 0.025)
colony_q975 <- apply(adjusted_colony_draws, c(2,3), quantile, probs = 0.975)

# Add treated bats back and convert to long format for plotting
plot_df <- as.data.frame(adjusted_colony_pred) %>%
  mutate(
    RB_Treated   = hair_summary_adjusted$Total_RB_Treated,
    R110_Treated = hair_summary_adjusted$Total_R110_Treated,
    Cohort       = hair_summary_adjusted$Cohort
  ) %>%
  pivot_longer(
    cols = all_of(c(categories, "RB_Treated", "R110_Treated")),
    names_to = "Marker",
    values_to = "Count"
  )

# Prepare CI for non-treated categories using colon_025 and colony_975
ci_lower <- as.data.frame(colony_q025)
ci_lower$Cohort <- hair_summary_adjusted$Cohort
ci_lower_long <- ci_lower %>%
  pivot_longer(cols = all_of(categories), names_to = "Marker", values_to = "ymin")

ci_upper <- as.data.frame(colony_q975)
ci_upper$Cohort <- hair_summary_adjusted$Cohort
ci_upper_long <- ci_upper %>%
  pivot_longer(cols = all_of(categories), names_to = "Marker", values_to = "ymax")

# Merge CI with plot_df
plot_df <- plot_df %>%
  left_join(ci_lower_long, by = c("Cohort", "Marker")) %>%
  left_join(ci_upper_long, by = c("Cohort", "Marker")) %>%
  mutate(
    ymin = ifelse(Marker %in% c("RB_Treated", "R110_Treated"), Count, ymin),
    ymax = ifelse(Marker %in% c("RB_Treated", "R110_Treated"), Count, ymax)
  )

plot_df

# Factor levels for plotting
plot_df$Marker <- factor(plot_df$Marker,
                         levels = c("Negative", "R110_Pos_Only", "Both_Positive", "RB_Pos_Only",
                                    "RB_Treated", "R110_Treated", "Both_Treated"))
# # Pattern, angle, and color palettes
# patterns <- c(
#   "RB_Treated"     = "stripe",
#   "R110_Treated"   = "stripe",
#   "RB_Pos_Only"    = "circle",
#   "R110_Pos_Only"  = "circle",
#   "Both_Positive"  = "crosshatch"
# )
# 
# pattern_angles <- c(
#   "RB_Treated"     = 45,   
#   "R110_Treated"   = -45,  
#   "RB_Pos_Only"    = 0,    
#   "R110_Pos_Only"  = 45,    
#   "Both_Positive"  = 45     
# )
# 
# colors <- c(
#   "RB_Pos_Only"   = "orange",
#   "R110_Pos_Only" = "turquoise",
#   "Both_Positive" = "purple",
#   "Negative"      = "grey70",
#   "RB_Treated"    = "red",
#   "R110_Treated"  = "green", 
#   "Both_Treated"  = "pink"
# )

# Factor level order for plotting 
plot_df$Marker <- factor(
  plot_df$Marker,
  levels = c("Negative","R110_Pos_Only","Both_Positive","RB_Pos_Only",
             "RB_Treated","R110_Treated","Both_Treated")
)

# Master palettes
master_patterns <- c(
  "RB_Treated"     = "stripe",
  "R110_Treated"   = "stripe",
  "RB_Pos_Only"    = "circle",
  "R110_Pos_Only"  = "circle",
  "Both_Positive"  = "crosshatch"
)

master_pattern_angle <- c(
  "RB_Treated"     = 45,
  "R110_Treated"   = -45,
  "RB_Pos_Only"    = 0,
  "R110_Pos_Only"  = 45,
  "Both_Positive"  = 45
)

master_colors <- c(
  "RB_Pos_Only"   = "orange",
  "R110_Pos_Only" = "turquoise",
  "Both_Positive" = "purple",
  "Negative"      = "grey70",
  "RB_Treated"    = "red",
  "R110_Treated"  = "green",
  "Both_Treated"  = "pink"
)

# CONTROL WHICH GROUPS TO SHOW (comment/uncomment) 
active_markers <- c(
  # "Negative",
  "RB_Treated",
  "R110_Treated",
  "RB_Pos_Only",
  "R110_Pos_Only",
  "Both_Positive",
  "Both_Treated"
)

# Subset palettes for active markers
colors <- master_colors[names(master_colors) %in% c(active_markers, "Negative")]
patterns <- master_patterns[names(master_patterns) %in% active_markers]
pattern_angles <- master_pattern_angle[names(master_pattern_angle) %in% active_markers]

# Add pattern_angle column to plot_df (NA where not defined) 
plot_df <- plot_df %>%
  mutate(pattern_angle = pattern_angles[as.character(Marker)])

# Build wide counts table (safe zeros if a marker is absent)
# Ensure Cohort is treated consistently
plot_wide <- plot_df %>%
  group_by(Cohort, Marker) %>%
  summarize(Count = sum(Count, na.rm = TRUE),
            ymin = sum(ifelse(!is.na(ymin), ymin, 0), na.rm = TRUE),
            ymax = sum(ifelse(!is.na(ymax), ymax, 0), na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = Marker, values_from = c(Count, ymin, ymax), values_fill = 0)

# Helper to safely access wide columns (returns 0 if not present)
safe_col <- function(df, prefix, marker) {
  nm <- paste0(prefix, "_", marker)
  if (nm %in% colnames(df)) df[[nm]] else rep(0, nrow(df))
}
# Compute error dfs for each treated marker using explicit sums from wide table 
build_error_df <- function(marker, expr) {
  if (marker %in% active_markers && paste0("Count_", marker) %in% colnames(plot_wide)) {
    eval(expr)
  } else {
    tibble(Cohort = character(), ymin = numeric(), ymid = numeric(), ymax = numeric(), Color = character())
  }
}

# RB_Pos_Only error bar
error_df_RB <- build_error_df("RB_Pos_Only", quote({
  plot_wide %>%
    transmute(
      Cohort,
      ymin = safe_col(plot_wide, "ymin", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "ymin", "RB_Pos_Only"),
      ymid = safe_col(plot_wide, "Count", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "Count", "RB_Pos_Only"),
      ymax = safe_col(plot_wide, "ymax", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "ymax", "RB_Pos_Only"),
      Color = "RB_Treated"
    )
}))

# Both_Positive error bar
error_df_Both <- build_error_df("Both_Positive", quote({
  plot_wide %>%
    transmute(
      Cohort,
      ymin = safe_col(plot_wide, "Count", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "Count", "RB_Pos_Only") +
        safe_col(plot_wide, "ymin", "Both_Positive"),
      ymid = safe_col(plot_wide, "Count", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "Count", "RB_Pos_Only") +
        safe_col(plot_wide, "Count", "Both_Positive"),
      ymax = safe_col(plot_wide, "Count", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "Count", "RB_Pos_Only") +
        safe_col(plot_wide, "ymax", "Both_Positive"),
      Color = "Both_Treated"
    )
}))

# R110_Treated error bar
error_df_R110 <- build_error_df("R110_Pos_Only", quote({
  plot_wide %>%
    transmute(
      Cohort,
      ymin = safe_col(plot_wide, "Count", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "Count", "RB_Pos_Only") +
        safe_col(plot_wide, "Count", "Both_Positive") +
        safe_col(plot_wide, "ymin", "R110_Pos_Only"),
      ymid = safe_col(plot_wide, "Count", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "Count", "RB_Pos_Only") +
        safe_col(plot_wide, "Count", "Both_Positive") +
        safe_col(plot_wide, "Count", "R110_Pos_Only"),
      ymax = safe_col(plot_wide, "Count", "RB_Treated") +
        safe_col(plot_wide, "Count", "R110_Treated") +
        safe_col(plot_wide, "Count", "RB_Pos_Only") +
        safe_col(plot_wide, "Count", "Both_Positive") +
        safe_col(plot_wide, "ymax", "R110_Pos_Only"),
      Color = "R110_Treated"
    )
}))

# Combine only the error_dfs that actually exist and correspond to active markers
error_df <- bind_rows(error_df_RB, error_df_Both, error_df_R110) %>%
  filter(Color %in% active_markers) %>%
  mutate(
    offset = case_when(
      Color == "RB_Treated"   ~ -0.15,
      Color == "Both_Treated" ~ 0,
      Color == "R110_Treated" ~ 0.15,
      TRUE ~ 0
    ),
    plot_color = case_when(
      Color == "RB_Treated"   ~ master_colors["RB_Treated"],
      Color == "R110_Treated" ~ master_colors["R110_Treated"],
      Color == "Both_Treated" ~ master_colors["Both_Treated"],
      TRUE ~ "black"
    )
  )


# Posterior totals
total_colony_draws <- apply(adjusted_colony_draws, c(1,2), sum)  # sims x cohorts
total_colony_draws <- sweep(total_colony_draws, 2, hair_summary_adjusted$Total_RB_Treated + hair_summary_adjusted$Total_R110_Treated, "+")

total_bats <- data.frame(
  Cohort = hair_summary_adjusted$Cohort,
  Total = apply(total_colony_draws, 2, median)
)

cohort_est_ci <- data.frame(
  Cohort = hair_summary_adjusted$Cohort,
  ymin = apply(total_colony_draws, 2, quantile, probs = 0.025),
  ymax = apply(total_colony_draws, 2, quantile, probs = 0.975)
)

print(total_bats)
print(cohort_est_ci)

# Use filtered plot_df for active markers
plot_df_active <- plot_df %>% filter(Marker %in% active_markers)

p2 <- ggplot() +
  # dashed outline total
  geom_bar(
    data = total_bats,
    aes(x = as.numeric(factor(Cohort)), y = Total),
    stat = "identity", fill = NA, color = "black",
    linetype = "dashed", linewidth = 1, width = 0.7
  ) +
  geom_errorbar(
    data = cohort_est_ci,
    aes(x = as.numeric(factor(Cohort)), ymin = ymin, ymax = ymax),
    color = "black", width = 0.1, linewidth = 1.2
  ) +
  # stacked patterned bars
  geom_bar_pattern(
    data = plot_df_active,
    aes(x = as.numeric(factor(Cohort)), y = Count, fill = Marker, pattern = Marker),
    stat = "identity", color = "black", width = 0.7,
    pattern_fill = "black", pattern_density = 0.1, pattern_spacing = 0.05,
    pattern_angle = plot_df_active$pattern_angle
  ) +
  # stacked counts errorbars 
  geom_errorbar(
    data = error_df,
    aes(x = as.numeric(factor(Cohort)) + offset, ymin = ymin, ymax = ymax),
    color = error_df$plot_color, width = 0.1, linewidth = 1.5, inherit.aes = FALSE
  ) +
  geom_segment(
    data = error_df,
    aes(
      x = as.numeric(factor(Cohort)) + offset - 0.05,
      xend = as.numeric(factor(Cohort)) + offset + 0.05,
      y = ymid, yend = ymid
    ),
    color = error_df$plot_color, linewidth = 1.5, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = master_colors[names(master_colors) %in% c(active_markers, "Negative")],
    labels = c(
      "RB_Treated"    = "RB Treated",
      "R110_Treated"  = "R110 Treated",
      "RB_Pos_Only"   = "RB Transfer",
      "R110_Pos_Only" = "R110 Transfer",
      "Both_Positive" = "Both Biomarkers",
      "Negative"      = "Negative"
    ),
    name = ""
  ) +
  scale_pattern_manual(values = patterns, guide = "none") +
  guides(
    fill = guide_legend(override.aes = list(
      pattern = pattern_for_legend <- patterns,
      pattern_angle = pattern_angles
    ))
  ) +
  scale_x_continuous(
    breaks = 1:length(unique(plot_df$Cohort)),
    labels = unique(plot_df$Cohort)
  ) +
  labs(y = "Predicted Number of Bats", x = "Cohort\n") +
  theme_minimal() +
  theme(
    text = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x  = element_text(size = 14, face = "bold"),
    axis.text.y  = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 14, face = "bold"),
    legend.key.size = unit(1.5, "lines"),
    legend.position = "none", #c(0.75, 0.8),
    legend.justification = c(0.5, 0.5)
  )

################################################################################
##### Transfer rate calculations ###############################################
################################################################################

cohort_transfer_sims <- function(donor_cohort, recipient_cohort, n_sims = 10000) {
  recipient_cohort <- as.character(recipient_cohort)
  
  # Identify applied biomarker and total treated for donor cohort
  donor_row <- hair_summary_adjusted %>%
    filter(Cohort == donor_cohort)
  
  applied_marker <- ifelse(donor_row$Total_RB_Treated > 0, "RB_Treated",
                           ifelse(donor_row$Total_R110_Treated > 0, "R110_Treated", NA_character_))
  treated_total <- donor_row$Total_RB_Treated + donor_row$Total_R110_Treated
  
  if (treated_total == 0 || is.na(applied_marker)) return(rep(NA, n_sims))
  
  marker_only <- if (applied_marker == "RB_Treated") "RB_Pos_Only" else "R110_Pos_Only"
  markers_to_use <- c(marker_only, "Both_Positive")
  
  # Extract the cohort indices
  cohort_idx <- match(recipient_cohort, hair_summary_adjusted$Cohort)
  
  # For each simulation, sum counts across recipients for relevant markers
  sim_total_counts <- numeric(n_sims)
  for (i in seq_len(n_sims)) {
    sim_total_counts[i] <- sum(adjusted_colony_draws[i, cohort_idx, markers_to_use])
  }
  
  # Calculate transfer rates
  transfer_rate_sim <- sim_total_counts / treated_total
  return(transfer_rate_sim)
}

# Summary statistics of simulations
cohort_transfer_summary <- function(donor_cohort, recipient_cohort, n_sims = 10000) {
  sims <- cohort_transfer_sims(donor_cohort, recipient_cohort, n_sims)
  
  data.frame(
    TreatedCohort = donor_cohort,
    ReceiverCohorts = paste(recipient_cohort, collapse = ","),
    transfer_rate = median(sims, na.rm = TRUE),
    lower_CI = quantile(sims, 0.025, na.rm = TRUE),
    upper_CI = quantile(sims, 0.975, na.rm = TRUE)
  )
}

# Transfer rates to estimate
cohort_transfer_summary("AF", "AF")
cohort_transfer_summary("AF", "AM")
cohort_transfer_summary("AM", "AF")
cohort_transfer_summary("AM", "AM")

cohort_transfer_summary("AF", c("AF", "AM", "JF", "JM"))
cohort_transfer_summary("AM", c("AF", "AM", "JF", "JM"))
  
################################################################################
######## Colony-wide uptake / Cohort Uptake in all bats ########################
################################################################################

# Sum transfer-positive posterior draws per cohort 
uptake_markers <- c("RB_Pos_Only", "R110_Pos_Only", "Both_Positive")
sim_uptake <- apply(adjusted_colony_draws[,,uptake_markers], c(1,2), sum)  # sims x cohorts

# Add treated bats (fixed counts)
treated_matrix <- matrix(
  hair_summary_adjusted$Total_RB_Treated + hair_summary_adjusted$Total_R110_Treated,
  nrow = n_sims, ncol = nrow(hair_summary_adjusted), byrow = TRUE
)
sim_total <- sim_uptake + treated_matrix

# Calculate colony-wide uptake proportion
colony_sim <- rowSums(sim_total)
prop_colony <- colony_sim / sim_total_colony
prop_colony_est <- median(prop_colony)
prop_colony_ci  <- quantile(prop_colony, probs = c(0.025, 0.975))

cat("Colony uptake proportion (including treated bats, 95% CI):",
    round(prop_colony_est, 3),
    "[", round(prop_colony_ci[1], 3),
    "-", round(prop_colony_ci[2], 3), "]\n")

# Uptake summaries by cohort 
# untreated_colony_total: cohorts x simulations
# total cohort size including treated bats
total_cohort_with_treated <- t(untreated_colony_total) + 
  matrix(rep(hair_summary_adjusted$Total_RB_Treated + 
               hair_summary_adjusted$Total_R110_Treated,
             each = n_sims),
         nrow = n_sims)

# Proportion of uptake including treated bats
prop_per_sim <- sim_total / total_cohort_with_treated

cohort_summary <- data.frame(
  Cohort = hair_summary_adjusted$Cohort,
  median = apply(sim_total, 2, median),
  lower  = apply(sim_total, 2, quantile, 0.025),
  upper  = apply(sim_total, 2, quantile, 0.975),
  prop_median = apply(prop_per_sim, 2, median),
  prop_lower  = apply(prop_per_sim, 2, quantile, 0.025),
  prop_upper  = apply(prop_per_sim, 2, quantile, 0.975)
)

print(cohort_summary %>% select(Cohort, prop_median, prop_lower, prop_upper))

################################################################################
######## Cohort Uptake in non-Treated bats #####################################
################################################################################

# Sum transfer-positive posterior draws per cohort 
uptake_markers <- c("RB_Pos_Only", "R110_Pos_Only", "Both_Positive")
sim_uptake <- apply(adjusted_colony_draws[,,uptake_markers], c(1,2), sum) 

# Calculate uptake proportion in non-treated bats by cohort (matrix)
prop_uptake_per_sim <- sim_uptake / t(untreated_colony_total)  

# Calculate summary statistics
cohort_summary_non_treated <- data.frame(
  Cohort = hair_summary_adjusted$Cohort,
  median = apply(sim_uptake, 2, median),
  lower  = apply(sim_uptake, 2, quantile, 0.025),
  upper  = apply(sim_uptake, 2, quantile, 0.975),
  prop_median = apply(prop_uptake_per_sim, 2, median),
  prop_lower  = apply(prop_uptake_per_sim, 2, quantile, 0.025),
  prop_upper  = apply(prop_uptake_per_sim, 2, quantile, 0.975)
)

print(cohort_summary_non_treated %>% select(Cohort, prop_median, prop_lower, prop_upper))

################################################################################
######## Collected hair samples by percentage ##################################
################################################################################

# Remove Not_Collected
hair_summary_clean <- hair_summary %>%
  select(-Not_Collected)

# Long format and percentages
hair_pct <- hair_summary_clean %>%
  pivot_longer(
    cols = -c(Cohort, TotalUniqueBats),
    names_to = "Category",
    values_to = "Count"
  ) %>%
  mutate(Percent = (Count / TotalUniqueBats) * 100)

# Stacking order
stack_order <- c(
  "Negative",
  "R110_Pos_Only",
  "Both_Positive",
  "RB_Pos_Only",
  "RB_Treated",
  "R110_Treated"
)
hair_pct$Category <- factor(hair_pct$Category, levels = stack_order)

# Color & pattern palettes
colors <- c(
  "RB_Treated"     = "red",
  "R110_Treated"   = "green",
  "RB_Pos_Only"    = "orange",
  "R110_Pos_Only"  = "turquoise",
  "Both_Positive"  = "purple",
  "Negative"       = "gray80"
)

patterns <- c(
  "Negative"       = "none",
  "RB_Pos_Only"    = "circle",
  "Both_Positive"  = "crosshatch",
  "R110_Pos_Only"  = "circle",
  "RB_Treated"     = "stripe",
  "R110_Treated"   = "stripe"

)

pattern_angles <- c(
  "Negative"       = 0,
  "RB_Pos_Only"    = 0,
  "Both_Positive"  = 45,
  "R110_Pos_Only"  = 45,
  "RB_Treated"     = 45,
  "R110_Treated"   = -45
)

# Cohort labels
cohort_labels <- setNames(
  paste0(hair_pct$Cohort, "\n(n = ", hair_pct$TotalUniqueBats, ")"),
  hair_pct$Cohort
)

# Plot with merged legend
p1 <- ggplot(hair_pct, aes(x = Cohort, y = Percent, fill = Category)) +
  geom_bar_pattern(
    aes(pattern = Category, pattern_angle = Category),
    stat = "identity",
    color = "black",
    pattern_fill = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.05
  ) +
  scale_fill_manual(
    values = colors,
    labels = c(
      "RB_Treated"    = "RB Treated",
      "R110_Treated"  = "R110 Treated",
      "RB_Pos_Only"   = "RB Transfer",
      "Both_Positive" = "Both Biomarkers",
      "R110_Pos_Only" = "R110 Transfer",
      "Negative"      = "Negative"
    ),
    name = ""
  ) +
  scale_pattern_manual(values = patterns, guide = "none") +
  scale_pattern_angle_manual(values = pattern_angles, guide = "none") +
  labs(x = "Cohort", y = "Percentage of Hair Samples", fill = "") +
  scale_x_discrete(labels = cohort_labels) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x  = element_text(size = 14, face = "bold"),
    axis.text.y  = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 14, face = "bold")
  ) 

################################################################################
##### Transfer Schematic #######################################################
################################################################################

donors <- c("AF", "AM")
recipients <- c("AF", "AM")

df_summary <- bind_rows(
  lapply(donors, function(d) {
    bind_rows(lapply(recipients, function(r) {
      cohort_transfer_summary(d, r)
    }))
  })
) %>%
  ungroup() %>%
  mutate(
    TreatedCohort = factor(TreatedCohort, levels = c("AF","AM")),
    ReceiverCohorts = factor(ReceiverCohorts, levels = c("AF","AM"))
  )

# Define 8 nodes (start/end for arrows)
nodes <- data.frame(
  node = c("AFAF_start", "AFAF_end",
           "AFAM_start", "AFAM_end",
           "AMAF_start", "AMAF_end",
           "AMAM_start", "AMAM_end"),
  x = c(-0.75, -0.25, -0.5, 0.3, 0.5, -0.25, 0.5, 1),
  y = c(0.6, 0.6, 0.25, -0.4, -0.35, 0.25, -0.6, -0.6)
)

# Map start/end coords and midpoints
df_plot <- df_summary %>%
  mutate(
    x_start = case_when(
      TreatedCohort=="AF" & ReceiverCohorts=="AF" ~ nodes$x[nodes$node=="AFAF_start"],
      TreatedCohort=="AF" & ReceiverCohorts=="AM" ~ nodes$x[nodes$node=="AFAM_start"],
      TreatedCohort=="AM" & ReceiverCohorts=="AF" ~ nodes$x[nodes$node=="AMAF_start"],
      TreatedCohort=="AM" & ReceiverCohorts=="AM" ~ nodes$x[nodes$node=="AMAM_start"]
    ),
    y_start = case_when(
      TreatedCohort=="AF" & ReceiverCohorts=="AF" ~ nodes$y[nodes$node=="AFAF_start"],
      TreatedCohort=="AF" & ReceiverCohorts=="AM" ~ nodes$y[nodes$node=="AFAM_start"],
      TreatedCohort=="AM" & ReceiverCohorts=="AF" ~ nodes$y[nodes$node=="AMAF_start"],
      TreatedCohort=="AM" & ReceiverCohorts=="AM" ~ nodes$y[nodes$node=="AMAM_start"]
    ),
    x_end = case_when(
      TreatedCohort=="AF" & ReceiverCohorts=="AF" ~ nodes$x[nodes$node=="AFAF_end"],
      TreatedCohort=="AF" & ReceiverCohorts=="AM" ~ nodes$x[nodes$node=="AFAM_end"],
      TreatedCohort=="AM" & ReceiverCohorts=="AF" ~ nodes$x[nodes$node=="AMAF_end"],
      TreatedCohort=="AM" & ReceiverCohorts=="AM" ~ nodes$x[nodes$node=="AMAM_end"]
    ),
    y_end = case_when(
      TreatedCohort=="AF" & ReceiverCohorts=="AF" ~ nodes$y[nodes$node=="AFAF_end"],
      TreatedCohort=="AF" & ReceiverCohorts=="AM" ~ nodes$y[nodes$node=="AFAM_end"],
      TreatedCohort=="AM" & ReceiverCohorts=="AF" ~ nodes$y[nodes$node=="AMAF_end"],
      TreatedCohort=="AM" & ReceiverCohorts=="AM" ~ nodes$y[nodes$node=="AMAM_end"]
    ),
    x_mid = (x_start + x_end)/2,
    y_mid = case_when(
      TreatedCohort=="AF" & ReceiverCohorts=="AF" ~ 1.0,
      TreatedCohort=="AF" & ReceiverCohorts=="AM" ~ 0.0,
      TreatedCohort=="AM" & ReceiverCohorts=="AF" ~ 0.0,
      TreatedCohort=="AM" & ReceiverCohorts=="AM" ~ -1.0
    ),
    curved = (TreatedCohort=="AF" & ReceiverCohorts=="AF") |
      (TreatedCohort=="AM" & ReceiverCohorts=="AM"),
    straight = !curved
  )

# Reshape for curved arrows (geom_bezier)
df_bezier <- df_plot %>%
  filter(curved) %>%
  rowwise() %>%
  mutate(
    x_points = list(c(x_start, x_mid, x_end)),
    y_points = list(c(y_start, y_mid, y_end))
  ) %>%
  unnest(c(x_points, y_points)) %>%
  group_by(TreatedCohort, ReceiverCohorts) %>%
  mutate(group_id = cur_group_id()) %>%
  ungroup()

# Create a df for manual adjustment of transfer rate labels
df_labels <- df_plot %>%
  select(TreatedCohort, ReceiverCohorts, transfer_rate, lower_CI, upper_CI, x_mid, y_mid) %>%
  rename(x_label = x_mid, y_label = y_mid) %>%
  # Manually adjust coordinates here
  mutate(
    x_label = case_when(
      TreatedCohort=="AF" & ReceiverCohorts=="AF" ~ x_label + 0.0,
      TreatedCohort=="AF" & ReceiverCohorts=="AM" ~ x_label - 0.25,
      TreatedCohort=="AM" & ReceiverCohorts=="AF" ~ x_label + 0.3,
      TreatedCohort=="AM" & ReceiverCohorts=="AM" ~ x_label + 0.0
    ),
    y_label = case_when(
      TreatedCohort=="AF" & ReceiverCohorts=="AF" ~ y_label + 0.0,
      TreatedCohort=="AF" & ReceiverCohorts=="AM" ~ y_label - 0.15,
      TreatedCohort=="AM" & ReceiverCohorts=="AF" ~ y_label + 0.1,
      TreatedCohort=="AM" & ReceiverCohorts=="AM" ~ y_label - 0.0
    )
  )

# Define transfer type colors
transfer_colors <- c(
  "RB Transfer"   = "orange",
  "R110 Transfer" = "turquoise"
)

# Create a lookup table of cohort → marker applied
marker_lookup <- hair_summary_adjusted %>%
  select(Cohort, RB_Treated, R110_Treated) %>%
  pivot_longer(
    cols = c(RB_Treated, R110_Treated),
    names_to = "Marker",
    values_to = "Count"
  ) %>%
  filter(Count > 0) %>%            # keep only markers actually applied
  group_by(Cohort) %>%
  slice(1) %>%                     # if multiple, take the first
  ungroup() %>%
  mutate(Marker = case_when(
    Marker == "RB_Treated" ~ "RB",
    Marker == "R110_Treated" ~ "R110"
  ))

# Now assign TransferType to df_bezier and df_plot based on this lookup
df_bezier <- df_bezier %>%
  left_join(marker_lookup, by = c("TreatedCohort" = "Cohort")) %>%
  mutate(TransferType = paste(Marker, "Transfer"))

df_plot <- df_plot %>%
  left_join(marker_lookup, by = c("TreatedCohort" = "Cohort")) %>%
  mutate(TransferType = paste(Marker, "Transfer"))

# Plot 
p3 <- ggplot() +
  # Curved arrows
  geom_bezier(
    data = df_bezier,
    aes(
      x = x_points, y = y_points, group = group_id,
      linewidth = transfer_rate, color = TransferType
    ),
    arrow = arrow(length = unit(0.25, "inches"), type = "closed"),
    show.legend = c(color = TRUE, linewidth = FALSE)
  ) +
  # Straight arrows
  geom_curve(
    data = df_plot %>% filter(straight),
    aes(
      x = x_start, y = y_start, xend = x_end, yend = y_end,
      linewidth = transfer_rate, color = TransferType
    ),
    curvature = 0,
    arrow = arrow(length = unit(0.25, "inches"), type = "closed"),
    show.legend = c(color = TRUE, linewidth = FALSE)
  ) +
  # Numeric labels with bold transfer_rate and plain CI
  geom_richtext(
    data = df_labels,
    aes(
      x = x_label, y = y_label,
      label = paste0("<b>", round(transfer_rate,2), "</b><br>(",
                     round(lower_CI,2), "-", round(upper_CI,2), ")")
      ),
    size = 5,
    fill = NA,     # No background box
    label.color = NA
  ) +
  # Cohort labels
  geom_text(
    data = data.frame(label=c("Adult Females","Adult Males"), x=c(-0.5,0.75), y=c(0.5,-0.5)),
    aes(x=x, y=y, label=label),
    size=6, fontface="bold"
  ) +
  scale_linewidth(range=c(0.5,3)) +
  scale_color_manual(values = transfer_colors) +
  coord_cartesian(xlim=c(-0.8,1), ylim=c(-1.1,1.1)) +
  theme_void() +
  theme(
    legend.position = c(0.75, 0.75),   # set legend coordinates
    legend.title = element_blank(),
    legend.text  = element_text(size=12, face="bold")
  )

##### Jirosto RFID plot (Figure 5H) ############################################

#Filter out nights with incomplete RFID data
night_summary <- night_summary |> 
  dplyr::filter(Night != as.Date(c("2025-06-19", "2025-06-24")))

fig5H <- ggplot(night_summary, aes(x = Night, y = UniqueTagCount)) +
  geom_line(color = "#1D91C0", size = 1) +
  geom_point(color = "#0C2C84", size = 2) +
  # Shaded regions
  geom_rect(aes(xmin = as.Date("2025-06-13") + 0.2, xmax = as.Date("2025-06-14") - 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.05, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2025-06-18") + 0.2, xmax = as.Date("2025-06-19") - 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.05, inherit.aes = FALSE) +
  
  # Optional labels for shaded areas
  annotate("text", x = as.Date("2025-06-13") - 0.8, y = 80,
           label = "Capture 1", hjust = 0, size = 5.5, fontface = "bold") +
  annotate("text", x = as.Date("2025-06-18") - 0.8, y = 80,
           label = "Capture 2", hjust = 0, size = 5.5, fontface = "bold") +
  
  labs(
    title = " ",
    x = "Night (June 2025)",
    y = "Unique Bats Detected"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 day", date_labels = "%d") +
  theme_minimal(base_size = 16) +
  theme(
    # Axes lines only on bottom and left
    axis.line = element_line(color = "black", size = 1),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    
    # Gridlines
    panel.grid.major.x = element_blank(),   # remove x-axis gridlines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90"),
    
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 16, hjust = 1, angle = 45),
    axis.text.y = element_text(face = "bold", size = 16)
  )

################################################################################
#### Plot of captures by cohort/date (Figure 5F) ###############################
################################################################################

# Identify the first two capture dates automatically
dates <- sort(unique(bat_data$capture_date))

# Count unique bats per date by Cohort
bat_summary <- bat_data %>%
  distinct(capture_date, bat_id, Cohort) %>%
  group_by(capture_date, Cohort) %>%
  summarise(Count = n(), .groups = "drop")

# Recaptures on first date: if Previous RFID == 1
recap_1 <- bat_data %>%
  filter(capture_date == dates[1], previous_rfid == 1)

# Recaptures on second date: if seen on first date
known_bats <- bat_data %>%
  filter(capture_date == dates[1]) %>%
  pull(bat_id) %>%
  unique()

recap_2 <- bat_data %>%
  filter(capture_date == dates[2], bat_id %in% known_bats)

# Recaptures on second date: if seen on first date
known_bats <- bat_data %>%
  filter(capture_date == dates[2]) %>%
  pull(bat_id) %>%
  unique()

# Combine recaptures
recap_all <- bind_rows(recap_1, recap_2)

# Summarize recaptures
recap_summary <- recap_all %>%
  distinct(capture_date, bat_id, Cohort) %>%
  group_by(capture_date, Cohort) %>%
  summarise(Recaptured = n(), .groups = "drop")

# Merge with full summary
plot_df <- left_join(bat_summary, recap_summary, by = c("capture_date", "Cohort")) %>%
  mutate(Recaptured = replace_na(Recaptured, 0))


plot_df <- plot_df %>%
  mutate(RecapFlag = ifelse(!is.na(Recaptured), "Recaptured", NA))

# Plot
fig5F <- ggplot(plot_df, aes(x = factor(capture_date), y = Count, fill = Cohort)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_point(
    aes(y = Recaptured, group = Cohort, shape = RecapFlag),
    position = position_dodge(width = 0.8),
    size = 10,
    stroke = 10,
    color = "black"
  ) +
  
  scale_fill_manual(values = c(
    "AM" = "#33A02C",
    "AF" = "#1F78B4",
    "JM" = "#6A3D9A",
    "JF" = "#E31A1C"
  )) +
  scale_shape_manual(values = c("Recaptured" = 95)) +
  
  labs(
    x = "Capture Date (2025)",
    y = "Number of Bats",
    fill = "Cohort",
    shape = ""
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(shape = NA, size = 5),  
      keywidth = 0.8,
      keyheight = 0.6
    ),
    shape = guide_legend(
      override.aes = list(fill = NA, size = 8)    # make recaptured underscore visible
    )
  ) +
  
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) + #105 - Jirosto; 25 - Ahuacapan 
  scale_x_discrete(labels = function(x) format(as.Date(x), "%b %d")) +
  theme_minimal(base_size = 18) +
  theme(
    # Axes lines only on bottom and left
    axis.line = element_line(color = "black", size = 1),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    
    # Gridlines
    panel.grid.major.x = element_blank(),   # remove x-axis gridlines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90"),
    
    legend.position = "left", #c(0.65, 0.8), # (0.65, 0.80) - Ahuacapan (0.85, 0.80) - Jirosto
    legend.key.size = unit(0.6, "cm"),  # global shrink
    legend.spacing.y = unit(0, "lines"),
    text = element_text(size = 20, face = "bold")
  )

################################################################################
#### Figure 6A-C Jirosto row ###################################################
################################################################################

# Spacer grob (fixed-width blank column)
spacer <- ggdraw() + theme_void()

# Step 1: combine p1 and p2 horizontally with aligned x-axes
row12 <- plot_grid(
  p1, p2,
  nrow = 1,
  align = "h",      # horizontal alignment
  axis = "b",       # align bottom axes
  rel_widths = c(1, 1)
)

# Step 2: append p3 to the right of the combined row
figure6j <- plot_grid(
  row12, p3,
  nrow = 1,
  rel_widths = c(2, 1)  # adjust widths as needed
)

figure6j