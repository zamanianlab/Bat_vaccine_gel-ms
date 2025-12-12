# Libraries
lapply(
  c("dplyr", "tidyr", "scales", "patchwork", "readr", "brms",
    "ggplot2","reshape2", "truncnorm", "lubridate",
    "ggforce", "cowplot"),
  library,
  character.only = TRUE
)

# Load data
bat_data <- read.csv("C:Casimiro_capture_data.csv", stringsAsFactors = FALSE)
rfid_folder <- "RFID/Casimiro/"

################################################################################

bat_data <- bat_data[-1, ]

# Convert date
bat_data$Capture.Date <- as.Date(bat_data$Capture.Date, format = "%m/%d/%Y")

# Clean columns
bat_data <- bat_data %>%
  rename(
    CaptureDate = Capture.Date,
    BatID = Unique.bat,
    HairSample = Hair.Sample,
    TreatedType = Gel.Applied,
    PreviousRFID = Previous.RFID
  ) %>%
  filter(!is.na(as.Date(CaptureDate, format = "%m/%d/%Y"))) %>%
  filter(!is.na(Age) & Age != "")

# Create AgeGroup and Cohort
bat_data <- bat_data %>%
  mutate(
    AgeGroup = ifelse(Age %in% c("SA", "J"), "J", "A"),
    AgeGroup = factor(AgeGroup, levels = c("A", "J")),
    Sex = factor(Sex, levels = c("M", "F")),
    Cohort = case_when(
      AgeGroup == "A" & Sex == "M" ~ "Adult Male",
      AgeGroup == "A" & Sex == "F" ~ "Adult Female",
      AgeGroup == "J" & Sex == "M" ~ "Juvenile Male",
      AgeGroup == "J" & Sex == "F" ~ "Juvenile Female",
      TRUE ~ NA_character_
    )
  )

# Identify the first two capture dates automatically
dates <- sort(unique(bat_data$CaptureDate))
first_date <- dates[1]
second_date <- dates[2]
third_date <- dates[3]

# Count unique bats per date by Cohort
bat_summary <- bat_data %>%
  distinct(CaptureDate, BatID, Cohort) %>%
  group_by(CaptureDate, Cohort) %>%
  summarise(Count = n(), .groups = "drop")

# Sum of bats captured per capture date
total_captures <- bat_summary %>%
  group_by(CaptureDate) %>%
  summarise(TotalBats = sum(Count), .groups = "drop") %>%
  arrange(CaptureDate)

# Print result
c <- c <- total_captures[[1, "TotalBats"]] #C for N = MC/R

################################################################################

total_recaptures <- bat_data %>%
  filter(CaptureDate == first_date, PreviousRFID == 1) %>%
  summarise(Count = n()) %>%
  pull(Count)

r <- total_recaptures #M for N = MC/R

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

# Combine all data frames into one "mega file"
mega_df <- do.call(rbind, all_dfs)

# View or save mega_df if needed
head(mega_df)

# Count total number of unique IDs
total_unique_ids <- length(unique(mega_df$TagID))
print(paste("Total unique IDs detected:", total_unique_ids))

#### Count Detections per Night ################################################

# Ensure DateTime is parsed correctly
mega_df <- mega_df %>%
  mutate(
    # Combine Date and Time, parse with explicit timezone
    DateTime = as.POSIXct(paste(Date, Time),
                          format = "%m/%d/%Y %H:%M:%OS",
                          tz = "America/Mexico_City"),
    Hour = hour(DateTime)
  )

# Assign Night based on rules
mega_df <- mega_df %>%
  mutate(
    Night = case_when(
      Hour < 10 ~ as.Date(DateTime) - days(1),  # before 10am → previous night
      Hour >= 17.5 ~ as.Date(DateTime) - days(1),           # after 5pm → same night
      TRUE ~ NA_Date_                           # 10am–4:59pm → ignore
    )
  )

# Filter night detections and remove test/system tags
night_df <- mega_df %>%
  filter(!is.na(Night)) %>%
  filter(!TagID %in% c("999.000000007425", "989.001040476348"))

# Count unique bats per night
night_summary <- night_df %>%
  group_by(Night) %>%
  summarise(UniqueTagCount = n_distinct(TagID), .groups = "drop") %>%
  arrange(Night)

print(night_summary)

# Filter night detections and remove test/system tags
night_df <- mega_df %>%
  filter(!is.na(Night)) %>%
  filter(!TagID %in% c("999.000000007425", "989.001040476348"))

# Count unique bats per night
night_summary <- night_df %>%
  group_by(Night) %>%
  summarise(UniqueTagCount = n_distinct(TagID), .groups = "drop") %>%
  arrange(Night)

print(night_summary)

################################################################################

# Number of unique tags the night before first capture
# Value of M for mark-recapture calculation
m <- night_summary %>%
  filter(Night == (first_date - 1)) %>%
  pull(UniqueTagCount)

print(first_date - 1)
print(m)

# Calculate colony population estimate
pop_est <- m*c/r

# Calculate variance and standard error of population estimate
var_pop_est <- (m+1)*(c+1)*(m-r)*(c-r)/((r+1)^2*(r+2))
se_pop_est <- sqrt(var_pop_est)

### Mortalities at Jirosto
pop_est
se_pop_est

################################################################################
#### Demographic totals ########################################################
################################################################################

# Ensure Age is "J" for "SA"
bat_data <- bat_data %>%
  mutate(
    AgeGroup = ifelse(Age %in% c("SA", "J"), "J", "A"),  # already done, repeat if needed
    Cohort = paste0(AgeGroup, Sex)                       # e.g., "AM", "JF"
  )

# Count unique bats per cohort
unique_bats <- bat_data %>%
  distinct(BatID, Cohort) %>%  # unique bat IDs per cohort
  count(Cohort, name = "Unique_Bats")

print(unique_bats)
sum(unique_bats$Unique_Bats)

################################################################################
################################################################################

# First capture treatment per bat
treatment_df <- bat_data %>%
  group_by(BatID) %>%
  slice_min(order_by = CaptureDate, n = 1) %>%
  select(BatID, Cohort, TreatedType) %>%
  ungroup() %>%
  mutate(TreatedType = replace_na(TreatedType, 0))

# Summarize treated counts separately for RB and R110
treated_counts <- treatment_df %>%
  group_by(Cohort) %>%
  summarise(
    RB_Treated   = sum(TreatedType == 1),
    .groups = "drop"
  )

print(treated_counts)

# Hair samples from all captures (include 4 = not collected)
hair_df <- bat_data %>%
  filter(HairSample %in% c(0, 0.5, 1, 2, 3, 4)) %>%
  select(BatID, Cohort, HairSample) %>%
  distinct(BatID, Cohort, HairSample)   # ensure each bat only counted once per hair result

# Join treatment info
hair_df <- hair_df %>%
  left_join(treatment_df, by = c("BatID", "Cohort"))

# Summarize per bat (only one row per BatID)
hair_per_bat <- hair_df %>%
  group_by(BatID) %>%
  summarise(
    Cohort = first(Cohort),              # stable cohort per bat
    TreatedType = first(TreatedType),    # assume treatment is fixed per bat
    RB_Pos = any(HairSample == 1),
    Not_Collected = (!any(HairSample %in% c(0, 0.5, 1, 2, 3), na.rm = TRUE)) & (first(TreatedType) == 0),
    .groups = "drop"
  )

# Summarize per cohort
hair_summary <- hair_per_bat %>%
  group_by(Cohort) %>%
  summarise(
    TotalUniqueBats = n(),
    RB_Treated = sum(TreatedType == 1),
    Not_Collected = sum(Not_Collected),
    TotalUntreatedHair = TotalUniqueBats - RB_Treated - Not_Collected,
    RB_Pos_Only = sum(RB_Pos & TreatedType != 1),
    Negative = TotalUniqueBats - RB_Treated - RB_Pos_Only - Not_Collected,
    .groups = "drop"
  )

print(hair_summary)

#### Build model ###############################################################

cohorts <- c("AM", "AF", "JM", "JF")

# Prepare data
df <- hair_summary %>%
  filter(Cohort %in% cohorts) %>%
  mutate(
    positive = RB_Pos_Only,
    total    = RB_Pos_Only + Negative,
    age      = ifelse(substr(Cohort, 1, 1) == "A", "A", "J"),
    sex      = ifelse(substr(Cohort, 2, 2) == "M", "M", "F"),
    cohort   = factor(Cohort, levels = c("JF", "AM", "JM", "AF"))
  ) %>%
  select(cohort, age, sex, positive, total)

# Define priors
prior_list <- c(
  prior(normal(0, 2), class = "Intercept"),  # prior for intercept
  prior(normal(0, 2), class = "b")           # prior for all coefficients (b)
)

# Fit the binomial model
fit1 <- brm(
  positive | trials(total) ~ cohort,
  family = binomial(),
  data = df,
  iter = 8000,
  warmup = 2000,
  prior = prior_list
)

# Hypothesis testing examples
hypothesis(fit1, "cohortAM = 0")
hypothesis(fit1, "cohortAM - cohortAF = 0")
hypothesis(fit1, "cohortAM - cohortJM = 0")
hypothesis(fit1, "cohortJM - cohortAF = 0")
hypothesis(fit1, "cohortJM = 0")
hypothesis(fit1, "cohortAF = 0")

# Add predicted values to df
df$predicted <- predict(fit1, type = "response")[, "Estimate"]

#### Update hair_summary with full non-treated bats ############################

# Known bats per cohort
known_bats <- setNames(unique_bats$Unique_Bats, unique_bats$Cohort)
n_known <- sum(known_bats)

# Join treated_counts to hair_summary and rename RB_Treated to Total_RB_Treated
hair_summary <- hair_summary %>%
  left_join(treated_counts %>% rename(Total_RB_Treated = RB_Treated),
            by = "Cohort") %>%
  mutate(
    untreated_bats = known_bats[Cohort] - Total_RB_Treated
  )

#### Posterior draws of total colony size ######################################

n_draws <- posterior_samples(fit1) %>% nrow()  # number of posterior draws
N_post <- round(rtruncnorm(
  n = n_draws,
  a = n_known,     # left truncation
  b = Inf,         # no upper limit
  mean = pop_est,
  sd = se_pop_est
))
N_post[N_post < sum(known_bats)] <- sum(known_bats)  # enforce lower bound

#### Compute unknown bats per cohort per posterior draw ########################

cohort_probs <- prop.table(known_bats)  # proportions of known bats per cohort

n_unknown_matrix <- matrix(0, nrow = n_draws, ncol = length(cohort_probs))
colnames(n_unknown_matrix) <- names(cohort_probs)

for (i in seq_len(n_draws)) {
  n_unknown_draw <- N_post[i] - n_known
  
  if (n_unknown_draw > 0) {
    n_unknown_matrix[i, ] <- rmultinom(
      n = 1,
      size = n_unknown_draw,
      prob = cohort_probs
    )
  }
}

#### Build theta_draws (posterior predicted probabilities) #####################

# posterior probability of being positive (success) per cohort
theta_draws <- posterior_linpred(fit1, transform = TRUE)  # draws × rows in df

# Map draws to hair_summary cohorts
# df used in brms fit may have a different order; ensure it matches hair_summary
cohort_order <- match(hair_summary$Cohort, df$cohort)
theta_draws_array <- array(NA, dim = c(n_draws, nrow(hair_summary), 2),
                           dimnames = list(NULL, hair_summary$Cohort, c("Positive", "Negative")))

for (i in seq_len(n_draws)) {
  theta_draws_array[i, , "Positive"] <- theta_draws[i, cohort_order]
  theta_draws_array[i, , "Negative"] <- 1 - theta_draws[i, cohort_order]
}

#### Build counts_matrix (observed counts per cohort per category) #############

categories <- c("RB_Treated", "RB_Pos_Only", "Negative")

counts_matrix <- as.matrix(hair_summary[, categories])
rownames(counts_matrix) <- hair_summary$Cohort

#### Matrix of total unknown bats per draw #####################################

# categories that correspond to known hair samples
observed_cats <- c("RB_Pos_Only", "Negative")

# remove those from the unsampled pool
observed_counts <- counts_matrix[, observed_cats, drop = FALSE]

unknown_colony_total <- hair_summary$untreated_bats + t(n_unknown_matrix) - rowSums(observed_counts)

#### Compute adjusted colony draws #############################################

adjusted_colony_draws <- array(
  NA,
  dim = c(n_draws, nrow(hair_summary), length(categories)),
  dimnames = list(NULL, hair_summary$Cohort, categories)
)

# Ensure treated_counts is a proper matrix with the same category columns
treated_matrix <- matrix(0, nrow = nrow(hair_summary), ncol = length(categories))
colnames(treated_matrix) <- categories
rownames(treated_matrix) <- hair_summary$Cohort

# Fill in the treated counts
treated_matrix[, "RB_Treated"] <- treated_counts$RB_Treated

# Convert observed_counts to a full matrix with same structure
observed_matrix <- matrix(0, nrow = nrow(hair_summary), ncol = length(categories))
colnames(observed_matrix) <- categories
rownames(observed_matrix) <- hair_summary$Cohort
observed_matrix[, colnames(observed_counts)] <- as.matrix(observed_counts)

for (i in seq_len(n_draws)) {
  unsampled_bats <- unknown_colony_total[, i]
  
  # predicted Positive/Negative counts for unsampled bats
  predicted_unsampled <- theta_draws_array[i,, "Positive"] * unsampled_bats
  predicted_unsampled_matrix <- matrix(0, nrow = nrow(hair_summary), ncol = length(categories))
  colnames(predicted_unsampled_matrix) <- categories
  rownames(predicted_unsampled_matrix) <- hair_summary$Cohort
  
  # assign predicted Positive counts to 'Negative' category for unsampled proportion
  # distribute other categories as 0 (observed only)
  predicted_unsampled_matrix[, "Negative"] <- unsampled_bats - predicted_unsampled
  predicted_unsampled_matrix[, "RB_Pos_Only"] <- predicted_unsampled  # optional if you want to include Positive
  # the rest (RB_Treated, R110_Treated, Both_Positive, R110_Pos_Only) are observed only
  
  # Final add-back step
  adjusted_colony_draws[i,, ] <- predicted_unsampled_matrix +
    observed_matrix +
    treated_matrix
  
}

#### Summarize posterior draws #################################################

adjusted_colony_median <- apply(adjusted_colony_draws, c(2,3), median)
adjusted_colony_q025 <- apply(adjusted_colony_draws, c(2,3), quantile, probs = 0.025)
adjusted_colony_q975 <- apply(adjusted_colony_draws, c(2,3), quantile, probs = 0.975)

################################################################################
# Total predicted bats per cohort 
total_predicted <- as.data.frame(adjusted_colony_median) %>%
  mutate(Cohort = rownames(adjusted_colony_median)) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(all_of(categories)))) %>%
  ungroup() %>%
  mutate(
    q025 = rowSums(adjusted_colony_q025),
    q975 = rowSums(adjusted_colony_q975)
  )

# Prepare stack_df for plotting 
# Use RB_Treated from treated_counts
rb_treated_df <- treated_counts %>%
  select(Cohort, RB_Treated)

# Get RB_Pos_Only counts and CIs from adjusted_colony draws
rb_pos_df <- data.frame(
  Cohort = rownames(adjusted_colony_median),
  RB_Pos_Only = adjusted_colony_median[, "RB_Pos_Only"],
  ymin = adjusted_colony_q025[, "RB_Pos_Only"],
  ymax = adjusted_colony_q975[, "RB_Pos_Only"]
)

# Combine into one wide data frame
stack_wide <- rb_treated_df %>%
  left_join(rb_pos_df, by = "Cohort") %>%
  mutate(
    # shift the CI + mean by RB_Treated so it stacks correctly
    ymin = ymin + RB_Treated,
    ymax = ymax + RB_Treated,
    RB_Pos_Only = RB_Pos_Only
  )

# Pivot longer for ggplot
stack_df <- stack_wide %>%
  pivot_longer(
    cols = c(RB_Treated, RB_Pos_Only),
    names_to = "Group",
    values_to = "Count"
  ) %>%
  mutate(
    ymin = ifelse(Group == "RB_Pos_Only", ymin, NA),
    ymax = ifelse(Group == "RB_Pos_Only", ymax, NA)
  )

# Plot 
fig4G <- ggplot() +
  # Stacked bars: RB_Treated (black) and RB_Pos_Only (gray)
  geom_bar(data = stack_df,
           aes(x = Cohort, y = Count, fill = Group),
           stat = "identity",
           position = "stack",
           width = 0.7,
           color = "black") +
  
  # Dashed outline + error bars for total predicted bats
  geom_bar(data = total_predicted,
           aes(x = Cohort, y = Total, fill = "Estimate within colony"),
           stat = "identity",
           color = "black",
           linetype = "dashed",
           size = 1,
           width = 0.7,
           show.legend = TRUE) +
  geom_errorbar(data = total_predicted,
                aes(x = Cohort, ymin = q025, ymax = q975),
                width = 0.3,
                color = "black",
                linewidth = 1) +
  
  # Red error bars for RB_Pos_Only
  geom_errorbar(data = subset(stack_df, Group == "RB_Pos_Only"),
                aes(x = Cohort, ymin = ymin, ymax = ymax),
                color = "red",
                linetype = "dashed",
                width = 0.3,
                linewidth = 1) +

  # Fill colors (now includes estimate within colony)
  scale_fill_manual(
    values = c(
      "Estimate within colony" = "transparent",   # transparent fill
      "RB_Pos_Only" = "gray",
      "RB_Treated" = "black"
    ),
    labels = c("Total Bats", "Transfer Uptake", "Topically Treated"),
    name = "",
    guide = guide_legend(override.aes = list(
      fill = c(NA, 
               "gray", # "gray", 
               "black"
               ),   # show transparent fill
      linetype = c("dashed", 
                   "solid", 
                   "solid"
                   ) # dashed for estimate
    ))
  ) +
  
  # Axes and theme
  labs(x = "Cohort", y = "Predicted Number of Bats") +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.9),
    # legend.justification = c(1, 1.1),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14)
  ) +
  ylim(0, max(total_predicted$q975) * 1.05)

################################################################################
######## Colony-wide uptake (RB_Pos_Only) ####################################
################################################################################

# Extract RB_Pos_Only posterior draws (sims × cohorts)
sim_rb_pos <- adjusted_colony_draws[, , "RB_Pos_Only"]

# Add treated bats (fixed counts)
treated_matrix <- matrix(
  hair_summary$Total_RB_Treated, 
  nrow = n_draws, ncol = nrow(hair_summary), byrow = TRUE
)
sim_total <- sim_rb_pos + treated_matrix

# Colony-wide uptake proportion
# N_post is total colony size per posterior draw
prop_colony <- rowSums(sim_total) / N_post
prop_colony_est <- median(prop_colony)
prop_colony_ci  <- quantile(prop_colony, c(0.025, 0.975))

cat("Colony uptake proportion (RB_Pos_Only, including treated bats, 95% CI):",
    round(prop_colony_est, 3),
    "[", round(prop_colony_ci[1], 3),
    "-", round(prop_colony_ci[2], 3), "]\n")

# Uptake summaries by cohort 
# total cohort size including treated bats
total_cohort_with_treated <- t(unknown_colony_total) + treated_matrix +
  matrix(rep(hair_summary$TotalUntreatedHair, each = nrow(sim_rb_pos)),
         nrow = nrow(sim_rb_pos))

# Proportion of uptake including treated bats per cohort
prop_per_sim <- sim_total / total_cohort_with_treated

cohort_summary <- data.frame(
  Cohort      = hair_summary$Cohort,
  median      = apply(sim_total, 2, median),
  lower       = apply(sim_total, 2, quantile, 0.025),
  upper       = apply(sim_total, 2, quantile, 0.975),
  prop_median = apply(prop_per_sim, 2, median),
  prop_lower  = apply(prop_per_sim, 2, quantile, 0.025),
  prop_upper  = apply(prop_per_sim, 2, quantile, 0.975)
)

print(cohort_summary %>% select(Cohort, prop_median, prop_lower, prop_upper))

# Colony-wide transfer rate: total transfer ÷ total treated
transfer_rate <- rowSums(sim_rb_pos) / sum(treated_counts$RB_Treated)

# Summarize
transfer_rate_summary <- data.frame(
  median = median(transfer_rate),
  lower  = quantile(transfer_rate, 0.025),
  upper  = quantile(transfer_rate, 0.975)
)

print(transfer_rate_summary)

################################################################################
######## Cohort Uptake in non-Treated bats (RB_Pos_Only) ######################
################################################################################

total <- t(unknown_colony_total) +
            matrix(rep(hair_summary$TotalUntreatedHair, each = nrow(sim_rb_pos)),
                   nrow = nrow(sim_rb_pos))
  
# Calculate uptake proportion in non-treated bats by cohort
# unknown_colony_total: cohorts × sims → transpose to match dimensions
prop_non_treated_per_sim <- sim_rb_pos / total

# Summary statistics per cohort
cohort_summary_non_treated <- data.frame(
  Cohort      = hair_summary$Cohort,
  median      = apply(sim_rb_pos, 2, median),
  lower       = apply(sim_rb_pos, 2, quantile, 0.025),
  upper       = apply(sim_rb_pos, 2, quantile, 0.975),
  prop_median = apply(prop_non_treated_per_sim, 2, median),
  prop_lower  = apply(prop_non_treated_per_sim, 2, quantile, 0.025),
  prop_upper  = apply(prop_non_treated_per_sim, 2, quantile, 0.975)
)

print(cohort_summary_non_treated %>% select(Cohort, prop_median, prop_lower, prop_upper))

################################################################################
#### Plot of captures be cohort/date (Figure 3E) ###############################
################################################################################

# Identify the first two capture dates automatically
dates <- sort(unique(bat_data$CaptureDate))

# Count unique bats per date by Cohort
bat_summary <- bat_data %>%
  distinct(CaptureDate, BatID, Cohort) %>%
  group_by(CaptureDate, Cohort) %>%
  summarise(Count = n(), .groups = "drop")

# Recaptures on first date: if Previous RFID == 1
recap_1 <- bat_data %>%
  filter(CaptureDate == dates[1], PreviousRFID == 1)

# Recaptures on second date: if seen on first date
known_bats <- bat_data %>%
  filter(CaptureDate == dates[1]) %>%
  pull(BatID) %>%
  unique()

recap_2 <- bat_data %>%
  filter(CaptureDate == dates[2], BatID %in% known_bats)

# Recaptures on second date: if seen on first date
known_bats <- bat_data %>%
  filter(CaptureDate == dates[2]) %>%
  pull(BatID) %>%
  unique()

recap_3 <- bat_data %>%
  filter(CaptureDate == dates[3], BatID %in% known_bats)

# Combine recaptures
recap_all <- bind_rows(recap_1, recap_2, recap_3)

# Summarize recaptures
recap_summary <- recap_all %>%
  distinct(CaptureDate, BatID, Cohort) %>%
  group_by(CaptureDate, Cohort) %>%
  summarise(Recaptured = n(), .groups = "drop")

# Merge with full summary
plot_df <- left_join(bat_summary, recap_summary, by = c("CaptureDate", "Cohort")) %>%
  mutate(Recaptured = replace_na(Recaptured, 0))

plot_df <- plot_df %>%
  mutate(RecapFlag = ifelse(!is.na(Recaptured), "Recaptured", NA))

fig3E <- ggplot(plot_df, aes(x = factor(CaptureDate), y = Count, fill = Cohort)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_point(
    aes(y = Recaptured, group = Cohort, shape = RecapFlag),
    position = position_dodge(width = 0.8),
    size = 8,
    stroke = 2,
    color = "black",
    show.legend = TRUE
  ) +
  
  scale_fill_manual(
    values = c(
      "AM" = "#33A02C",
      "AF" = "#1F78B4",
      "JM" = "#6A3D9A",
      "JF" = "#E31A1C"
    )
  ) +
  
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
      keyheight = 0.6,
      title.theme = element_text(size = 15, face = "bold"),
      label.theme = element_text(size = 13)
    ),
    shape = guide_legend(
      override.aes = list(fill = NA, color = "black", size = 8),
      title.theme = element_text(size = 15, face = "bold"),
      label.theme = element_text(size = 13)
    )
  ) +
  
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +  # start y-axis at 0
  scale_x_discrete(labels = function(x) format(as.Date(x), "%b %d")) +
  
  theme_minimal(base_size = 16) +
  theme(
    # Axes lines only on bottom and left
    axis.line = element_line(color = "black", size = 1),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    
    # Gridlines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90"),
    
    # Legend
    legend.position = c(0.7, 0.77),
    legend.key.size = unit(0.6, "cm"),
    legend.spacing.y = unit(0, "lines"),
    
    # Text
    text = element_text(size = 18, face = "bold")
  )

################################################################################
### RFID per Night Graph (Figure 3F) ###########################################
################################################################################

fig3F <- ggplot(night_summary, aes(x = Night, y = UniqueTagCount)) +
  geom_line(color = "#1D91C0", size = 1) +
  geom_point(color = "#0C2C84", size = 2) +
  
  # Shaded regions
  geom_rect(aes(xmin = as.Date("2024-11-02") + 0.2, xmax = as.Date("2024-11-03") - 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.05, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-11-06") + 0.2, xmax = as.Date("2024-11-07") - 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.05, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-10-30") + 0.2, xmax = as.Date("2024-10-31") - 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.05, inherit.aes = FALSE) +
  
  # Optional labels for shaded areas
  annotate("text", x = as.Date("2024-11-02") - 0.4, y = 40,
           label = "Capture 2", hjust = 0, size = 5, fontface = "bold") +
  annotate("text", x = as.Date("2024-11-06") - 0.4, y = 40,
           label = "Capture 3", hjust = 0, size = 5, fontface = "bold") +
  annotate("text", x = as.Date("2024-10-30") - 0.4, y = 40,
           label = "Capture 1", hjust = 0, size = 5, fontface = "bold") +
  
  labs(
    title = " ",
    x = "Night",
    y = "Unique Microchipped Bats"
  ) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  
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
    legend.text = element_text(face = "bold", size = 14),
    
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14, hjust = 1, angle = 45),
    axis.text.y = element_text(face = "bold", size = 14)
  )

################################################################################

# Combine fig3E and fig3F side by side
plot_grid(
  fig3E, NULL, fig3F,           # the plots to combine
  # labels = c("E", "", "F"), 
  label_size = 18, 
  ncol = 3,             # number of columns
  align = "h", 
  rel_widths = c(1, 0.2, 1)
)

################################################################################
#### Figure 4 A-D ##############################################################
################################################################################

# Load data
df <- read_csv("Casimiro_capture_data.csv", skip = 1)

# Clean column names
colnames(df) <- c("Date", "RFID", "ArmBand", "UniqueBat", "PreviousRFID", 
                  "CaptureType", "Sex", "Age", "GelApplied", "HairSample")

# Standardize date format
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

# Reformat df with each row representing a unique bat
summary_df <- df %>%
  group_by(UniqueBat, Sex, Age) %>%
  summarise(
    `10/31 Capture` = ifelse(any(Date == as.Date("2024-10-31")), 1, 0),
    `10/31 Treatment` = ifelse(any(Date == as.Date("2024-10-31") & GelApplied == 1), 1, 0),
    `11/3 Capture` = ifelse(any(Date == as.Date("2024-11-03")), 1, 0),
    `11/7 Capture` = ifelse(any(Date == as.Date("2024-11-07")), 1, 0),
    `11/3 Hair` = HairSample[Date == as.Date("2024-11-03")][1],
    `11/7 Hair` = HairSample[Date == as.Date("2024-11-07")][1],
    .groups = "drop"
  ) %>%
  mutate(AgeGroup = ifelse(Age %in% c("SA", "J"), "J", Age)) %>%
  filter(!is.na(`11/3 Hair`) | !is.na(`11/7 Hair`))

# Change format for plotting
long_df <- summary_df %>%
  select(UniqueBat, Sex, AgeGroup, 
         `10/31 Capture`, `10/31 Treatment`, 
         `11/3 Capture`, `11/3 Hair`,
         `11/7 Capture`, `11/7 Hair`) %>%
  pivot_longer(
    cols = c(`10/31 Capture`, `10/31 Treatment`, `11/3 Capture`, `11/3 Hair`, `11/7 Capture`, `11/7 Hair`),
    names_to = c("Date", "Measure"),
    names_sep = " ",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  mutate(
    Date = case_when(
      Date == "10/31" ~ as.Date("2024-10-31"),
      Date == "11/3" ~ as.Date("2024-11-03"),
      Date == "11/7" ~ as.Date("2024-11-07")
    ),
    Capture = ifelse(is.na(Capture), 0, Capture),  # just in case
    PointType = case_when(
      Date == as.Date("2024-10-31") & Capture == 1 & Treatment == 1 ~ "treated",
      Date == as.Date("2024-10-31") & Capture == 1 ~ "captured_only",
      Date %in% c(as.Date("2024-11-03"), as.Date("2024-11-07")) & Capture == 1 & Hair == 1 ~ "rb_pos",
      Date %in% c(as.Date("2024-11-03"), as.Date("2024-11-07")) & Capture == 1 & Hair == 0 ~ "rb_neg",
      Date %in% c(as.Date("2024-11-03"), as.Date("2024-11-07")) & Capture == 1 & Hair == 2 ~ "not_collected",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(PointType))


shapes <- c(
  captured_only = 1,
  treated = 15,
  rb_pos = 16,
  rb_neg = 16,
  not_collected = 17
)
colors <- c(
  captured_only = "black",
  treated = "orange",
  rb_pos = "green",
  rb_neg = "red",
  not_collected = "gray"
)

long_df <- long_df %>%
  mutate(
    Shape = shapes[PointType],
    Color = colors[PointType]
  )

# Factor for age and sex
long_df$Sex <- factor(long_df$Sex, levels = c("M", "F"))
long_df$AgeGroup <- factor(long_df$AgeGroup, levels = c("A", "J"))

# Separate by cohort
AM <- filter(long_df, AgeGroup == "A", Sex == "M")
AF <- filter(long_df, AgeGroup == "A", Sex == "F")
JM <- filter(long_df, AgeGroup == "J", Sex == "M")
JF <- filter(long_df, AgeGroup == "J", Sex == "F")

plot_bats <- function(df, title) {
  points <- unique(df$PointType)
  ggplot(df, aes(x = Date, y = factor(UniqueBat))) +
    geom_point(aes(color = PointType, shape = PointType), size = 4) +
    geom_line(aes(group = UniqueBat), color = "black", linetype = "dashed") +
    scale_shape_manual(values = shapes[points]) +
    scale_color_manual(values = colors[points]) +
    scale_x_date(
      breaks = as.Date(c("2024-10-31", "2024-11-03", "2024-11-07")),
      labels = c("Capture 1", "Capture 2", "Capture 3")
    ) +
    labs(title = title, x = NULL, y = "Bat ID") +
    theme_minimal() +
    theme(
      title = element_text(face = "bold", size = 16),
      legend.position = "none",
      axis.title.x = element_text(face = "bold", size = 18),
      axis.title.y = element_text(face = "bold", size = 18),
      axis.text.x = element_text(face = "bold", size = 14, angle = 45, hjust = 0.5, vjust = 0.7),
      axis.text.y = element_blank(),
      plot.margin = margin(t = 15, r = 25, b = 15, l = 15)  # <-- extra right margin
    )
}

# Generate plots
p1 <- plot_bats(AM, "Adult Males")
p2 <- plot_bats(AF, "Adult Females")
p3 <- plot_bats(JM, "Juvenile Males")
p4 <- plot_bats(JF, "Juvenile Females")

# Create invisible spacer
spacer <- ggplot() + theme_void()

# Combine top row with horizontal spacer
top_row <- plot_grid(p1, spacer, p2, ncol = 3, rel_widths = c(1, 0.1, 1))
bottom_row <- plot_grid(p3, spacer, p4, ncol = 3, rel_widths = c(1, 0.1, 1))

# Combine rows with vertical spacer
final_plot <- plot_grid(top_row, spacer, bottom_row, ncol = 1, rel_heights = c(1, 0.05, 1))

# Display
final_plot

