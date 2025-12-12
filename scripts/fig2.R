library(ggplot2)
library(dplyr)
library(readr)
library(brms)
library(tidyr)
library(purrr)
library(tidyverse)
library(cowplot)
library(grid)

base_theme <- theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(face = "bold", size = 12),
    
    # KEEP ONLY Y GRIDLINES
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_line(),
    
    # TURN ON AXIS LINES
    axis.line = element_line(color = "black")
  )

#### Import/clean data #########################################################

data <- read_csv("Stability_assay_1.csv") %>%
  rename(titer = `Log Fold Change`) %>%
  mutate(
    Formulation = recode(Formulation, `Glycerin Jelly` = "GJ"),
    Temperature = as.factor(Temperature),
    Replicate = as.factor(Replicate)
  )

#### FIGURE 2A #################################################################

# Filter data for 6 months plot
data_6m <- data %>%
  filter(!is.na(titer)) %>%
  group_by(Days, Formulation, Temperature) %>%
  summarise(
    LogfoldChange = mean(titer, na.rm = TRUE),
    SE = sd(titer, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(Days %in% c(0, 7, 14, 30, 90, 180)) %>%
  mutate(
    Group = factor(
      paste0(Formulation, " ", Temperature, "°C"),
      levels = c(
        "CMC 4°C", "CMC 23°C", "CMC 40°C",
        "GJ 4°C",  "GJ 23°C",  "GJ 40°C"
      )
    )
  )

shapes <- c(15, 16, 17, 22, 21, 24)
fills  <- rep("white", 6)
colors <- c("darkred", "red", "salmon", "darkblue", "blue", "mediumturquoise")

p1 <- ggplot(data_6m, aes(Days, LogfoldChange, shape = Group, fill = Group, color = Group)) +
  geom_line(linewidth = 0.9) +
  geom_errorbar(aes(ymin = LogfoldChange - SE, ymax = LogfoldChange + SE),
                width = 4, linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 2.5, stroke = 2) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  scale_color_manual(values = colors) +
  scale_x_continuous(limits = c(0, 185), breaks = seq(0, 180, 30)) +
  labs(
    x = "Days of Storage",
    y = expression(bold("Log Fold Change (" * Log[10] * " PFU/mL)"))
  ) +
  base_theme +
  theme(
    legend.position = "left",
    legend.text = element_text(face = "bold"),
    legend.title = element_blank()
  )

#### Fit Bayesian linear model #################################################

# Fit the Bayesian model (weakly informative priors, same as first assay)
brms_model <- brm(
  formula = titer ~ Days * Formulation * Temperature + (1|Replicate), 
  data = data, 
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class = "b"),
    set_prior("student_t(3, 0, 2.5)", class = "sigma")
  ),
  chains = 4,
  iter = 8000,
  warmup = 2000,
  seed = 123
)

#### Extract posterior draws ###################################################

posterior <- as_draws_df(brms_model)  # tibble of all posterior samples

# Identify coefficient names for interaction model
coef_names <- posterior %>% select(starts_with("b_")) %>% names()

# Function to compute slope for each group
get_slope <- function(form, temp) {
  # base Days coefficient
  slope <- posterior$b_Days
  
  # Add interactions if applicable
  if (form != "CMC") {
    slope <- slope + posterior[[paste0("b_Days:Formulation", form)]]
  }
  if (temp != "4") {
    slope <- slope + posterior[[paste0("b_Days:Temperature", temp)]]
  }
  if (form != "CMC" & temp != "4") {
    slope <- slope + posterior[[paste0("b_Days:Formulation", form, ":Temperature", temp)]]
  }
  return(slope * 7)  # convert to weekly slope
}

# Compute slopes for all Formulation × Temperature combinations
groups <- expand.grid(
  Formulation = c("GJ", "CMC"),
  Temperature = c("4", "23", "40")
)

slopes_df <- groups %>%
  mutate(
    posterior_slope = map2(Formulation, Temperature, get_slope),
    DecayRate = map_dbl(posterior_slope, median),  # posterior median
    CI_Lower = map_dbl(posterior_slope, ~quantile(.x, 0.025)),
    CI_Upper = map_dbl(posterior_slope, ~quantile(.x, 0.975)),
    Group = factor(paste(Formulation, Temperature),
                   levels = c("GJ 4", "CMC 4", "GJ 23", "CMC 23", "GJ 40", "CMC 40"))
  ) %>%
  select(Formulation, Temperature, Group, DecayRate, CI_Lower, CI_Upper)

#### Plot decay rates ##########################################################

p2 <- ggplot(slopes_df, aes(x = Formulation, y = DecayRate, color = Group, shape = Group)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                width = 0.2, linewidth = 0.8) +
  geom_point(aes(fill = Group), size = 2.5, stroke = 2.5) +
  facet_grid(~ Temperature, drop = FALSE, 
             labeller = labeller(Temperature = c(`4` = "4 °C", `23` = "23 °C", `40` = "40 °C"))) +
  scale_shape_manual(values = c(22, 15, 21, 16, 24, 17)) +
  scale_fill_manual(values = rep("white", 6)) +
  scale_color_manual(values = c("darkblue", "darkred", "blue", "red", "mediumturquoise", "salmon")) +
  labs(x = "Formulation", y = expression(bold("Weekly Decay Rate (" * Log[10] * " PFU/mL)"))) +
  ylim(-4.25, 0.25) +
  base_theme +
  theme(axis.text.x = element_text(face = "bold", size = 12),
        strip.text = element_text(face = "bold", size = 12))


################################################################################
#### Stability Assay 2 Data ####################################################
################################################################################

data2<- read_csv("Stability_assay_2.csv") %>%
  rename(titer = `Log Fold Change`) %>%
  mutate(
    Formulation = factor(Formulation),
    Temperature = factor(Temperature),
    Replicate   = factor(Replicate),
    Days        = as.numeric(Days),
    titer       = as.numeric(titer)
  ) %>%
  filter(Temperature != -80) %>%
  droplevels()  # <-- drop -80 from factor levels

################################################################################

# Define aesthetics
shapes_p1 <- c(15, 22, 16, 21, 17, 24)
colors_p1 <- c("darkred", "red", "darkblue", "blue", "darkgreen", "green")

shapes_p2 <- c(15, 16, 17, 22, 21, 24)
colors_p2 <- c("darkred", "darkblue", "darkgreen", "red", "blue", "green")

assay2_data <- data2%>%
  filter(
    !is.na(titer),
    Days %in% c(0, 7, 14, 30, 90),
    Formulation %in% c("C", "R", "S")
  ) %>%
  group_by(Days, Formulation, Temperature) %>%
  summarise(
    LogfoldChange = mean(titer, na.rm = TRUE),
    SE = sd(titer, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    TempLabel = paste0(Temperature, "°C"),
    Group     = factor(
      paste(Formulation, TempLabel),
      levels = c("C 4°C", "C 23°C", "R 4°C", "R 23°C", "S 4°C", "S 23°C")
    ),
    LineType = ifelse(Temperature == 4, "solid", "dashed")
  )

p3 <- ggplot(
  assay2_data,
  aes(Days, LogfoldChange, shape = Group, fill = Group,
      color = Group, linetype = LineType, group = Group)
) +
  geom_line(linewidth = 0.9) +
  geom_errorbar(
    aes(ymin = LogfoldChange - SE, ymax = LogfoldChange + SE),
    width = 3.5, linewidth = 0.9, alpha = 0.7
  ) +
  geom_point(size = 2.5, stroke = 2) +
  scale_shape_manual(values = shapes_p1) +
  scale_fill_manual(values = rep("white", 6)) +
  scale_color_manual(values = colors_p1) +
  scale_linetype_identity() +
  scale_x_continuous(breaks = seq(0, 90, 30)) +
  ylim(-6.25, 0) +
  labs(
    x = "Days of Storage",
    y = expression(bold("Log Fold Change (" * Log[10] * " PFU/mL)"))
  ) +
  annotate("label", x = 85, y = -5.8, label = "n.d.", color = "red", size = 4) +
  annotate("label", x = 45, y = -5.8, label = "n.d.", color = "blue", size = 4) +
  base_theme +
  theme(
    legend.position = "left",
    legend.text = element_text(face = "bold"),
    legend.title = element_blank()
  )


###############################################################################
#                                   P2

# Fit the Bayesian model (weakly informative priors, same as first assay)
brms_model2 <- brm(
  formula = titer ~ Days * Formulation * Temperature + (1|Replicate),
  data = data2, 
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class = "b"),
    set_prior("student_t(3, 0, 2.5)", class = "sigma")
  ),
  chains = 4,
  iter = 8000,
  warmup = 2000,
  seed = 123
)


# Extract posterior draws
posterior2 <- as_draws_df(brms_model2)  # tibble of posterior samples

# Function to compute slope (Days effect) for each Formulation × Temperature
# This recreates the emtrends-style contrasts
# Uses posterior predictions at Days = 0 and Days = 1, difference = slope per day

get_slope2 <- function(form, temp) {
  slope <- posterior2$b_Days  # base Days effect
  
  # Formulation main effect
  form_name <- paste0("b_Days:Formulation", form)
  if (form_name %in% names(posterior2)) slope <- slope + posterior2[[form_name]]
  
  # Temperature main effect
  temp_name <- paste0("b_Days:Temperature", temp)
  if (temp_name %in% names(posterior2)) slope <- slope + posterior2[[temp_name]]
  
  # Interaction effect
  int_name <- paste0("b_Days:Formulation", form, ":Temperature", temp)
  if (int_name %in% names(posterior2)) slope <- slope + posterior2[[int_name]]
  
  return(slope * 7)  # convert daily slope to weekly
}

groups2 <- expand.grid(
  Formulation = levels(data2$Formulation),
  Temperature = levels(data2$Temperature)
)

slopes_df2 <- groups2 %>%
  mutate(
    posterior_slope = map2(Formulation, Temperature, get_slope2),
    DecayRate = map_dbl(posterior_slope, median),
    CI_Lower  = map_dbl(posterior_slope, ~quantile(.x, 0.025)),
    CI_Upper  = map_dbl(posterior_slope, ~quantile(.x, 0.975)),
    Group = paste(Formulation, Temperature)
  ) %>%
  select(Formulation, Temperature, Group, DecayRate, CI_Lower, CI_Upper)

slopes_df2$Group <- factor(slopes_df2$Group, levels = c("C 4", "R 4", "S 4", "C 23", "R 23", "S 23"))

# The rest of your plotting code can remain unchanged
p4 <- ggplot(
  slopes_df2,
  aes(
    x = Formulation,
    y = DecayRate,
    color = Group,
    shape = Group,
    group = Group
  )
) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.2, linewidth = 0.8,
    position = position_identity()
  ) +
  geom_point(
    aes(fill = Group),
    size = 2.5, stroke = 2.5,
    position = position_identity()
  ) +
  
  scale_shape_manual(values = shapes_p2) +
  scale_fill_manual(values = rep("white", 6)) +
  scale_color_manual(values = colors_p2) +
  
  scale_x_discrete(labels = c("C" = "C", "R" = "R", "S" = "S")) +
  
  ylim(-1.25, 0.2) +
  labs(
    x = "Formulation",
    y = expression(bold("Weekly Decay Rate (" * Log[10] * " PFU/mL)"))
  ) +
  base_theme +
  
  facet_grid(
    ~ Temperature,
    labeller = labeller(
      Temperature = c(`4` = "4 °C", `23` = "23 °C")
    )
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 12)
  )

################################################################################

# Spacer grob (fixed-width blank column)
spacer <- ggdraw() + theme_void()

# Column 1 = A over C
col1 <- plot_grid(
  NULL,
  p1, 
  NULL,
  p3,
  ncol = 1,
  rel_heights = c(0.05, 1, 0.05, 1),
  align = "v",
  axis = "l",
  labels = c("a","","c"),
  label_size = 18,
  label_fontface = "bold"
)

# Column 2 = B over D
col2 <- plot_grid(
  NULL,
  p2, 
  NULL,
  p4,
  ncol = 1,
  rel_heights = c(0.05, 1, 0.05, 1),
  align = "v",
  axis = "l",
  labels = c("b","","d"),
  label_size = 18,
  label_fontface = "bold",
  label_x = -0.075
)

# Combine with a spacer column in the middle
figure2 <- plot_grid(
  col1,
  spacer,
  col2,
  nrow = 1,
  rel_widths = c(1, 0.1, 0.7)   # adjust spacer width as needed
)

figure2
