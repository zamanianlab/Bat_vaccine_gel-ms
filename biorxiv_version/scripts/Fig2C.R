library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)
library(emmeans)

data <- read_csv("Stability_assay.csv")

data <- data %>%
  rename(
    titer = `Log Fold Change`
  ) %>%
  mutate(
    Formulation = as.factor(Formulation),
    Temperature = as.factor(Temperature),
    Replicate = as.factor(Replicate)
  )

# Fit linear models to calculate decay rate
decay_summary <- data %>%
  group_by(Formulation, Temperature) %>%
  do({
    model <- lm(`titer` ~ Days, data = .)
    slope <- coef(model)[["Days"]]
    ci <- confint(model)["Days", ]
    tibble(
      DecayRate = slope,
      CI_Lower = ci[1],
      CI_Upper = ci[2]
    )
  })

#### Figure 2C ####################################################

# Convert to decay rate per week
decay_summary <- decay_summary %>%
  mutate(
    DecayRate = DecayRate * 7,
    CI_Lower = CI_Lower * 7,
    CI_Upper = CI_Upper * 7,
    Group = paste(Formulation, Temperature)
  )

decay_summary$Group <- factor(
  decay_summary$Group,
  levels = c("GJ 4", "CMC 4", "GJ 23", "CMC 23", "GJ 40", "CMC 40")
)

# Model to compare decay rates by formulation
model <- lm(titer ~ Days * Formulation * Temperature + Replicate, data = data)
em_trends <- emtrends(model, ~ Formulation * Temperature, var = "Days")

pairs(em_trends)

group_labels <- levels(decay_summary$Group)

pairwise_comparisons <- t(combn(group_labels, 2))
colnames(pairwise_comparisons) <- c("group1", "group2")

# Labels for significant differences
sig_df <- as.data.frame(pairwise_comparisons) %>%
  mutate(
    label = c(  # Manually input your significance levels
      "ns", "*", "***", "***", "***",
      "ns", "***", "***", "***",
      "***", "***", "***",
      "***", "***",
      "***"
    ),
    y.position = c(
      -0.25, -0.95, -1.65, -2.7, 0.75,
      -0.6, -1.3, -2.0, 0.5,
      -2.35, -3.05, 0.25,
      -0.25, 0,
      -0.75
    )
  )

group_positions <- data.frame(Group = group_labels, x = seq_along(group_labels))

sig_df <- sig_df %>%
  left_join(group_positions, by = c("group1" = "Group")) %>%
  rename(x1 = x) %>%
  left_join(group_positions, by = c("group2" = "Group")) %>%
  rename(x2 = x)


shapes <- c(22, 15, 21, 16, 24, 17)
fills <- c("white", "white", "white", "white", "white", "white")
colors <- c("darkblue", "darkred", "blue", "red", "mediumturquoise", "salmon")

ggplot(decay_summary, aes(x = Group, y = DecayRate, color = Group, shape = Group)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, linewidth = 0.8) +
  labs(x = "Formulation", y = "Weekly Decay Rate (Log10 PFU/mL)") +
  geom_point(aes(fill = Group), size = 2, stroke = 2.5) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = c(
    "GJ 4" = "GJ 4°C",
    "CMC 4" = "CMC 4°C",
    "GJ 23" = "GJ 23°C",
    "CMC 23" = "CMC 23°C",
    "GJ 40" = "GJ 40°C",
    "CMC 40" = "CMC 40°C"
  )) +
  ylim(-5, 0.7) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(color = "black", size = 0.8, fill = NA),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14, color = "black"),
    axis.text.y = element_text(face = "bold", size = 14)
  ) +
  geom_segment(data = sig_df,
               aes(x = x1, xend = x2, y = y.position, yend = y.position),
               inherit.aes = FALSE, linewidth = 0.7) +
  geom_text(data = sig_df,
            aes(x = (x1 + x2) / 2, y = y.position + 0.1, label = label),
            inherit.aes = FALSE, size = 5)