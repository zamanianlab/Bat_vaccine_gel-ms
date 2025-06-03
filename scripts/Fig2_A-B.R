library(ggplot2)
library(dplyr)
library(readr)

data <- read_csv("Stability_assay.csv")

shapes <- c(15, 16, 17, 22, 21, 24)
fills <- c("white", "white", "white", "white", "white", "white")
colors <- c("darkred", "red", "salmon", "darkblue", "blue", "mediumturquoise")


#### Figure 1A ###############################################################

# Filter data for first week
data_1w <- data %>%
  filter(!is.na(`Log Fold Change`)) %>%
  filter(Days %in% c(0, 1, 2, 4, 7)) %>%
  group_by(Days, Formulation, Temperature) %>%
  summarise(
    LogfoldChange = mean(`Log Fold Change`, na.rm = TRUE),
    SE = sd(`Log Fold Change`, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    Formulation = ifelse(Formulation == "Glycerin Jelly", "GJ", Formulation),
    Group = paste0(Formulation, " ", Temperature, "\u00B0C"),
    Group = factor(Group, levels = c(
      "CMC 4°C", "CMC 23°C", "CMC 40°C",
      "GJ 4°C", "GJ 23°C", "GJ 40°C"
    ))
  )

ggplot(data_1w, aes(x = Days, y = LogfoldChange,
                    shape = Group, fill = Group, color = Group)) +
  geom_line(linewidth = 0.9) +
  geom_errorbar(aes(ymin = LogfoldChange - SE, ymax = LogfoldChange + SE),
                width = 0.25, linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 2.5, stroke = 2) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  scale_color_manual(values = colors) +
  labs(
    x = "Days of Storage",
    y = "Log fold Change (Log10 PFU/mL)",
    color = "Formulation",
    fill = "Formulation",
    shape = "Formulation"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = c(0.25, 0.25),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(color = "black", size = 0.8, fill = NA),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14)
  )

#### Figure 2B #################################################################

#Filter data for 3-month plot 
data_3m <- data %>%
  filter(!is.na(`Log Fold Change`)) %>%
  group_by(Days, Formulation, Temperature) %>%
  summarise(
    LogfoldChange = mean(`Log Fold Change`, na.rm = TRUE),
    SE = sd(`Log Fold Change`, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(Days %in% c(0, 14, 30, 90)) %>%
  mutate(
    Formulation = ifelse(Formulation == "Glycerin Jelly", "GJ", Formulation),
    Group = paste0(Formulation, " ", Temperature, "\u00B0C"),
    Group = factor(Group, levels = c(
      "CMC 4°C", "CMC 23°C", "CMC 40°C",
      "GJ 4°C", "GJ 23°C", "GJ 40°C"
    ))
  )


# Main plot (full time course)
ggplot(data_3m, aes(x = Days, y = LogfoldChange,
                         shape = Group, fill = Group, color = Group)) +
  geom_line(linewidth = 0.9) +
  geom_errorbar(aes(ymin = LogfoldChange - SE, ymax = LogfoldChange + SE),
                width = 2, linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 2.5, stroke = 2) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  scale_color_manual(values = colors) +
  labs(
    x = "Days of Storage",
    y = "Log fold Change (Log10 PFU/mL)",
    color = "Formulation",
    fill = "Formulation",
    shape = "Formulation"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = c(0.55, 0.25),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(color = "black", size = 0.8, fill = NA),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14)
  )

