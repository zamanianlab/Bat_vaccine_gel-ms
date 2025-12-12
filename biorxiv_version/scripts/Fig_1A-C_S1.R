library(readr)
library(dplyr)
library(ggplot2)
library(janitor)

# Read and clean data
df <- read_csv("Rate_sweep_test.csv", skip_empty_rows = TRUE) |> 
  janitor::clean_names() |> 
  filter(!is.na(material))

df <- df |> mutate(across(c(replicate, time, eta, rate, temp), as.numeric))

df <- df %>%
  mutate(
    eta = round(eta, 3), 
    rate = round(rate, 4),
    temp = round(temp, 0)
  )

##### Figure 1A ################################################################

# Combinations of material and temperature to plot
plot_conditions <- tribble(
  ~material,         ~temp,
  "10% CMC",        40,
  "12.5% CMC",        40,
  "15% CMC",        40,
)

# Combine replicates for each series
summary_all <- plot_conditions %>%
  rowwise() %>%
  do({
    m <- .$material
    t <- .$temp
    df_filtered <- df %>%
      filter(material == m, temp == t) %>%
      group_by(rate) %>%
      summarise(
        mean_eta = mean(eta, na.rm = TRUE),
        se_eta = sd(eta, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(material = m, temp = t)
    df_filtered
  }) %>%
  bind_rows() %>%
  mutate(series = paste(material, temp, "°C"))

custom_colors <- c(
  "10% CMC 40 °C" = "gold",
  "12.5% CMC 40 °C" = "red",
  "15% CMC 40 °C"   = "darkgreen"
)

ggplot(summary_all, aes(x = rate, y = mean_eta, color = series)) +
  geom_line(aes(color = series, group = series), linewidth = 0.8) +
  geom_point(aes(color = series, group = series), size = 2.5) +
  geom_errorbar(aes(ymin = mean_eta - se_eta, ymax = mean_eta + se_eta), width = 0.05, linewidth = 0.8) +
  scale_color_manual(values = custom_colors) +
  scale_x_log10() +
  labs(
    title = "",
    x = "Shear Rate (1/s)",
    y = "Viscosity (Pa·s)",
    color = " "
  ) +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.7),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))


#### Figure 1B #################################################################

plot_conditions <- tribble(
  ~material,         ~temp,
  "12.5% CMC",        0,
  "12.5% CMC",        20,
  "12.5% CMC",        40,
  "GJ",               0,
  "GJ",   20,
  "GJ",   40
)

summary_all <- plot_conditions %>%
  rowwise() %>%
  do({
    m <- .$material
    t <- .$temp
    df_filtered <- df %>%
      filter(material == m, temp == t) %>%
      group_by(rate) %>%
      summarise(
        mean_eta = mean(eta, na.rm = TRUE),
        se_eta = sd(eta, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(material = m, temp = t)
    df_filtered
  }) %>%
  bind_rows() %>%
  mutate(series = paste(material, temp, "°C"))

custom_colors <- c(
  "12.5% CMC 0 °C" = "darkred",
  "12.5% CMC 20 °C" = "red",
  "12.5% CMC 40 °C"   = "pink",
  "GJ 0 °C"   = "darkblue",
  "GJ 20 °C" = "blue",
  "GJ 40 °C" = "mediumturquoise"   
)

ggplot(summary_all, aes(x = rate, y = mean_eta, color = series)) +
  geom_line(aes(color = series, group = series), linewidth = 0.8) +
  geom_point(aes(color = series, group = series), size = 2.5) +
  geom_errorbar(aes(ymin = mean_eta - se_eta, ymax = mean_eta + se_eta), width = 0.05, linewidth = 0.8) +
  scale_color_manual(values = custom_colors) +
  scale_x_log10() +
  labs(
    title = "",
    x = "Shear Rate (1/s)",
    y = "Viscosity (Pa·s)",
    # y = expression(paste("Mean ", eta, " ± SE (Pa·s)")),
    color = " "
  ) +
  theme_minimal() +  # Minimal theme
  theme(legend.position = c(0.7, 0.7),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))

#### Figure 1C ################################################################

ggplot(summary_all, aes(x = rate, y = mean_eta, color = series)) +
  geom_line(aes(color = series, group = series), linewidth = 0.8) +
  geom_point(aes(color = series, group = series), size = 2.5) +
  geom_errorbar(aes(ymin = mean_eta - se_eta, ymax = mean_eta + se_eta), width = 3, linewidth = 0.8) +
  scale_color_manual(values = custom_colors) +
  scale_x_log10() +
  xlim(20, 200) +
  ylim(0, 35) +
  labs(
    title = "",
    x = "Shear Rate (1/s)",
    y = "Viscosity (Pa·s)",
    # y = expression(paste("Mean ", eta, " ± SE (Pa·s)")),
    color = " "
  ) +
  theme_minimal() +  # Minimal theme
  theme(legend.position = c(0.7, 0.7),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))



##### Figure S1A ################################################################

# Combinations of material and temperature to plot
plot_conditions <- tribble(
  ~material,         ~temp,
  "10% CMC",        0,
  "12.5% CMC",        0,
  "15% CMC",        0,
)

# Combine replicates for each series
summary_all <- plot_conditions %>%
  rowwise() %>%
  do({
    m <- .$material
    t <- .$temp
    df_filtered <- df %>%
      filter(material == m, temp == t) %>%
      group_by(rate) %>%
      summarise(
        mean_eta = mean(eta, na.rm = TRUE),
        se_eta = sd(eta, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(material = m, temp = t)
    df_filtered
  }) %>%
  bind_rows() %>%
  mutate(series = paste(material, temp, "°C"))

custom_colors <- c(
  "10% CMC 0 °C" = "gold",
  "12.5% CMC 0 °C" = "red",
  "15% CMC 0 °C"   = "darkgreen"
)

ggplot(summary_all, aes(x = rate, y = mean_eta, color = series)) +
  geom_line(aes(color = series, group = series), linewidth = 0.8) +
  geom_point(aes(color = series, group = series), size = 2.5) +
  geom_errorbar(aes(ymin = mean_eta - se_eta, ymax = mean_eta + se_eta), width = 0.05, linewidth = 0.8) +
  scale_color_manual(values = custom_colors) +
  scale_x_log10() +
  labs(
    title = "",
    x = "Shear Rate (1/s)",
    y = "Viscosity (Pa·s)",
    color = " "
  ) +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.7),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))

##### Figure S1B ################################################################

# Combinations of material and temperature to plot
plot_conditions <- tribble(
  ~material,         ~temp,
  "10% CMC",        20,
  "12.5% CMC",        20,
  "15% CMC",        20,
)

# Combine replicates for each series
summary_all <- plot_conditions %>%
  rowwise() %>%
  do({
    m <- .$material
    t <- .$temp
    df_filtered <- df %>%
      filter(material == m, temp == t) %>%
      group_by(rate) %>%
      summarise(
        mean_eta = mean(eta, na.rm = TRUE),
        se_eta = sd(eta, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(material = m, temp = t)
    df_filtered
  }) %>%
  bind_rows() %>%
  mutate(series = paste(material, temp, "°C"))

custom_colors <- c(
  "10% CMC 20 °C" = "gold",
  "12.5% CMC 20 °C" = "red",
  "15% CMC 20 °C"   = "darkgreen"
)

ggplot(summary_all, aes(x = rate, y = mean_eta, color = series)) +
  geom_line(aes(color = series, group = series), linewidth = 0.8) +
  geom_point(aes(color = series, group = series), size = 2.5) +
  geom_errorbar(aes(ymin = mean_eta - se_eta, ymax = mean_eta + se_eta), width = 0.05, linewidth = 0.8) +
  scale_color_manual(values = custom_colors) +
  scale_x_log10() +
  labs(
    title = "",
    x = "Shear Rate (1/s)",
    y = "Viscosity (Pa·s)",
    color = " "
  ) +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.7),
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))



