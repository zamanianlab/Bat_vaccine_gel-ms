library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)

# Read and clean data
df <- read_csv("Strain_sweep_test.csv", skip_empty_rows = TRUE) |> 
  janitor::clean_names() |> 
  filter(!is.na(material))

df <- df |> mutate(across(c(replicate, strain, g, g_2, temp), as.numeric))

df <- df %>%
  mutate(
    g = round(g, 3),
    g_2 = round(g_2, 3),
    strain = round(strain, 4),
    temp = round(temp, 0)
  )

#### Figure 1D #################################################################

# Combinations of material and temperature to plot
plot_conditions <- tribble(
  ~material,         ~temp,
  "12.5% CMC",        40,
  "GJ",               40
)

# Combine data across replicates for both G' and G''
summary_all <- plot_conditions %>%
  rowwise() %>%
  do({
    m <- .$material
    t <- .$temp
    
    df_filtered <- df %>%
      filter(material == m, temp == t) %>%
      arrange(replicate, strain) %>%
      group_by(replicate) %>%
      mutate(row = row_number()) %>%
      ungroup() %>%
      group_by(row) %>%
      summarise(
        mean_strain = mean(strain, na.rm = TRUE),
        mean_g = mean(g, na.rm = TRUE),
        se_g = sd(g, na.rm = TRUE) / sqrt(n()),
        mean_g2 = mean(g_2, na.rm = TRUE),
        se_g2 = sd(g_2, na.rm = TRUE) / sqrt(n()),
        material = m,
        temp = t,
        .groups = "drop"
      )
    df_filtered
  }) %>%
  bind_rows() %>%
  mutate(series = paste(material, temp, "°C")) |>
  pivot_longer(
    cols = c(mean_g, mean_g2, se_g, se_g2),
    names_to = c("stat", "modulus"),
    names_pattern = "(mean|se)_(g2?)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    modulus = recode(modulus, "g" = "G'", "g2" = "G''"),
    modulus = factor(modulus, levels = c("G'", "G''")),
    series_mod = paste(series, modulus, sep = " ")
  ) 

colors <- c(
  "12.5% CMC 40 °C G'" = "darkred",
  "12.5% CMC 40 °C G''" = "red",
  "GJ 40 °C G'" = "darkblue",
  "GJ 40 °C G''" = "blue"
)

ggplot(summary_all, aes(x = mean_strain, y = mean, color = series_mod)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.05, linewidth = 0.8) +
  scale_color_manual(values = colors) +
  scale_x_log10() +
  labs(
    x = "Strain %",
    y = "G' and G'' (Pa)",
    color = " ",
    linetype = "Modulus"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.3),
    legend.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14)
  )

##########  Figure S2A #########################################################

# Combinations of material and temperature to plot
plot_conditions <- tribble(
  ~material,         ~temp,
  "12.5% CMC",        0,
  "GJ",               0
)

summary_all <- plot_conditions %>%
  rowwise() %>%
  do({
    m <- .$material
    t <- .$temp
    
    df_filtered <- df %>%
      filter(material == m, temp == t) %>%
      arrange(replicate, strain) %>%
      group_by(replicate) %>%
      mutate(row = row_number()) %>%
      ungroup() %>%
      group_by(row) %>%
      summarise(
        mean_strain = mean(strain, na.rm = TRUE),
        mean_g = mean(g, na.rm = TRUE),
        se_g = sd(g, na.rm = TRUE) / sqrt(n()),
        mean_g2 = mean(g_2, na.rm = TRUE),
        se_g2 = sd(g_2, na.rm = TRUE) / sqrt(n()),
        material = m,
        temp = t,
        .groups = "drop"
      )
    df_filtered
  }) %>%
  bind_rows() %>%
  mutate(series = paste(material, temp, "°C")) |>
  pivot_longer(
    cols = c(mean_g, mean_g2, se_g, se_g2),
    names_to = c("stat", "modulus"),
    names_pattern = "(mean|se)_(g2?)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    modulus = recode(modulus, "g" = "G'", "g2" = "G''"),
    modulus = factor(modulus, levels = c("G'", "G''")),
    series_mod = paste(series, modulus, sep = " ")
  )

colors <- c(
  "12.5% CMC 0 °C G'" = "darkred",
  "12.5% CMC 0 °C G''" = "red",
  "GJ 0 °C G'" = "darkblue",
  "GJ 0 °C G''" = "blue"
)

ggplot(summary_all, aes(x = mean_strain, y = mean, color = series_mod)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.05, linewidth = 0.8) +
  scale_color_manual(values = colors) +
  scale_x_log10() +
  labs(
    x = "Strain %",
    y = "G' and G'' (Pa)",
    color = " ",
    linetype = "Modulus"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.25),
    legend.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14)
  ) + 
  guides(color = guide_legend(ncol = 2))


##########  Figure S2B  ########################################################

# Combinations of material and temperature to plot
plot_conditions <- tribble(
  ~material,         ~temp,
  "12.5% CMC",        20,
  "GJ",               20
)

# Summarize data across replicates by row position for both G' and G''
summary_all <- plot_conditions %>%
  rowwise() %>%
  do({
    m <- .$material
    t <- .$temp
    
    df_filtered <- df %>%
      filter(material == m, temp == t) %>%
      arrange(replicate, strain) %>%
      group_by(replicate) %>%
      mutate(row = row_number()) %>%
      ungroup() %>%
      group_by(row) %>%
      summarise(
        mean_strain = mean(strain, na.rm = TRUE),
        mean_g = mean(g, na.rm = TRUE),
        se_g = sd(g, na.rm = TRUE) / sqrt(n()),
        mean_g2 = mean(g_2, na.rm = TRUE),
        se_g2 = sd(g_2, na.rm = TRUE) / sqrt(n()),
        material = m,
        temp = t,
        .groups = "drop"
      )
    df_filtered
  }) %>%
  bind_rows() %>%
  mutate(series = paste(material, temp, "°C")) |>
  pivot_longer(
    cols = c(mean_g, mean_g2, se_g, se_g2),
    names_to = c("stat", "modulus"),
    names_pattern = "(mean|se)_(g2?)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    modulus = recode(modulus, "g" = "G'", "g2" = "G''"),
    modulus = factor(modulus, levels = c("G'", "G''")),
    series_mod = paste(series, modulus, sep = " ")
  )

colors <- c(
  "12.5% CMC 20 °C G'" = "darkred",
  "12.5% CMC 20 °C G''" = "red",
  "GJ 20 °C G'" = "darkblue",
  "GJ 20 °C G''" = "blue"
)

ggplot(summary_all, aes(x = mean_strain, y = mean, color = series_mod)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.05, linewidth = 0.8) +
  scale_color_manual(values = colors) +
  scale_x_log10() +
  labs(
    x = "Strain %",
    y = "G' and G'' (Pa)",
    color = " ",
    linetype = "Modulus"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.3),
    legend.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14)
  )
