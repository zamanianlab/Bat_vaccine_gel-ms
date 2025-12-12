library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(cowplot)

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

################################################################################

# Plotting function
plot_strain_sweep <- function(df, material_name, temps = c(0, 20, 40),
                              colors = NULL, strain_range = NULL, log_scale = TRUE) {
  
  # Combine data across replicates for G' and G''
  summary_all <- tibble(material = material_name, temp = temps) %>%
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
          se_g   = sd(g, na.rm = TRUE) / sqrt(n()),
          mean_g2 = mean(g_2, na.rm = TRUE),
          se_g2   = sd(g_2, na.rm = TRUE) / sqrt(n()),
          material = m,
          temp = t,
          .groups = "drop"
        )
      df_filtered
    }) %>%
    bind_rows() %>%
    mutate(series = paste(material, temp, "°C")) %>%
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
  
  summary_all <- summary_all %>% 
    group_by(series_mod) %>% 
    mutate(idx = row_number()) %>% 
    filter(idx %% 2 == 1) %>%     # keep odd rows → every other point
    ungroup()
  
  
  # Filter for strain range if provided
  if (!is.null(strain_range)) {
    summary_all <- summary_all %>% filter(mean_strain >= strain_range[1],
                                          mean_strain <= strain_range[2])
  }
  
  # Default colors if none provided
  if (is.null(colors)) {
    temp_colors <- c("0 °C G'" = "darkblue", "0 °C G''" = "blue",
                     "20 °C G'" = "darkgreen", "20 °C G''" = "green",
                     "40 °C G'" = "darkred", "40 °C G''" = "red")
    colors <- setNames(temp_colors, paste(material_name, names(temp_colors)))
  }
  
  temp_shapes <- c("0 °C G'" = 15, "0 °C G''" = 0,
                   "20 °C G'" = 16, "20 °C G''" = 1,
                   "40 °C G'" = 17, "40 °C G''" = 2)
  shapes <- setNames(temp_shapes, paste(material_name, names(temp_colors)))
  
  
  # Conditional width for error bars
  err_width <- ifelse(is.null(strain_range), 0.1, 0.2)
  
  p <- ggdraw(
    ggplot(summary_all, aes(x = mean_strain, y = mean, color = series_mod)) +
      geom_line(aes(group = series_mod), linewidth = 0.8) +
      geom_point(aes(shape = series_mod, group = series_mod), size = 2.5) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = err_width, linewidth = 0.8) +
      scale_color_manual(values = colors) +
      scale_shape_manual(values = shapes) +
      { if (log_scale) scale_x_log10() else scale_x_continuous() } +
      labs(x = "Strain %", y = "G' and G'' (Pa)", color = " ", linetype = "Modulus") +
      theme_minimal() +
      theme(
        axis.title = element_text(face = "bold", size = 14),
        axis.text  = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.x  = element_text(face = "bold", size = 12),
        axis.text.y  = element_text(face = "bold", size = 12)
      ) +
      { if (!is.null(strain_range)) coord_cartesian(xlim = strain_range) }
  ) +
    draw_label(
      material_name,
      x = 0.6,
      y = 1,
      fontface = "bold",
      size = 18
    )
  
  return(p)
}

p1 <- plot_strain_sweep(df, "10% CMC")
p2 <- plot_strain_sweep(df, "12.5% CMC")
p3 <- plot_strain_sweep(df, "15% CMC")
p4 <- plot_strain_sweep(df, "GJ")
p5 <- plot_strain_sweep(df, "PJ")
p6 <- plot_strain_sweep(df, "SQ")

# Create invisible spacer
spacer <- ggplot() + theme_void()
library(cowplot)
library(grid)

# Spacer grob (fixed-width blank column)
spacer <- ggdraw() + theme_void()

# Column 1 = A over C
col1 <- plot_grid(
  NULL,
  p1, 
  NULL,
  p3,
  NULL,
  p5,
  ncol = 1,
  rel_heights = c(0.15, 1, 0.15, 1, 0.15, 1),
  align = "v",
  axis = "l",
  labels = c("a","","c", "", "e"),
  label_size = 18,
  label_fontface = "bold"
)

# Column 2 = B over D
col2 <- plot_grid(
  NULL,
  p2, 
  NULL,
  p4,
  NULL,
  p6,
  ncol = 1,
  rel_heights = c(0.15, 1, 0.15, 1, 0.15, 1),
  align = "v",
  axis = "l",
  labels = c("b","","d", "", "f"),
  label_size = 18,
  label_fontface = "bold",
  label_x = -0.075
)

# Combine with a spacer column in the middle
figure_S3 <- plot_grid(
  col1,
  spacer,
  col2,
  nrow = 1,
  rel_widths = c(1, 0.15, 1)   # adjust spacer width as needed
)

figure_S3_scaled <- ggdraw() +
  draw_plot(figure_S3, scale = 0.9)

figure_S3_scaled
