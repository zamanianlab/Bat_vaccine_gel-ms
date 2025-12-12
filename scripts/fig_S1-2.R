library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(tibble)
library(cowplot)
library(grid)

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

###############################################################################

plot_material <- function(df, material_name, temps = c(0, 20, 40), 
                          colors = NULL, rate_range = NULL, log_scale = TRUE) {
  
  # Summarize replicates as before
  summary_all <- tibble(material = material_name, temp = temps) %>%
    rowwise() %>%
    do({
      m <- .$material
      t <- .$temp
      df_filtered <- df %>%
        filter(material == m, temp == t) %>%
        mutate(rate_clean = signif(rate, 2)) %>%
        group_by(rate_clean) %>%
        summarise(
          mean_eta = mean(eta, na.rm = TRUE),
          se_eta   = sd(eta, na.rm = TRUE) / sqrt(n()),
          .groups  = "drop"
        ) %>%
        rename(rate = rate_clean) %>%
        mutate(material = m, temp = t)
      df_filtered
    }) %>%
    bind_rows() %>%
    mutate(series = paste(material, temp, "°C"))
  
  # If rate_range is specified, filter summary_all to that range
  if (!is.null(rate_range)) {
    summary_all <- summary_all %>% filter(rate >= rate_range[1], rate <= rate_range[2])
  }
  
  # Default colors
  if (is.null(colors)) {
    temp_colors <- c("0 °C" = "blue", "20 °C" = "green", "40 °C" = "red")
    colors <- setNames(temp_colors, paste(material_name, names(temp_colors)))
  }
  
  # Conditional width for error bars
  err_width <- ifelse(is.null(rate_range), 0.05, 3)  # ⭐ wider for zoomed plot
  
  # Build ggplot
  p <- ggdraw(
    ggplot(summary_all, aes(x = rate, y = mean_eta, color = series)) +
      geom_line(aes(group = series), linewidth = 0.8) +
      geom_point(aes(group = series), size = 2.5) +
      geom_errorbar(aes(ymin = mean_eta - se_eta, ymax = mean_eta + se_eta), 
                    width = err_width, linewidth = 0.8) +
      scale_color_manual(values = colors) +
      { if (log_scale) scale_x_log10() else scale_x_continuous(
        breaks = seq(50, 200, 50)
      ) } +
      labs(x = "Shear Rate (1/s)", y = "Viscosity (Pa·s)", color = " ") +
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
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.text.x  = element_text(face = "bold", size = 14),
        axis.text.y  = element_text(face = "bold", size = 14)
      )
  ) +
    draw_label(
      material_name,
      x = 0.75,
      y = 0.75,
      fontface = "bold",
      size = 18
    )
  
  return(p)
}

p1 <- plot_material(df, "10% CMC")
p2 <- plot_material(df, "15% CMC")
p3 <- plot_material(df, "PJ")
p4  <- plot_material(df, "SQ")

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
  rel_heights = c(0.075, 0.9, 0.075, 0.9),
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
  rel_heights = c(0.075, 0.9, 0.075, 0.9),
  align = "v",
  axis = "l",
  labels = c("b","","d"),
  label_size = 18,
  label_fontface = "bold",
  label_x = -0.075
)

# Combine with a spacer column in the middle
figure_S1 <- plot_grid(
  col1,
  spacer,
  col2,
  nrow = 1,
  rel_widths = c(1, 0.1, 1)   # adjust spacer width as needed
)

figure_S1

################################################################################

p5 <- plot_material(df, "10% CMC", rate_range = c(20, 200), log_scale = FALSE)
p6 <- plot_material(df, "15% CMC", rate_range = c(20, 200), log_scale = FALSE)
p7 <- plot_material(df, "PJ", rate_range = c(20, 200), log_scale = FALSE)
p8  <- plot_material(df, "SQ", rate_range = c(20, 200), log_scale = FALSE)

# Column 1 = A over C
col3 <- plot_grid(
  NULL,
  p5, 
  NULL,
  p7,
  ncol = 1,
  rel_heights = c(0.05, 1, 0.05, 1),
  align = "v",
  axis = "l",
  labels = c("a","","c"),
  label_size = 18,
  label_fontface = "bold"
)

# Column 2 = B over D
col4 <- plot_grid(
  NULL,
  p6, 
  NULL,
  p8,
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
figure_S2 <- plot_grid(
  col3,
  spacer,
  col4,
  nrow = 1,
  rel_widths = c(1, 0.1, 1)   # adjust spacer width as needed
)

figure_S2

