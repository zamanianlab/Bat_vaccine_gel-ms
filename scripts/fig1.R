library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(tibble)
library(cowplot)
library(tidyr)

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

plot_material_pairs <- function(df, pairs,
                                rate_range = NULL,
                                log_scale = TRUE) {
  
  # Ensure tibble format
  pairs_tbl <- as_tibble(pairs)
  
  # Summarize each pair
  summary_all <- pairs_tbl %>%
    rowwise() %>%
    do({
      m <- .$material
      t <- .$temp
      c <- .$color   # ADD THIS
      
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
        mutate(series = paste(m, t, "°C"),
               color  = c)   # USE THIS
      
      df_filtered
    }) %>%
    bind_rows()
  
  
  # Apply rate filtering if requested
  if (!is.null(rate_range)) {
    summary_all <- summary_all %>%
      filter(rate >= rate_range[1], rate <= rate_range[2])
  }
  
  # Build color mapping directly from tibble
  color_map <- summary_all %>%
    distinct(series, color) %>%
    deframe()
  
  # Errorbar width logic
  err_width <- ifelse(is.null(rate_range), 0.05, 3)
  
  # Build plot
  p <- ggdraw(
    ggplot(summary_all, aes(x = rate, y = mean_eta, color = series)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2.5) +
      geom_errorbar(aes(ymin = mean_eta - se_eta,
                        ymax = mean_eta + se_eta),
                    width = err_width,
                    linewidth = 0.8) +
      scale_color_manual(values = color_map) +
      { if (log_scale) scale_x_log10() else scale_x_continuous() } +
      labs(x = "Shear Rate (1/s)", y = "Viscosity (Pa·s)") +
      theme_minimal() +
      theme(
        axis.title = element_text(face = "bold", size = 14),
        axis.text  = element_text(face = "bold", size = 12),
        axis.line  = element_line(color = "black"),
        legend.position = c(0.7, 0.7),#"none"
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12)
      )
  )
  
  return(p)
}

p1_pairs <- tibble(
  material = c("10% CMC", "12.5% CMC", "15% CMC"),
  temp     = c(40, 40, 40),
  color   = c("gold", "salmon", "darkgreen")
)

p2_pairs <- tibble(
  material = c("12.5% CMC", "12.5% CMC", "12.5% CMC", "GJ", "GJ", "GJ"),
  temp     = c(0, 20, 40, 0, 20, 40),
  color    = c("darkred", "red", "salmon", "darkblue", "blue", "turquoise")
)

p3_pairs <- tibble(
  material = c("12.5% CMC", "12.5% CMC", "12.5% CMC", "GJ", "GJ", "GJ"),
  temp     = c(0, 20, 40, 0, 20, 40),
  color    = c("darkred", "red", "salmon", "darkblue", "blue", "turquoise")
)

p1 <- plot_material_pairs(df, p1_pairs)
p2 <- plot_material_pairs(df, p2_pairs)
p3 <- plot_material_pairs(df, p3_pairs, rate_range = c(20, 200), log_scale = FALSE)
  
################################################################################


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

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(purrr)

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(purrr)

plot_strain_sweep_pairs <- function(df, pairs, log_scale = TRUE) {
  
  # Ensure tibble format
  pairs_tbl <- as_tibble(pairs)
  
  # Summarize each material-temp pair across replicates
  summary_all <- purrr::pmap_dfr(
    pairs_tbl,
    function(material, temp, color_gprime, color_gdoubleprime) {
      
      df_pair <- df %>%
        filter(material == !!material, temp == !!temp) %>%
        arrange(replicate, strain) %>%
        group_by(replicate) %>%
        mutate(row = row_number()) %>%
        ungroup() %>%
        group_by(row) %>%
        reframe(
          mean_strain = mean(strain, na.rm = TRUE),
          mean_g      = mean(g, na.rm = TRUE),
          se_g        = sd(g, na.rm = TRUE)/sqrt(n()),
          mean_g2     = mean(g_2, na.rm = TRUE),
          se_g2       = sd(g_2, na.rm = TRUE)/sqrt(n()),
          material = material,
          temp = temp,
          color_gprime = color_gprime,
          color_gdoubleprime = color_gdoubleprime
        )
      
      df_pair
    }
  )
  
  # Combine G' and G'' into long-format for plotting
  plot_df <- bind_rows(
    summary_all %>% transmute(
      mean_strain,
      mean = mean_g,
      se = se_g,
      series_mod = paste(material, temp, "°C G'"),
      color = color_gprime
    ),
    summary_all %>% transmute(
      mean_strain,
      mean = mean_g2,
      se = se_g2,
      series_mod = paste(material, temp, "°C G''"),
      color = color_gdoubleprime
    )
  )
  
  # Make unique color mapping
  color_map <- setNames(plot_df$color, plot_df$series_mod)
  
  # Build ggplot
  p <- ggdraw(
    ggplot(plot_df, aes(x = mean_strain, y = mean, color = series_mod)) +
      geom_line(aes(group = series_mod), linewidth = 0.8) +
      geom_point(aes(group = series_mod), size = 2.5) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    width = 0.05, linewidth = 0.8) +
      scale_color_manual(values = color_map) +
      { if (log_scale) scale_x_log10() else scale_x_continuous() } +
      labs(x = "Strain %", y = "G' and G'' (Pa)", color = " ") +
      theme_minimal() +
      theme(
        axis.title = element_text(face = "bold", size = 14),
        axis.text  = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.line = element_line(color = "black"),
        legend.position = c(0.5, 0.25), #"none",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size =12),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x  = element_text(face = "bold", size = 12),
        axis.text.y  = element_text(face = "bold", size = 12)
      )
  )
  
  return(p)
}

p4_pairs <- tibble(
  material = c("GJ", "12.5% CMC"),
  temp     = c(40, 40),
  color_gprime       = c("darkblue", "darkred"),
  color_gdoubleprime = c("blue", "red")
)

p4 <- plot_strain_sweep_pairs(df, p4_pairs)

# Spacer grob
spacer <- ggdraw() + theme_void()

# Column 1 = a over c
col1 <- plot_grid(
  spacer,  # label row above p1
  p1, 
  spacer,  # spacer between top and bottom plots
  p3,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.1, 1),
  align = "v",
  axis = "l",
  labels = c("a", "", "c"),
  label_size = 18,
  label_fontface = "bold"
)

# Column 2 = b over d
col2 <- plot_grid(
  spacer, 
  p2, 
  spacer,
  p4,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.1, 1),
  align = "v",
  axis = "l",
  labels = c("b", "", "d"),
  label_size = 18,
  label_fontface = "bold",
  label_x = -0.075  # nudge labels slightly left if needed
)

# Combine the two columns with a spacer in between
final_figure <- plot_grid(
  col1,
  spacer,
  col2,
  nrow = 1,
  rel_widths = c(1, 0.1, 1)
)

final_figure
