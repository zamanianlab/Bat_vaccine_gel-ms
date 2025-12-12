library(ggplot2)
library(dplyr)

data <- data.frame(
  formulation = rep(c("GJ", "CMC"), each = 3),
  titer = c(8.13, 7.78, 8.16, 7.90, 7.45, 7.57) #Log10 viral titer at T=0 of each replicate
)

# Compute summary stats
summary_data <- data %>%
  group_by(formulation) %>%
  summarise(
    mean = mean(titer),
    sd = sd(titer),
    se = sd / sqrt(n())
  )

# Plot
p1 <- ggplot(summary_data, aes(x = formulation, y = mean)) +
  geom_jitter(data = data, aes(x = formulation, y = titer), width = 0.1, size = 3, alpha = 0.7, color = "red") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 1) + 
  geom_point(aes(y = mean), color = "black", size = 4) + 
  theme_minimal() +
  labs(x = "", y = "Viral Titer (Log10 PFU/mL)") +
  theme(text = element_text(size = 14, face = "bold")) +
  ylim(7, 8.5)

################################################################################
################################################################################
#### Assay 2 ###################################################################


data <- data.frame(
  formulation = rep(c("C", "R", "S"), each = 3),
  titer = c(7.53, 7.61, 7.36,
            7.41, 7.22, 7.15, 
            7.69, 7.53, 7.24) #Log10 viral titer at T=0 of each replicate
)

# Compute summary stats
summary_data <- data %>%
  group_by(formulation) %>%
  summarise(
    mean = mean(titer),
    sd = sd(titer),
    se = sd / sqrt(n())
  )

# Plot
p2 <- ggplot(summary_data, aes(x = formulation, y = mean)) +
  geom_jitter(data = data, aes(x = formulation, y = titer), width = 0.1, size = 3, alpha = 0.7, color = "red") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 1) + 
  geom_point(aes(y = mean), color = "black", size = 4) + 
  theme_minimal() +
  labs(x = "", y = "Viral Titer (Log10 PFU/mL)") +
  theme(text = element_text(size = 14, face = "bold")) +
  ylim(7, 8.0)


################################################################################

library(cowplot)
library(grid)

# Spacer grob (fixed-width blank column)
spacer <- ggdraw() + theme_void()

# Column 1 = A over C
col1 <- plot_grid(
  NULL,
  p1, 
  ncol = 1,
  rel_heights = c(0.05, 1),
  align = "v",
  axis = "l",
  labels = c("a"),
  label_size = 18,
  label_fontface = "bold"
)

# Column 2 = B over D
col2 <- plot_grid(
  NULL,
  p2, 
  ncol = 1,
  rel_heights = c(0.05, 1),
  align = "v",
  axis = "l",
  labels = c("b"),
  label_size = 18,
  label_fontface = "bold"
)


# Combine with a spacer column in the middle
figure_S4 <- plot_grid(
  col1,
  spacer,
  col2,
  nrow = 1,
  rel_widths = c(1, 0.1, 1)   # adjust spacer width as needed
)

figure_S4


