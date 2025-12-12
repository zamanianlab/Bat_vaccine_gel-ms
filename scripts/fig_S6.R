library(ggplot2)
library(dplyr)

# Mean CT values of three replicates from the stability assay
df <- data.frame(
  Timepoint = c("T=0", "23 °C; 1 mo", "23 °C; 3 mo", "4 °C; 1 mo", "4 °C; 3 mo"),
  Mean_CT = c(16.82333333, 17.65333333, 18.06, 17.71, 17.90333333),
  SE = c(0.641517299, 0.943774926, 1.130058996, 0.878806008, 1.013481349)
)

df$Timepoint <- factor(df$Timepoint, levels = df$Timepoint)

ggplot(df, aes(x = Timepoint, y = Mean_CT)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_CT - SE, ymax = Mean_CT + SE), width = 0.2, linewidth = 0.8) +
  labs(x = "CMC Storage Condition", y = "Mean CT Value") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold")
  )
