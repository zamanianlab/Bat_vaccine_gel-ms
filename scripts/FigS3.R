library(ggplot2)
library(dplyr)

data <- data.frame(
  formulation = rep(c("Glycerin Jelly", "CMC"), each = 3),
  titer = c(8.13, 7.78, 8.16, 7.90, 7.45, 7.57) #Log10 viral titer at T=0 of each replicate
)

summary_data <- data %>%
  group_by(formulation) %>%
  summarise(
    mean = mean(titer),
    sd = sd(titer),
    se = sd / sqrt(n())
  )

ggplot(summary_data, aes(x = formulation, y = mean)) +
  geom_jitter(data = data, aes(x = formulation, y = titer), width = 0.1, size = 3, alpha = 0.7, color = "red") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 1) + 
  geom_point(aes(y = mean), color = "black", size = 4) + 
  theme_minimal() +
  labs(x = "", y = "Viral Titer (Log10 PFU/mL)") +
  theme(text = element_text(size = 20, face = "bold")) +
  ylim(7, 8.5)
