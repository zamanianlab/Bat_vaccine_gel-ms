library(ggplot2)
library(dplyr)
library(tidyr)

data <- data.frame(
  Date = rep(c("Oct 31", "Nov 3", "Nov 7"), each = 4),
  Group = rep(c("Adult Females", "Adult Males", "Juvenile Females", "Juvenile Males"), times = 3),
  Total = c(38, 23, 17, 11, 
            9, 12, 10, 7, 
            5, 10, 7, 7),
  Recaptured = c(26, 17, 3, 1, 
                 8, 10, 10, 5, 
                 2, 4, 5, 5)
)

data$Date <- factor(data$Date, levels = c("Oct 31", "Nov 3", "Nov 7"))

fill_colors <- c(
  "Adult Females"    = "#1F78B4",
  "Adult Males"      = "#33A02C",
  "Juvenile Females" = "#E31A1C",
  "Juvenile Males"   = "#6A3D9A"
)

ggplot(data, aes(x = Date, y = Total, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.7) +
  geom_point(aes(y = Recaptured, group = Group, shape = "Recaptured"), 
             position = position_dodge(width = 0.75), 
             size = 10, fill = "white", stroke = 5, color = "black") +
  scale_shape_manual(name = "", values = c("Recaptured" = "_")) + 
  theme_minimal() +
  ylim(0, 40) +
  scale_fill_manual(values = fill_colors) +
  labs(x = "Capture Date (2024)", y = "Number of Bats", fill = "Subpopulation") +
  theme(text = element_text(size = 16, face = "bold")
  )

