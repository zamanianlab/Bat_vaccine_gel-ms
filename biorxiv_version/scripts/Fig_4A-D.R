library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)

# Load data
df <- read_csv("Bat_capture_data.csv", skip = 1)

# Clean column names
colnames(df) <- c("Date", "RFID", "ArmBand", "UniqueBat", "PreviousRFID", 
                  "CaptureType", "Sex", "Age", "GelApplied", "HairSample")

# Standardize date format
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

# Reformat df with each row representing a unique bat
summary_df <- df %>%
  group_by(UniqueBat, Sex, Age) %>%
  summarise(
    `10/31 Capture` = ifelse(any(Date == as.Date("2024-10-31")), 1, 0),
    `10/31 Treatment` = ifelse(any(Date == as.Date("2024-10-31") & GelApplied == 1), 1, 0),
    `11/3 Capture` = ifelse(any(Date == as.Date("2024-11-03")), 1, 0),
    `11/7 Capture` = ifelse(any(Date == as.Date("2024-11-07")), 1, 0),
    `11/3 Hair` = HairSample[Date == as.Date("2024-11-03")][1],
    `11/7 Hair` = HairSample[Date == as.Date("2024-11-07")][1],
    .groups = "drop"
  ) %>%
  mutate(AgeGroup = ifelse(Age %in% c("SA", "J"), "J", Age)) %>%
  filter(!is.na(`11/3 Hair`) | !is.na(`11/7 Hair`))

# Change format for plotting
long_df <- summary_df %>%
  select(UniqueBat, Sex, AgeGroup, 
         `10/31 Capture`, `10/31 Treatment`, 
         `11/3 Capture`, `11/3 Hair`,
         `11/7 Capture`, `11/7 Hair`) %>%
  pivot_longer(
    cols = c(`10/31 Capture`, `10/31 Treatment`, `11/3 Capture`, `11/3 Hair`, `11/7 Capture`, `11/7 Hair`),
    names_to = c("Date", "Measure"),
    names_sep = " ",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  mutate(
    Date = case_when(
      Date == "10/31" ~ as.Date("2024-10-31"),
      Date == "11/3" ~ as.Date("2024-11-03"),
      Date == "11/7" ~ as.Date("2024-11-07")
    ),
    Capture = ifelse(is.na(Capture), 0, Capture),  # just in case
    PointType = case_when(
      Date == as.Date("2024-10-31") & Capture == 1 & Treatment == 1 ~ "treated",
      Date == as.Date("2024-10-31") & Capture == 1 ~ "captured_only",
      Date %in% c(as.Date("2024-11-03"), as.Date("2024-11-07")) & Capture == 1 & Hair == 1 ~ "rb_pos",
      Date %in% c(as.Date("2024-11-03"), as.Date("2024-11-07")) & Capture == 1 & Hair == 0 ~ "rb_neg",
      Date %in% c(as.Date("2024-11-03"), as.Date("2024-11-07")) & Capture == 1 & Hair == 2 ~ "not_collected",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(PointType))


shapes <- c(
  captured_only = 1,
  treated = 15,
  rb_pos = 16,
  rb_neg = 16,
  not_collected = 17
)
colors <- c(
  captured_only = "black",
  treated = "orange",
  rb_pos = "green",
  rb_neg = "red",
  not_collected = "gray"
)

long_df <- long_df %>%
  mutate(
    Shape = shapes[PointType],
    Color = colors[PointType]
  )

# Factor for age and sex
long_df$Sex <- factor(long_df$Sex, levels = c("M", "F"))
long_df$AgeGroup <- factor(long_df$AgeGroup, levels = c("A", "J"))

# Separate by cohort
AM <- filter(long_df, AgeGroup == "A", Sex == "M")
AF <- filter(long_df, AgeGroup == "A", Sex == "F")
JM <- filter(long_df, AgeGroup == "J", Sex == "M")
JF <- filter(long_df, AgeGroup == "J", Sex == "F")

plot_bats <- function(df, title) {
  points <- unique(df$PointType)
  ggplot(df, aes(x = Date, y = factor(UniqueBat))) +
    geom_point(aes(color = PointType, shape = PointType), size = 4) +  # <-- color mapped to PointType
    geom_line(aes(group = UniqueBat), color = "black", linetype = "dashed") +
    scale_shape_manual(values = shapes[points]) +
    scale_color_manual(values = colors[points]) +
    scale_x_date(
      breaks = as.Date(c("2024-10-31", "2024-11-03", "2024-11-07")),
      labels = c("Capture 1", "Capture 2", "Capture 3")
    ) +
    labs(title = title, x = NULL, y = "Bat ID") +
    theme_minimal() +
    theme(
      title = element_text(face = "bold", size = 16),
      legend.position = "none",
      axis.title.x = element_text(face = "bold", size = 18),
      axis.title.y = element_text(face = "bold", size = 18),
      axis.text.x = element_text(face = "bold", size = 14, angle = 45, hjust = 1),
      # axis.text.y = element_text(face = "bold", size = 11),
      axis.text.y = element_blank()
    )
}

# Generate plots
p1 <- plot_bats(AM, "Adult Males")
p2 <- plot_bats(AF, "Adult Females")
p3 <- plot_bats(JM, "Juvenile Males")
p4 <- plot_bats(JF, "Juvenile Females")

(p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(15, 15, 15, 15))


