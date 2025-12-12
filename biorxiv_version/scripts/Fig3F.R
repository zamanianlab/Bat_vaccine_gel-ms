folder_path <- "RFID data"
files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
all_dfs <- list()

for (file in files) {
  lines <- readLines(file)
  
  # Filter only lines that start with TAG:
  lines <- gsub('^"|"$', '', lines)
  tag_lines <- grep("^TAG:", lines, value = TRUE)
  
  split_lines <- strsplit(tag_lines, " ")
  
  # Convert each line to a data frame row
  df <- do.call(rbind, lapply(split_lines, function(x) {
    data.frame(
      Value = x[2],
      Date = x[3],
      Time = x[4],
      TagID = x[5],
      SourceFile = basename(file),
      stringsAsFactors = FALSE
    )
  }))
  
  all_dfs[[length(all_dfs) + 1]] <- df
}


# Combine all data frames into one "mega file"
combined_df <- do.call(rbind, all_dfs)


# Count total number of unique IDs
total_unique_ids <- length(unique(combined_df$TagID))
print(paste("Total unique IDs detected:", total_unique_ids))

#### Figure 3F ##################################################################

library(dplyr)
library(lubridate)

#Combine date and time into a POSIXct datetime
combined_df$DateTime <- as.POSIXct(paste(combined_df$Date, combined_df$Time), format="%m/%d/%Y %H:%M:%OS")

# Assign tag readings to a "Night" 
combined_df <- combined_df %>%
  mutate(
    Hour = hour(DateTime),
    Night = case_when(
      Hour >= 17 ~ as.Date(DateTime),                 # 6pm to midnight → same day
      Hour < 10  ~ as.Date(DateTime) - days(1),        # midnight to 8am → previous day
      TRUE ~ NA_Date_
    )
  )

# Filter only entries between 6pm and 8am 
night_df <- combined_df %>%
  filter(Hour >= 17 | Hour < 10)

# This is a message for start/stop of read times for the RFID
night_df <- filter(night_df, TagID != "999.000000007425") 

# This is a test tag
night_df <- filter(night_df, TagID != "989.001040476348") 


# Summarize number of unique RFID tags per night
summary_df <- night_df %>%
  group_by(Night) %>%
  summarise(UniqueTagCount = n_distinct(TagID)) %>%
  arrange(Night)

ggplot(summary_df, aes(x = Night, y = UniqueTagCount)) +
  geom_line(color = "#1D91C0", size = 1) +
  geom_point(color = "#0C2C84", size = 2) +
  
  geom_rect(aes(xmin = as.Date("2024-11-02") + 0.2, xmax = as.Date("2024-11-03") - 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.05, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-11-06") + 0.2, xmax = as.Date("2024-11-07") - 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.05, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2024-10-30") + 0.2, xmax = as.Date("2024-10-31") - 0.2,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.05, inherit.aes = FALSE) +
  
  annotate("text", x = as.Date("2024-11-02") - 0.4, y = 40,
           label = "Capture 2", hjust = 0, size = 5, fontface = "bold") +
  annotate("text", x = as.Date("2024-11-06") - 0.4, y = 40,
           label = "Capture 3", hjust = 0, size = 5, fontface = "bold") +
  annotate("text", x = as.Date("2024-10-30") - 0.4, y = 40,
           label = "Capture 1", hjust = 0, size = 5, fontface = "bold") +
  
  labs(
    title = " ",
    x = "Night",
    y = "Unique Microchipped Bats"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14, hjust = 1, angle = 45),
    axis.text.y = element_text(face = "bold", size = 14)
  )