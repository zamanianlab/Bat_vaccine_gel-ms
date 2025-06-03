#### Build model ###############################################################
library(readr)
library(brms)

df <- data.frame(
  cohort = factor(c("AM", "AF", "JM", "JF"), levels = c("JF", "AM", "JM", "AF")),
  age = factor(c("A", "A", "J", "J"), levels = c("A", "J")),
  sex = factor(c("M", "F", "M", "F"), levels = c("M", "F")),
  positive = as.integer(c(5, 11, 9, 4)), # positive hair samples
  total = as.integer(c(11, 11, 9, 4)) # total hair samples collected
)

prior <- c(
  prior(normal(0, 2), class = "b", coef = "cohortAF"),
  prior(normal(0, 2), class = "b", coef = "cohortAM"),
  #prior(normal(0, 2), class = "b", coef = "cohortJF"),
  prior(normal(0, 2), class = "b", coef = "cohortJM")
)

fit1 <- brm(positive | trials(total) ~ cohort, family = "binomial", data = df, iter = 8000, warmup = 2000, prior = prior)

hypothesis(fit1, "cohortAM = 0")
hypothesis(fit1, "cohortAM - cohortAF = 0")
hypothesis(fit1, "cohortAM - cohortJM = 0")
hypothesis(fit1, "cohortJM - cohortAF = 0")
hypothesis(fit1, "cohortJM = 0")
hypothesis(fit1, "cohortAF = 0")

df$predicted <- predict(fit1, type = "response")

#### Simulate spread in uncaptured bats ########################################
# library(truncnorm)

known_bats <- c(AF = 40, AM = 26, JF = 17, JM = 12) 
cohort_probs <- prop.table(known_bats)

n_known <- sum(known_bats)
# n_est <- round(rtruncnorm(1, a = 95, b = Inf, mean = 117.4, sd = 31.64)) # Estimated colony size with simulation
n_est <- 117.4  # Fixed estimated colony size

n_unknown <- round(n_est - n_known) # Number of unknown bats

# sim_cohorts <- sample(names(cohort_probs), size = n_unknown, replace = TRUE, prob = cohort_probs) # Assign age/sex to simulated bats

known_df <- data.frame(
  batID = seq_len(n_known),
  cohort = rep(names(known_bats), times = known_bats),
  Source = "Known"
)

unknown_bats <- round(cohort_probs * n_unknown)

unknown_df <- data.frame(
  batID = (n_known + 1):(n_known + n_unknown),
  cohort = rep(names(unknown_bats), times = unknown_bats),
  Source = "Unknown"
)

full_colony <- rbind(known_df, unknown_df)

# Create summary table
tab <- addmargins(table(full_colony$cohort, full_colony$Source))
tab2 <- tab[rownames(tab) != "Sum", "Sum"]

colony <- data.frame(
  cohort = c("AM", "JM", "AF", "JF"),
  total_Bats = c(tab2["AM"], tab2["JM"], tab2["AF"], tab2["JF"]),
  Treated = c(9, 2, 7, 6),
  Hair_pos = c(5, 9, 11, 4), 
  Hair_neg = c(6, 0, 0, 0)
)

# Calculate number of bats with unknown hair sample status
colony$Remaining <- colony$total_Bats - colony$Treated - colony$Hair_pos - colony$Hair_neg

#### Apply model to remaining bats #############################################

# Use brm model to predict spread in bats that were not treated and have no hair sample
predict_spread <- predict(fit1, newdata = transform(colony, total = Remaining), summary = TRUE)

colony$Spread.Estimate <- predict_spread[, "Estimate"]
colony$Spread.Q2.5 <- predict_spread[, "Q2.5"]
colony$Spread.Q97.5 <- predict_spread[, "Q97.5"]

# total treated bats is the sum of topically treated, positive hair samples and predicted spread
colony$total_treated.Estimate <- colony$Treated + colony$Hair_pos + colony$Spread.Estimate

colony$total_treated.Q2.5 <- colony$Treated + colony$Hair_pos + colony$Spread.Q2.5
colony$total_treated.Q97.5 <- colony$Treated + colony$Hair_pos + colony$Spread.Q97.5

colony$Category <- factor(c("Adult Male", "Juvenile Male", "Adult Female", "Juvenile Female"))

#### Figure 4G #################################################################

library(ggplot2)
library(reshape2)

colony_long <- melt(
  colony[, c("Category", "total_Bats", "total_treated.Estimate")],
  id.vars = "Category",
  variable.name = "Group",
  value.name = "Count"
)

treated_df <- data.frame(
  Category = colony$Category,
  Group = "Treated",
  Count = colony$Treated
)

colony_long <- rbind(colony_long, treated_df)

colony_long$Group <- factor(colony_long$Group,
                            levels = c("total_Bats", "Treated", "total_treated.Estimate"),
                            labels = c("total", "Treated", "total_Treated"))

ggplot() +
  geom_bar(data = subset(colony_long, Group == "total"),
           aes(x = Category, y = Count, fill = Group),
           stat = "identity", position = "identity", width = 0.7,
           color = "black", linetype = "dashed", linewidth = 1) +
  
  geom_bar(data = subset(colony_long, Group == "total_Treated"),
           aes(x = Category, y = Count, fill = Group), alpha = 0.7,
           stat = "identity", position = "identity", width = 0.7) +
  
  geom_errorbar(data = colony,
                aes(x = Category,
                    ymin = total_treated.Q2.5,
                    ymax = total_treated.Q97.5),
                width = 0.3, color = "red", linetype = "dashed", linewidth = 1) +
  
  geom_bar(data = subset(colony_long, Group == "Treated"),
           aes(x = Category, y = Count, fill = Group),
           stat = "identity", position = "stack", width = 0.7, color = "black") +
  ylim(0, 50) +
  scale_fill_manual(
    values = c("total" = "transparent", "total_Treated" = "gray", "Treated" = "black"),
    labels = c("Estimate within Colony", "Estimated Coverage", "Topically Treated"),
    name = "",
    guide = guide_legend()
  ) +
  theme_minimal() +
  labs(title = "", y = "Number of Bats", x = "Cohort") +
  theme(
    legend.position = c(1, 1.1),
    legend.justification = c(1, 1.1),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(face = "bold", size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 14)
  )
