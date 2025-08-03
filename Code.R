library("dplyr")
library("ggplot2")
library("tableone")
library("readr")

df <- read.csv("hosp_admn.csv")

# OOP Spend distribution (log scale)
ggplot(df, aes(x = oop_spend)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of Out-of-Pocket Spending",
       x = "OOP Spend (log scale)", y = "Count")

# Labour earnings distribution (log scale)
ggplot(df, aes(x = riearnsemp)) +
  geom_histogram(bins = 50, fill = "darkgreen", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of Labour Earnings",
       x = "Earnings (log scale)", y = "Count")

# OOP Spend distribution (non - log scale)
ggplot(df, aes(x = oop_spend)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Out-of-Pocket Spending",
       x = "OOP Spend (log scale)", y = "Count")

# Labour earnings distribution (non - log scale)
ggplot(df, aes(x = riearnsemp)) +
  geom_histogram(bins = 50, fill = "darkgreen", color = "black") +
  labs(title = "Distribution of Labour Earnings",
       x = "Earnings (log scale)", y = "Count")

# Variables to compare
covariates <- c("age_hosp", "male", "married", "white", "black", "hispanic",
                "medicaid", "medicare", "insured_pv",
                "lessthan_hs", "hs_grad", "some_college", "collegeplus",
                "working_ft", "retired", "unemployed")

# Create balance table
df$ever_hosp <- as.factor(df$ever_hosp)
balance_table <- CreateTableOne(vars = covariates,
                                strata = "ever_hosp",
                                data = df,
                                factorVars = covariates)
print(balance_table, showAllLevels = TRUE)

# Total number of unique individuals
n_distinct(df$hhidpn)

# How many waves each person is observed in
df %>%
  group_by(hhidpn) %>%
  summarise(num_waves = n()) %>%
  count(num_waves) %>%
  arrange(desc(n))

# Calculate percentage of missing values in each column
missing_summary <- sapply(df, function(x) mean(is.na(x))) %>%
  sort(decreasing = TRUE)

# View top missing columns
print(round(missing_summary[missing_summary > 0], 3))

# Create treatment group: those hospitalised before or at wave 3
df$treat_group <- ifelse(df$wave_hosp <= 3, 1, 0)

# Create a post-treatment indicator for each individual
df$post <- with(df, ifelse(wave >= wave_hosp & treat_group == 1, 1, 0))

# Sanity check
table(df$treat_group, df$post)

# Average outcome by wave and treatment
avg_trend <- df %>%
  group_by(wave, treat_group) %>%
  summarise(mean_earn = mean(riearnsemp, na.rm = TRUE),
            mean_oop = mean(oop_spend, na.rm = TRUE))

# Plot
ggplot(avg_trend, aes(x = wave, y = mean_earn, color = factor(treat_group))) +
  geom_line() +
  labs(title = "Labour Earnings: Treated vs Control", color = "Treated")

# Pick one pre-treatment wave (e.g. wave 1)
pre_wave <- df %>% filter(wave == 1)

# Compare averages by treatment group
pre_wave %>%
  group_by(treat_group) %>%
  summarise(across(c(age_hosp, male, white, medicaid, lessthan_hs),
                   mean, na.rm = TRUE))
