# Script for practicing PK/PD scripts

# Read data
df <- read.csv(“file.csv”)

# File structure and summary
str(df)
summary(df)


# Filter, mutate, select (dplyr)
library(dplyr)
df2 <- df %>%
  filter(group == “Treatment”) %>%
  mutate(bmi = weight / (height^2)) %>%
  select(subject_id, bmi)

# Descriptive Stats

# Mean, SD, IQR
mean(df$age, na.rm=TRUE)
sd(df$age, na.rm=TRUE)
IQR(df$age, na.rm=TRUE)

# Table of Counts
table(df$treatment)

#Cross-tab with proportions
prop.table(table(df$group, df$outcome), 1)


# Statistical Tests
# T-tests
t.test(value ~ group, data=df)

# ANOVA
aov_model <- aov(response ~ treatment, data=df)
summary(aov_model)

# chi-square test
chisq.test(table(df$sex, df$outcome))

# Linear regression
lm_model <- lm(response ~ age + sex, data=df)
summary(lm_model)

# Logistic regression
glm_model <- glm(outcome ~ age + sex, data=df, family=binomial)
summary(glm_model)

# Visualization
library(ggplot2)


# Line plot (e.g., concentration vs. time)
ggplot(df, aes(x=time, y=conc, color=subject_id)) +
  geom_line() +
  theme_minimal()

# Boxplot
ggplot(df, aes(x=group, y=conc)) +
  geom_boxplot() +
  theme_minimal()

# Histogram
ggplot(df, aes(x=conc)) +
  geom_histogram(binwidth=0.5) +
  theme_minimal()


# PK/PD Tools
# Noncompartmental PK analysis (PKNCA)
library(PKNCA)
conc_obj <- PKNCAconc(df, conc ~ time | subject)
dose_obj <- PKNCAdose(df, dose ~ time | subject)
pk_obj <- PKNCAdata(conc_obj, dose_obj)
results <- pk.nca(pk_obj)
summary(results)

# Nonlinear mixed effects modeling (nlme)
library(nlme)
model <- nlme(conc ~ SSfol(Dose, lKe, lKa, lCl),
              data=df,
              fixed = lKe + lKa + lCl ~ 1,
              random = lKe + lKa + lCl ~ 1 | subject)

# Data I/O
# Write CSV
write.csv(df, “output.csv”, row.names=FALSE)

# Save RDS (R-native object)
saveRDS(df, “df.rds”)
df <- readRDS(“df.rds”)


