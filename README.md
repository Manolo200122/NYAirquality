# NYAirquality
ANOVA and hypothesis testing model techniques on the built in Rstudio data set "airquality"
airquality
# Load dataset
data(airquality)

# One-way ANOVA for ozone across months
anova_result <- aov(Ozone ~ factor(Month), data = airquality)
summary(anova_result)

# Load required packages
library(ggplot2)

# Create boxplots with explicit handling of missing values
ggplot(airquality, aes(x = factor(Month), y = Ozone, fill = factor(Month))) +
  geom_boxplot(na.rm = TRUE) +  # Explicitly remove NA values
  labs(
    title = "Ozone Levels by Month (ANOVA Results)",
    x = "Month",
    y = "Ozone (ppb)",
    caption = "37 missing values excluded"
  ) +
  scale_fill_discrete(name = "Month") +
  theme_minimal()

#Enhanced Version with Annotations
# Add ANOVA p-value to plot
library(ggpubr)

ggplot(airquality, aes(x = factor(Month), y = Ozone)) +
  geom_boxplot(na.rm = TRUE, fill = "steelblue") +
  stat_compare_means(
    method = "anova", 
    label.x = 1.5,
    label.y = 160  # Adjust position
  ) +
  labs(
    title = "Ozone Distribution by Month (p < 0.001)",
    x = "Month",
    y = "Ozone (ppb)"
  ) +
  theme_classic()

air_clean <- na.omit(airquality)  # Explicitly remove NAs upfront
# Shapiro-Wilk test for normality
shapiro.test(air_clean$Ozone)

# Levene's test for equal variance
car::leveneTest(Ozone ~ factor(Month), data = air_clean)

# Tukey HSD results
tukey_results <- TukeyHSD(anova_result)
plot(tukey_results)

# Load data and handle missing values
data(airquality)
air_clean <- na.omit(airquality)  # Remove 37 NA rows upfront

# ANOVA with explicit factor conversion
anova_result <- aov(Ozone ~ factor(Month), data = air_clean)
summary(anova_result)

kruskal.test(Ozone ~ factor(Month), data = air_clean)

TukeyHSD(anova_result)
plot(TukeyHSD(anova_result), las=1)

t.test(airquality$Wind, mu=9)

t.test(airquality$Solar.R, mu=175, alternative="greater")

ggplot(air_clean, aes(factor(Month), Ozone)) + 
  geom_boxplot() + 
  labs(title="Ozone Levels by Month (ANOVA Results)")
