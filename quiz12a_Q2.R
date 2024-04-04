# Load required library
install.packages("ggplot2")
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Simulate the dataset
lambda <- 50
data <- data.frame(
  year = rep(2002:2021, times = 5),
  hospital = rep(1:5, each = 20),
  deaths = rpois(100, lambda)
)

# Add some predictors to the dataset
data$avg_age = rnorm(100, mean = 60, sd = 10)
data$smokers_percent = rnorm(100, mean = 20, sd = 5)
data$treatment_success_rate = rnorm(100, mean = 80, sd = 10)

# 10 tests on the simulated data
# Test 1: Mean number of deaths across all years
mean_deaths <- mean(data$deaths)

# Test 2: Variance of number of deaths
var_deaths <- var(data$deaths)

# Test 3: Test for overdispersion in Poisson data
test_statistic <- (var_deaths - mean_deaths) / sqrt(2 * mean_deaths / length(data$deaths))
p_value_overdispersion <- 2 * (1 - pnorm(abs(test_statistic)))

# Test 4: ANOVA to test if there's a significant difference in deaths by hospital
anova_test <- aov(deaths ~ as.factor(hospital), data = data)

# Test 5: Correlation between average age and number of deaths
cor_test_age_deaths <- cor.test(data$deaths, data$avg_age)

# Test 6: Correlation between smokers percentage and number of deaths
cor_test_smokers_deaths <- cor.test(data$deaths, data$smokers_percent)

# Test 7: Correlation between treatment success rate and number of deaths
cor_test_treatment_deaths <- cor.test(data$deaths, data$treatment_success_rate)

# Test 8: Linear regression to predict deaths using avg_age
lm_avg_age <- lm(deaths ~ avg_age, data = data)

# Test 9: Linear regression to predict deaths using smokers_percent
lm_smokers_percent <- lm(deaths ~ smokers_percent, data = data)

# Test 10: Linear regression to predict deaths using treatment_success_rate
lm_treatment_success_rate <- lm(deaths ~ treatment_success_rate, data = data)

# Bar plot of the simulated data
ggplot(data, aes(x = year, y = deaths, fill = factor(hospital))) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Cancer Deaths per Year in Sydney Hospitals", x = "Year", y = "Total Number of Cancer Deaths") + 
  theme_minimal()

# Fit a generalized linear model
model <- glm(deaths ~ year + factor(hospital) + avg_age + smokers_percent + treatment_success_rate, 
             data = data, family = poisson(link = "log"))
summary(model)

# Print p-values from the tests
list(
  p_value_overdispersion = p_value_overdispersion,
  anova_p_value = summary(anova_test)$'Pr(>F)'[1],
  cor_test_age_p_value = cor_test_age_deaths$p.value,
  cor_test_smokers_p_value = cor_test_smokers_deaths$p.value,
  cor_test_treatment_p_value = cor_test_treatment_deaths$p.value,
  lm_avg_age_p_value = summary(lm_avg_age)$coefficients[2,4],
  lm_smokers_percent_p_value = summary(lm_smokers_percent)$coefficients[2,4],
  lm_treatment_success_rate_p_value = summary(lm_treatment_success_rate)$coefficients[2,4]
)

