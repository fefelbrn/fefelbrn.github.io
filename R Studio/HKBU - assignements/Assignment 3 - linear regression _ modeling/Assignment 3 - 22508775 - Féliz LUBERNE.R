# EX1.a

data1.a <- read.csv("C:/Fichiers/HKBU 2023/Cours Semestre 6/BUSI2045 - Data Analytics for Business Decision Making/Assignement/Assignment 3/NILT2012GR_SUBSET.csv")

Q1 <- subset(data1.a, !is.na(persinc2) & !is.na(rage))
Q1$log_income <- log(Q1$persinc2)

# log_income = β0 + β1 * rage + ε ... where:
  
# β0 is the intercept of the regression line
# β1 is the slope of the regression line, which represents the change in log income associated with a one-unit increase in age
# ε is the error term, which represents the variability in log income that is not explained by age.

# The equation implies that as the age of an individual increases by one unit, the log income increases by a constant amount (β1). The intercept β0 represents the expected value of log income when the age is zero. The error term ε captures the variation in log income that cannot be explained by age. The regression line can be used to predict the expected log income for a given age or to estimate the effect of age on log income.

# EX1.b

model <- lm(log_income ~ rage, data = Q1)

summary(model)

# The coefficient estimate for age suggests that there is a positive relationship between age and log_income, but the p-value associated with the coefficient is not statistically significant at the 0.05 level. This means that we cannot conclude that the relationship between age and log_income is significant.
# The adjusted R-squared value suggests that the model does not explain much of the variation in log_income, which means that there may be other factors that are more important in predicting personal income.

# Overall, the results suggest that age alone is not a strong predictor of personal income in the Q1 subset of the NILT2012GR dataset. Further analysis may be necessary to identify other factors that are more strongly related to personal income.

#EX1.c

library(ggplot2)

ggplot(Q1, aes(x = rage, y = log_income)) +
  geom_point() +
  stat_smooth(se=TRUE) +
  labs(x = "Age", y = "Log Income") +
  ggtitle("Relationship between Age and Log Income")
  theme(plot.title = element_text(hjust = 0.5))

# plot(model)

# EX1.d

Q1$ragesq <- Q1$rage^2
model <- lm(log_income ~ rage + ragesq, data = Q1)
summary(model)

# The multiple linear regression model includes both age (rage) and its squared term (ragesq) as predictors for log_income. The coefficients of both predictors are statistically significant with p-values less than 0.001. The intercept coefficient indicates that when age is 0, the expected log_income is 8.092. The coefficient for rage (0.0576) indicates that for a one-unit increase in age, the expected log_income increases by 0.0576 units while holding ragesq constant. The coefficient for ragesq (-0.000561) indicates that the quadratic relationship between age and log_income is negative. That is, the expected log_income first increases with age, but then decreases after a certain point (around 54.5 years old) where ragesq becomes negative. The adjusted R-squared value of the model is 0.0443, which means that only about 4.4% of the variation in log_income is explained by the predictors. The F-statistic is significant (p < 0.001), indicating that the overall model is significant in predicting log_income.

# EX1.e

#  In part (b), where only the linear term "rage" is included as a predictor of log income, the scatter plot shows a weak and non-linear relationship between the two variables. As the data points are scattered and do not follow a clear linear trend, it is not surprising that the coefficient of "rage" is not statistically significant. This suggests that a linear model may not be appropriate to describe the relationship between rage and log income.

# In contrast, in part (d), both the linear term "rage" and the quadratic term "ragesq" are included as predictors of log income. The scatter plot shows a clear U-shaped relationship between the two variables, suggesting that a quadratic model may be more appropriate to describe the relationship. The coefficients of both "rage" and "ragesq" are statistically significant, which indicates that the quadratic model provides a better fit to the data than the linear model. Specifically, the negative coefficient for "ragesq" suggests that the effect of "rage" on log income becomes weaker as "rage" increases beyond a certain point, consistent with the U-shaped relationship observed in the scatter plot.


# EX2.a

data2.a <- read.csv("C:/Fichiers/HKBU 2023/Cours Semestre 6/BUSI2045 - Data Analytics for Business Decision Making/Assignement/Assignment 3/real_estate_valuation.csv")

model_1 <- lm(house_price_of_unit_area ~ house_age, data = data2.a)
summary(model_1)

# The linear regression model indicates that house_age has a significant negative effect on house_price_of_unit_area. For every one-unit increase in house_age, the predicted house_price_of_unit_area decreases by $251.49. However, other variables not included in the model may also play a role in determining house_price_of_unit_area, as only 4.4% of the variation in house_price_of_unit_area can be explained by house_age alone.

# EX2.b

model_2 <- lm(house_price_of_unit_area ~ house_age + distance_to_MRT_station + num_of_convenience_store, data = data2.a)
summary(model_2)

# The multiple linear regression model (model_2) shows that all three predictor variables (house_age, distance_to_MRT_station, and num_of_convenience_store) have a statistically significant effect on the dependent variable, house_price_of_unit_area. The coefficients of the variables indicate that for every unit increase in house_age, house_price_of_unit_area decreases by approximately 0.27 units, for every unit increase in distance_to_MRT_station, house_price_of_unit_area decreases by approximately 0.01 units, and for every unit increase in num_of_convenience_store, house_price_of_unit_area increases by approximately 1.23 units. The adjusted R-squared value suggests that approximately 60% of the variance in house_price_of_unit_area can be explained by the three predictor variables in the model. The p-value for the F-statistic is less than 0.05, indicating that the overall model is statistically significant.

# EX2.c
library(coefplot)

coefplot(model_2,
         intercept = FALSE,
         outerCI=2,
         lwdOuter=1.5,
         ylab="House Features", 
         xlab="Impact on House Price",
         title = "Coefficient Plot of Model 2")

# EX2.d

anova(model_1, model_2)

# To compare the significance of the two models, we can use an ANOVA test by comparing the residual sums of squares (RSS) between the two models. The null hypothesis is that the simpler model (model_1) is sufficient to explain the variation in the response variable, while the alternative hypothesis is that the more complex model (model_2) is significantly better at explaining the variation in the response variable.
# From the ANOVA table, we see that the p-value of the F-test is less than 0.05, which indicates strong evidence against the null hypothesis. Therefore, we reject the null hypothesis and conclude that the more complex model (model_2) is significantly better at explaining the variation in the response variable than the simpler model (model_1).