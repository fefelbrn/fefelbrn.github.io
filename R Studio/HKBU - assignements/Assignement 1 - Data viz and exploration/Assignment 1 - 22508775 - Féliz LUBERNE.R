# EX1.1

library(ggplot2)
data1 <- read.csv("brand_ratings.csv")

num_bars <- 5
ggplot(data1, aes(x = perform)) + 
  geom_histogram(binwidth = 1) +
  labs(x = "perform", y = "count", title = "Histogram of Variable Perform") +
  theme_bw() +
  facet_wrap(~ brand, ncol = 4) +
  scale_x_continuous(breaks = seq(0, 10, by = 3), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))

# Try 1.1

data <- read.csv("brand_ratings.csv")

ggplot(data, aes(x = perform)) + 
  geom_histogram(binwidth = 1, alpha = 0.8, position = "identity") +
  labs(x = "Perform", y = "Count", title = "Histogram of Variable Perform") +
  theme_bw() + facet_wrap(~ brand, ncol = 4)

# EX1.2

library(ggplot2)
library(foreign)
churn <- read.arff("churn.arff")

ggplot(churn, aes(x = REPORTED_SATISFACTION, fill = COLLEGE)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ LEAVE) +
  labs(title = "Bar Chart of Variable Reported_Satisfaction",
       x = "REPORTED_SATISFACTION",
       y = "Count",
       fill = "College") +
  theme_minimal()

# EX2
# A) 

Q2_data <- read.csv("Assignment1_Q2.csv", header = TRUE)
Q2_data <- Q2_data[complete.cases(Q2_data$X3), ]

n_obs <- dim(Q2_data)[1]
cat("Number of observations in the data set: ", n_obs, "\n")

str(Q2_data)

# B)
sapply(Q2_data, class)
X1          X2          X3 

unique(Q2_data$X1)
unique(Q2_data$X2)
unique(Q2_data$X3)
unique(Q2_data$X4)
unique(Q2_data$X5)

summary(Q2_data$X3)
summary(Q2_data$X4)
summary(Q2_data$X5)


sd(Q2_data$X3)
range(Q2_data$X3)

sd(Q2_data$X4)
range(Q2_data$X4)

sd(Q2_data$X5)
range(Q2_data$X5)

# C) 

Q2_data <- read.csv("Assignment1_Q2.csv")

freq_table <- table(Q2_data$X2, Q2_data$X1)

print(freq_table)

# D) 

library(ggplot2)

Q2_data_subset <- Q2_data[!is.na(Q2_data$X4),]

ggplot(Q2_data_subset, aes(sample = X4)) + 
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Variable X4") +
  theme_bw()

# E) 

library(ggplot2)

Q2_data <- read.csv("Assignment1_Q2.csv")

ggplot(Q2_data, aes(x = X2, y = X3)) + 
  geom_boxplot() +
  labs(x = " Value of X2", y = "Value of X3", title = "Grouped Box Plot") +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "grey90"))

# F)

Q2_data <- read.csv("Assignment1_Q2.csv")

Q2_data$X6 <- Q2_data$X3 + Q2_data$X4

# Ask somebody for the Mean Line
ggplot(Q2_data, aes(x = X6)) +
  geom_density(fill="gold", alpha=0.8) +
  geom_vline(aes(xintercept=median(X6)), size=1) +
  labs(title="Density for New Variable X6", x = "value of X6", y="Density") +
  theme_bw()


 # EX3

# A)

df2 <- read.csv("marketing_campaign.csv", header = TRUE, na.strings = c("Unknown", " ", ""))

df2_sub <- df2[complete.cases(df2$Income) & df2$NumStorePurchases != 0 & df2$NumWebPurchases != 0,]

num_obs <- nrow(df2_sub)
num_vars <- ncol(df2_sub)
cat("Number of observations in the subset:", num_obs, "\n")
cat("Number of variables in the subset:", num_vars, "\n")

# B)

df2 <- read.csv("marketing_campaign.csv")
df2_sub <- df2[complete.cases(df2$Income) & df2$NumStorePurchases != 0 & df2$NumWebPurchases != 0,]

quantile(df2_sub$Income, probs = c(0.1, 0.5, 0.8))

# C) 

iq_range_ratio <- function(x) {
  iqr_x <- quantile(x, 0.75) - quantile(x, 0.25)
  range_x <- max(x) - min(x)
  return(iqr_x / range_x)
}
apply(df2_sub[, c("Income", "NumStorePurchases", "NumWebPurchases")], 2, iq_range_ratio)

# D) 

interquantile_ratio <- function(x) {
  IQR(x, na.rm = TRUE) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

apply(df2_sub[, c("Income", "NumStorePurchases", "NumWebPurchases")], 2, interquantile_ratio)
