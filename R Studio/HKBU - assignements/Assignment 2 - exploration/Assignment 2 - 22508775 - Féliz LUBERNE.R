# EX1.a

data1.a <- read.csv("C:/Fichiers/HKBU 2023/Cours Semestre 6/BUSI2045 - Data Analytics for Business Decision Making/Assignement/Assignment 2/NILT2012GR_SUBSET.csv")

data1.a$log_Income <- log(data1.a$persinc2)

mean_log_income <- mean(data1.a$log_Income, na.rm = TRUE)
sd_log_income <- sd(data1.a$log_Income, na.rm = TRUE)

correlation <- cor(data1.a$log_Income, data1.a$rage, use = "complete.obs")

# EX1.b

data1.b <- read.csv("C:/Fichiers/HKBU 2023/Cours Semestre 6/BUSI2045 - Data Analytics for Business Decision Making/Assignement/Assignment 2/NILT2012GR_SUBSET.csv")

data1.b$log_Income <- log(data1.b$persinc2)

plot(data1.b$rage, data1.b$log_Income, xlab = "Age", ylab = "Log Income", pch = 16, cex = 0.5)

# EX1.c

data1.c <- read.csv("C:/Fichiers/HKBU 2023/Cours Semestre 6/BUSI2045 - Data Analytics for Business Decision Making/Assignement/Assignment 2/NILT2012GR_SUBSET.csv")

data1.c$log_Income <- log(data1.c$persinc2)

cor_test <- cor.test(data1.c$log_Income, data1.c$rage, method = "pearson", use = "complete.obs")

print(cor_test)

# EX2.a

data2.a <- read.csv("C:/Fichiers/HKBU 2023/Cours Semestre 6/BUSI2045 - Data Analytics for Business Decision Making/Assignement/Assignment 2/marketing_campaign.csv")

data_subset <- subset(data2.a, Education %in% c("Graduation", "Master", "PhD") & Marital_Status %in% c("Single", "Married"))

nrow(data_subset)

# EX2.b

table(data_subset$Education)
prop.table(table(data_subset$Education, data_subset$Marital_Status), 1)

# EX2.c

observed_counts <- table(data_subset$Education)

n <- sum(observed_counts)
expected_proportions <- rep(1/length(observed_counts), length(observed_counts))
expected_counts <- n * expected_proportions

chisq.test(observed_counts, p = expected_proportions)

# EX2.d

contingency_table <- table(data_subset$Education, data_subset$Marital_Status)

chisq.test(contingency_table)

# EX2.e

marriage_rate <- sum(data_subset$Marital_Status == "Married") / nrow(data_subset)
summary_data <- data.frame(marriage_rate)

print(summary_data)

# EX3.a

data3.a <- read.csv("C:/Fichiers/HKBU 2023/Cours Semestre 6/BUSI2045 - Data Analytics for Business Decision Making/Assignement/Assignment 2/marketing_campaign.csv")

subset_data <- subset(data3.a, Marital_Status %in% c("Married", "Single"))
income_avg <- aggregate(Income ~ Marital_Status, data = subset_data, mean)
t.test(Income ~ Marital_Status, data = subset_data)

# EX3.b

filtered_data <- data3.a[data3.a$Education %in% c("Graduation", "Master", "PhD"),]
grouped_data <- aggregate(Income ~ Education, data = filtered_data, mean, na.rm = TRUE)
print(grouped_data)

library(ggplot2)
data3.a %>%
  ggplot(aes(x = Education, y = Income, fill = Education)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Education", y = "Income Level", fill = "Education") +
  ggtitle("Mean Income by Education Level") +
  theme_bw()

# EX3.c

income_education <- data3.a %>% select(Education, Income)
model <- aov(Income ~ Education, data = income_education)
summary(model)
