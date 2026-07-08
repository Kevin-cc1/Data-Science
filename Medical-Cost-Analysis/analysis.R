# 3.1.1 One-sample t-test (right-tailed)
t.test(insurance$bmi, mu = 30, alternative = "greater")


result <- t.test(insurance$bmi, mu = 30, alternative = "greater")

t_value <- result$statistic
df <- result$parameter

t_critical <- qt(0.95, df)

x <- seq(-4, 4, 0.01)
y <- dt(x, df)

plot(x, y, type = "l", main = "t-Distribution: One-Sample t-test on BMI",
     xlab = "t value", ylab = "Density")

abline(v = t_value, col = "blue", lwd = 2)
abline(v = t_critical, col = "red", lwd = 2, lty = 2)

# Add a legend
legend("topright", legend = c("Test Statistic ", "Critical Value "),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)


# 3.1.2 Two-Sample t-test on Charges
insurance$smoker <- factor(insurance$smoker, levels = c("yes", "no"))

t2_result <- t.test(charges ~ smoker, data = insurance, alternative = "greater")
print(t2_result)


t_value <- as.numeric(t2_result$statistic)
df <- as.numeric(t2_result$parameter)
t_critical <- qt(0.95, df)


x_min <- min(-4, t_value - 1)
x_max <- max(4, t_value + 1)
x <- seq(x_min, x_max, 0.01)
y <- dt(x, df)


plot(x, y, type = "l", main = "Graph 2: Medical Charges by Smoking Status",
     xlab = "t value", ylab = "Density")


abline(v = t_value, col = "blue", lwd = 2)            
abline(v = t_critical, col = "red", lwd = 2, lty = 2)  

legend("topright",
       legend = c("Test Statistic ", "Critical Value "),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)



# 3.2 Correlation Test between Age and Charges

cor_result <- cor.test(insurance$age, insurance$charges, alternative = "greater", method = "pearson")
print(cor_result)

plot(insurance$age, insurance$charges,
     main = "Graph 3: Correlation between Age and Charges",
     xlab = "Age", ylab = "Charges", pch = 19, col = "darkblue")

abline(lm(charges ~ age, data = insurance), col = "red", lwd = 2)

legend("topleft", legend = "Regression Line", col = "red", lwd = 2)

#3.3 Regression
# Perform linear regression: charges ~ bmi
model <- lm(charges ~ bmi, data = insurance)

# Show regression summary
summary(model)

# Plot scatter plot and regression line
plot(insurance$bmi, insurance$charges,
     main = "Regression Line: Charges vs BMI",
     xlab = "BMI", ylab = "Insurance Charges",
     pch = 19, col = "darkorange")

abline(model, col = "red", lwd = 2)

#3.4 ANOVA

anova_result <- aov(charges ~ region, data = insurance)
summary(anova_result)


# 3.5 Chi-Square Test of Independence: Sex vs Smoker

table_chi <- table(insurance$sex, insurance$smoker)

print(table_chi)

chi_result <- chisq.test(table_chi)

print(chi_result)

table_chi <- table(insurance$sex, insurance$smoker)

barplot(table_chi,
        beside = TRUE,
        col = c("lightblue", "lightpink"),
        main = "Graph 4: Smoking Status by Gender",
        xlab = "Smoker",
        ylab = "Count",
        legend.text = TRUE,
        args.legend = list(title = "Gender", x = "topright"))




