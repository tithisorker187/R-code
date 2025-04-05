#problem-2(updated)
install.packages("ggplot2")
library(ggplot2)
X <- c(-7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7)
Y <- c(1, 14, 25, 34, 41, 46, 49, 50, 49, 46, 41, 34, 25, 14, 1)

# Step 2: Fit a simple linear regression model
model <- lm(Y ~ X)
model
# Step 3: Summary of the model
summary(model)

# Step 4: Scatter plot of Y vs X
plot(X, Y, xlab = "X", ylab = "Y", main = "Scatter Plot of Y vs X")
abline(model, col = "red")  # Add regression line

# Step 5: Plot of residuals vs X
residuals =residuals(model)
residuals
plot(X, residuals, xlab = "X", ylab = "Residuals", main = "Scatter Plot of Residuals vs X")
abline(h=0, col = "blue")
