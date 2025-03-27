#problem-simple linear-1
#(a)

# Data
data=data.frame(
money=c(2.0,2.5,3.2,3.6,3.3,4.0, 4.2,4.6,4.8,5.0),
income=c(5.0,5.5,6.0,7.0,7.2,7.7,8.4,9.0,9.7,10.0)
)
data

# Scatter plot with regression line
plot(money, income, main = "Money vs Income", xlab = "Money", ylab = "Income", pch = 19)
abline(lm(income ~ money), col = "red")

# Model summary
model=lm(income~money,data=data)
model
summary(model)


#(b)

#extract intercept and slope
intercept=coef(model)[1]
cat("intercept:",intercept,"\n")
slope=coef(model)[2]
cat("slope:",slope,"\n")

#intercept (b0):it represents the estimated national income when quantity of money is 0
#slope(b1):this represents the change in national income for every unit increases
#in the quantity of money

#(c)

#target national income 
target_income=12.0
target_income

required_money=(target_income-intercept)/slope
required_money



#interpretation:The required quantity of money
#to achieve a national income of $12,000,000 is approximately 6.69 million.
According to the regression model, if the money supply is increased
#to 6.69 million, the national income is expected to reach $12,000,000. 
#This result is based on the observed linear relationship between money supply and national income.





























