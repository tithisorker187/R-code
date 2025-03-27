#*********************problem :( 2)******************************
#delivery time data

Y=c(16.68,11.50,12.03,14.88,13.75,18.11,8.00,17.83,79.24,21.50,40.33,21.00,13.50,19.75,24.00,29.00,15.35,19.00,9.50,35.10,17.90,52.32,18.75,19.83,10.75)
X1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
X2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,132,36,770,140,810,450,635,150)
model=lm(Y~X1+X2)
model
summary(model)
#95% confidence interval 
confint(model)

#sum of error square ESS
deviance(model)

#sum of square total TSS
deviance(lm(Y~1))

#RSS
RSS=5784.543-233.7317
RSS
#ANOVA
anova(model)

#R squared and adjusted R squared
summary(model)$r.squared
summary(model)$adj.r.squared

#interpretation: we have very high R^2 qnd ajjusted R^2 values that indicate that the ols regression adequately fits the data.

#test of significance
coefficients=summary(model)$coefficients
coefficients

t_value_X1=coefficients["X1", "t value"]
t_value_X1
t_value_X2=coefficients["X2", "t value"]
t_value_X2
#df=n-k-1
df=df.residual(model)
df

p_value_X1=2*pt(-abs(t_value_X1),df)
p_value_X1

p_value_X2=2*pt(-abs(t_value_X2),df)
p_value_X2

#interpretation: p value is less than o.o5 .so test is significant.






