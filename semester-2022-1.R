#A
install.packages("ggplot2")
library(ggplot2)
install.packages("car")
library(car)
Force=c(30,40,30,40,30,40,30,40,30,40,30,40,30,40,30,40,25,45,35,35,35,35,35,35,35,35,35,35,35,35)
Power=c(60,60,90,90,60,60,90,90,60,60,90,90,60,60,90,90,75,75,45,105,75,75,75,75,75,75,75,75,75,75)
Temperature= c(175, 175, 175, 175, 225, 225, 225, 225, 175, 175, 175, 175, 225, 225, 225, 225, 200, 200, 200, 200, 150, 200, 200, 200, 200, 200, 200, 200, 200, 200)
Time = c(15, 15, 15, 15, 15, 15, 15, 15, 25, 25, 25, 25, 25, 25, 25, 25, 20, 20, 20, 20, 20, 10, 10, 20, 20, 20, 20, 20, 20, 20)
Strength = c(26.2, 35.9, 39.8, 39.3, 38.6, 38.5, 48.8, 26.6, 23.4, 33.6, 38.6, 52.1, 39.3, 32.9, 33.3, 56.0, 35.2, 46.9, 22.7, 58.7, 34.5, 35.4, 35.7, 41.8, 36.5, 37.6, 40.0, 46.0, 27.8, 40.3)
data=data.frame(Force,Power,Temperature,Time,Strength)
data

model=lm(Strength~Force+Power+Temperature+Time,data=data)
model
summary(model)
residuals=residuals(model)
residuals

qqnorm(residuals,main="QQ plot:residuals plot")
qqline(residuals,col="green")

#B
studentized_residuals=rstandard(model)
studentized_residuals
deleted_studentized_residuals=rstudent(model)
deleted_studentized_residuals

par(mfrow =c(1,3))
qqnorm(studentized_residuals)
qqline(studentized_residuals,col="red")

qqnorm(deleted_studentized_residuals)
qqline(deleted_studentized_residuals,col="blue")

outliers_studentized=which(abs(studentized_residuals)>2)
outliers_studentized

outliers_deleted_studentized=which(abs(deleted_studentized_residuals)>2)
outliers_deleted_studentized

leverage_values=hatvalues(model)
leverage_values

mean_leverage=mean(leverage_values)
mean_leverage

leverage_2x=which(leverage_values> 2 *mean_leverage)
leverage_2x
leverage_3x=which(leverage_values> 3 *mean_leverage)
leverage_3x

cooks_d=cooks.distance(model)
cooks_d
dffits=dffits(model)
dffits

influential_cooks=which(cooks_d>4/nrow(data))
influential_cooks

influential_dffits=which(abs(dffits)>(2*sqrt(length(coef(model))/nrow(data))))
influential_dffits


#C
data_clean=data[cooks_d<=(4/nrow(data)),]
data_clean

clean_model=lm(Strength~Force+Power+Temperature+Time,data=data_clean)

clean_model

summary(model)

residuals_clean=residuals(clean_model)
residuals_clean

qqnorm(residuals_clean)
qqline(residuals_clean,col="green")

#E

summary(model)$r.squared
summary(model)$adj.r.squared

#F
## Get model coefficients
coefficients = summary(model)$coefficients
coefficients
beta1_estimate = coefficients["Force", "Estimate"]
beta1_estimate
beta1_se = coefficients["Force", "Std. Error"]
beta1_se

# t-test statistic
t_value = (beta1_estimate - 1.2) / beta1_se
t_value
# Degrees of freedom
df = df.residual(model)
df
# Two-tailed p-value
p_value = 2 * pt(-abs(t_value), df)
p_value















































































