#**************************problem (3)*******************************
#water fall data.
#(a)
#Given data 
install.packages("ggplot2")
library(ggplot2)

install.packages("car")
library(car)
data=data.frame(
libby=c(27.1,20.9,33.4,77.6,37.0,21.6,17.6,35.1,32.6,26.0,27.6,38.7,27.8),
newgate=c(19.7,18.0,26.1,15.7,26.1,19.9,15.7,27.6,24.9,23.4,23.1,31.3,23.8)
)
data
Fit=lm(newgate~libby,data=data)
Fit

#compute ordinary residuals and normal probability plot
residuals=residuals(Fit)
residuals
#compute studentized residuals
studentized_residuals=rstandard(Fit)
studentized_residuals
deleted_studentized_residuals <- rstudent(Fit)  # Deleted Studentized residuals
deleted_studentized_residuals 

###plot
par(mfrow=c(1,3))

#normal probability plot
qqnorm(residuals,main="QQ plot: Ordinary residuals")
qqline(residuals)

#plot-2 studentized residuals
qqnorm(studentized_residuals,main="QQ plot: Studentized Residuals")
qqline(deleted_studentized_residuals)
#plot_3

qqnorm(deleted_studentized_residuals,main="QQ plot:Deleted Studentized Residuals")
qqline(deleted_studentized_residuals)
#reset plot layout
par(mfrow=c(1,1))




#***********************(b)********************************


outliers_studentized = which(abs(studentized_residuals) > 2)
outliers_studentized
outliers_deleted_studentized=which(abs(deleted_studentized_residuals) > 2)
outliers_deleted_studentized

#******************************(C)*************************888

# Compute leverage values (hat values)
leverage_values <- hatvalues(Fit)
leverage_values

# Calculate mean leverage
mean_leverage <- mean(leverage_values)
mean_leverage

# Identify high leverage points
high_leverage_2x <- which(leverage_values > 2 * mean_leverage)
high_leverage_2x
high_leverage_3x <- which(leverage_values > 3 * mean_leverage)
high_leverage_3x

#******************************(D)**********************

#compute cooks distance and DFFITS
cooks_d=cooks.distance(Fit)
cooks_d
dffits_value=dffits(Fit)
dffits_value


#identify influential points
influential_cooks=which(cooks_d>4/nrow(data))
influential_cooks

influential_dffits=which(abs(dffits_value)>(2*sqrt(length(coef(Fit))/nrow(data))))
influential_dffits



#********************************(E)*************************

plot QQ before removing outliers
par(mfrow=c(1,3))
qqnorm(residuals,main="QQ plot:before removing outliers QQ residuals plot")
qqline(residuals,col="red")

#removing outliers
data_clean=data[cooks_d<=(4/nrow(data)),]
data_clean

fit_clean=lm(libby~newgate,data=data_clean)
fit_clean
residual_clean=residuals(fit_clean)
residual_clean

#plot QQ after removing outliers

qqnorm(residual_clean,main="QQ plot:after removing outliers QQ plot")
qqline(residual_clean,col="blue")


Interpretation of Results:
#1. QQ Plot Before Removing Outliers:
#If the points deviate significantly from the straight line, 
#it indicates that residuals are not normally distributed,
#possibly due to influential outliers.



#2.QQ Plot After Removing Outliers:
#If the plot becomes more linear, it suggests that removing the outliers improved normality.
#If there’s little or no change, the outliers may not have affected normality significantly.
#If the plot worsens, the removed points might not have been true outliers, and they could be important for the model.














