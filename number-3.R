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
qqline(residuals,col="blue")

#plot-2 studentized residuals
qqnorm(studentized_residuals,main="QQ plot: Studentized Residuals")
qqline(deleted_studentized_residuals,col="green")
#plot_3

qqnorm(deleted_studentized_residuals,main="QQ plot:Deleted Studentized Residuals")
qqline(deleted_studentized_residuals,col="red")
#reset plot layout
par(mfrow=c(1,1))




#***********************(b)********************************


outliers_studentized = which(abs(studentized_residuals) > 2)
outliers_studentized
outliers_deleted_studentized=which(abs(deleted_studentized_residuals) > 2)
outliers_deleted_studentized

#interpretation: observation 4 is flagged as an outlier by both studentized
#and deleted studentized residuals
#this means it has large residual.indicating the actual newgate value differs 
#significantly from the predicted value given libby.




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

#interpretation: obsevation 4 has a leverage point which exceeds both 2x and 3x mean leverage.
#4 is high leverage point ,meaning its predictor value(libby) is far from others.



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

#interpretation: Tells use how much predicted values for all observations would change
#if a given point removed.

#here higher cooks distance means greater influence on the regression fit.
#observation #4 (4th row from dataset)was flagged by both cooks and dffits 
#as influential.
#this means removing this point would affect the model.

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


#Interpretation of Results:
#Before Outlier Removal:
#The QQ plot likely shows curvature or heavy tails, indicating the residuals deviate from normality.
#This suggests the regression assumptions (normality of residuals) may be violated, possibly due to an outlier or influential observation.

#After Outlier Removal:
#The QQ plot appears more linear, meaning the residuals now follow the normal distribution more closely.

















