#############....................problem-3.................###################

#*********************************(a)*****************************************

house=c(25.9,29.9,27.9,25.9,29.9,29.9,30.9,28.9,45.8,36.9,38.9,37.9,44.5,37.9,37.5,43.9)
size=c(3.472,3.531,2.275,4.050,4.455,4.455,5.850,9.520,7.326,8.000,9.150,6.727,9.890,5.000,5.520,7.800)
age=c(42,62,40,54,42,56,51,32,31,3,48,44,50,22,40,23)
fit=lm(house~size+age)
fit
summary(fit)
#B1=1.70921 ,a one unit increase in size results in an increase in house is 1.709.
#B2=-0.08262 ,it means for every unit increase in size,house decreases 0.082 units.

#******************************(b)**********************************##
##calculate R square and adjusted R square********

#calculate R square
summary(fit)$r.squared

#interpretation:  R^2=0.469it means that 947% of the variance in y explained
#by the regression model.and remaining due to random error.

#Adjusted R squared

summary(fit)$adj.r.squared

#Interpretation:  adjusted R squared= 0.94 thats a high value.so its a better model fit.

#********************************(c)*************************

#contrast ANOVA table for the fit 

anova(fit)

#test of significance 

coefficients=summary(fit)$coefficients
coefficients

t_value_size=coefficients["size", "t value"]
t_value_size
t_value_age=coefficients["age", "t value"]
t_value_age


#df=n-k-1
df=df.residual(fit)
df
#pr(>|t|) thats why test is two tailed.
#compute P value
p_value_size=2*pt(-abs(t_value_size),df)
p_value_size
p_value_age=2*pt(-abs(t_value_age),df)
p_value_age

#interpretation: p< 0.05 .so statistically significant.

########****************************(D)**************************************

#confidence interval 

new_data=data.frame(size=10000,age=50)
new_data
conf_interval=predict(fit,new_data,interval="confidence",level=0.90)
conf_interval
pred_interval=predict(fit,new_data,interval="prediction",level=0.90)
pred_interval

#interpretation : 
#confidence interval:the range in which the average price of similar ouses is expected to fall
#prediction interval: its also expected to fall.

#********************************(E)**********************************
#standardised variables 
house_std=(house-mean(house)/sd(house))
house_std
size_std=(size-mean(size)/sd(size))
size_std
age_std=(age-mean(age)/sd(age))
age_std
model_std=lm(house_std~ size_std + age_std)
model_std
summary(model_std)$coefficients


###*******************************(F)***********************************

#fit regression models without interaction
model1=lm(house~size+age)
model1
#with interaction

model2=lm(house~size*age)
model2
#compare models using anova
anova_results=anova(model1,model2)
print(anova_results)

#check significance of interaction term 
summary(model1)
summary(model2)





















































