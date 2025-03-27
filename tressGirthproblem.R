data=trees
x1=c(trees$Girth)
x1
x2=c(tress$Height)
x2
x=c(x1,x2)
x
y=c(trees$Volume)
y
#fitted model
fit=lm(y~x1+x2)
fit

#coefficient
coef(fit)
#summary
summary(fit)
#predicted value=fited value : y hat
fitted(fit)
predict(fit)

#residuals : error , e= y - y hat
residuals(fit)

#deviance (TSS)
deviance(lm(y~1))

#deviance (ESS)  Error deviance 
deviance(fit)

summary(fit)$r.squared
summary(fit)$adj.r.squared

#Anova
anova(fit)

#AIC
AIC(fit)


######Test statistics to test residuals assumptions and check normality####

hist(fit.residuals)
qqnorm(fit.residuals)
plot(fit,which=2)   ##qqplot

######_________________test statistic of Check of normality assumption of residuals__________#######


fit.residuals=resid(fit)
fit.residuals
# Kolmogrov-smirnov test#### for normality test ## H0: the varaible follows Normal Dist.
        
mean=mean(fit.residuals)
mean
sd=sd(fit.residuals)
sd
ks.test(fit.residuals, "pnorm", mean , sd)










win.graph(5.5,3,8)
par(mar=c(4,4,3,3))
fit.residuals<-resid(fit)
par(mfrow=c(1,2)) 

####### ______________________Prediction function and predicted value


newdata <-data.frame(Girth=c(9.1,11.5,13.0), Height=c(69,74,84))

predict(fit, newdata)



#interval estimation of reg coef

confint(fit)




















