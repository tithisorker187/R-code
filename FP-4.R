install.packages=("ggplot2")
library(ggplot2)

install.packages=("car")
library(car)

data=data.frame(
libby=c(27.1,20.9,33.4,77.6,37.0,21.6,17.6,35.1,32.6,26.0,27.6,38.7,27.8),
newgate=c(19.7,18.0,26.1,15.7,27.6,24.9,23.4,23.1,31.3,23.8)
)
data
fit=lm(newgate~libby,data=data)
fit

summary(fit)

residuals=residuals(fit)
residuals
studentized_residuals=rstandard(fit)
studentized_residuals

deleted_studentized_residuals=rstudent(fit)
deleted_studentized_residuals

outliers_studentized=which(abs(studentized_residuals)>2)
outliers_studentized
data[outliers_studentized,]


    data$outliers_flag_studentized <- ifelse(abs(studentized_residuals)>2,
                              "Outlier","Not outlier")
              print(data$outliers_flag_studentized)


outliers_deleted_studentized=which(abs(deleted_studentized_residuals)>2)
outliers_deleted_studentized

data[outliers_deleted_studentized,]

        outliers_flag_del_studentized <- ifelse(abs(deleted_studentized_residuals)>2,
                                     "Outlier","Not outlier")
              print(outliers_flag_del_studentized)




par(mfrow=c(1,3))

qqnorm(residuals, main="QQ plot: residuals plot")
qqline(residuals,col="red")

shapiro.test(residuals)

qqnorm(studentized_residuals, main="QQ plot: studentized residuals plot")
qqline(studentized_residuals,col="blue")
shapiro.test(studentized_residuals)

qqnorm(deleted_studentized_residuals,main= "QQ plot: plot")
qqline(deleted_studentized_residuals,col="green")
shapiro.test(deleted_studentized_residuals)
#c

#leverage point 

leverage_values=hatvalues(fit)
leverage_values
mean_leverage=mean(leverage_values)
mean_leverage

leverage_2x=which(leverage_values>2*mean_leverage)
leverage_2x
data[leverage_2x,]

data$leverage_flag<- ifelse(leverage_values> 2* mean_leverage,
                                     "Leverage","Not Leverage")
              print(data$leverage_flag)



leverage_3x=which(leverage_values>3*mean_leverage)
leverage_3x
data[leverage_3x,]


#d

cooks_d=cooks.distance(fit)
cooks_d

dffits=dffits(fit)
dffits


influential_cooks=which(cooks_d>4/nrow(data))
influential_cooks

        influential_cooks_flag= ifelse( cooks_d> 4/nrow(data),
                                        "Influential",
                                         "Not Influential")
            print(influential_cooks_flag)



influential_dffits= which(abs(dffits)>(2*sqrt(length(coef(fit))/nrow(data))))
influential_dffits

#F
#removing outliers

data_clean=data[cooks_d<=(4/nrow(data)),]
data_clean

fit_clean=lm(newgate~libby,data=data_clean)
fit_clean

residuals_clean=residuals(fit_clean)
residuals_clean

qqnorm(residuals_clean)

shapiro.test(residuals_clean)












































