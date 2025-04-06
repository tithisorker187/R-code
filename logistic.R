#logistic regression

loan_data=data.frame(
Income=c(4000,3000,5000,2500,6000,4500,3200,5800,2200,4900),
CreditScore=c(700,650,720,600,750,710,640,730,580,705),
LoanAmount=c(20000,15000,25000,11000,27000,22000,16000,26000,9000,24000),
Married=c("Yes","No","Yes","No","Yes","Yes","No","Yes","No","Yes"),
Approved=c(1,0,1,0,1,1,0,1,0,1)
)
print(loan_data)

#creating dummy varriable for married(yes=1,n0=0)

loan_data$Married_dummy=ifelse(loan_data$Married=="Yes",1,0)
loan_data$Married_dummy

model=lm(Approved~Income+CreditScore+LoanAmount+Married_dummy,family=binomial(link="logit"),data=loan_data)
model
summary(model)

#new applicant data
new_applicant=data.frame(
Income=4500,
CreditScore=700,
LoanAmount=20000,
Married_dummy=1)
new_applicant

#predict probability

prob=predict(model,new_applicant,type="response")
print(prob)







