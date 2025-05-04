# Step 1: Create the dataset
temperature=c(53, 56, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 80, 81)
temperature
failure= c('Y','Y','Y','Y','Y','N','N','N','N','N','N','Y','Y','N','N','N','Y','N','N','N','N','N','N')
faliure
# Step 2: Convert failure variable to binary (1 for failure, 0 for no failure)
failure_binary=ifelse(failure == "Y", 1, 0)
faliure_binary
# Step 3: Fit the logistic regression model
model = glm(failure_binary ~ temperature, family = binomial)
model
# Step 4: Summary of the model
summary(model)

new_data=data.frame(temperature=40)
new_data

predicted_prob=predict(model,newdata=new_data,type="response")
predicted_prob