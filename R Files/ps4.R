rm(list=ls(all = TRUE))

install.packages("RcmdrMisc")
library(data.table)

context1 <- fread("htv.csv")

context1$lwage <- log(context1$wage)

model1 <- lm(lwage~abil+educ+exper, data=context1)

summary(model1)

AIC(model1)
BIC(model1)

context1$sqabil <- (context1$abil)^2
context1$sqeduc <- (context1$educ)^2
context1$sqexper <- (context1$exper)^2
context1$abileduc <- context1$abil*context1$educ
context1$abilexper <- context1$abil*context1$exper
context1$educexper <- context1$educ*context1$exper

model2 <- lm(lwage~abil+educ+exper+sqabil+sqeduc+sqexper+abileduc+abilexper+educexper, data = context1)

summary(model2)

AIC(model2)
BIC(model2)

library(RcmdrMisc)

model2_lowBIC <- stepwise(model2, direction = "backward", criterion = "BIC")
BIC(model2_lowBIC)

##########Interpretations######################################
#1. Education and ability are not significant on there own however an interaction between 
#   ability-education and education-experience is highly significant.

#2. context1$educ*context1$exper is an interactive variable, the interactive variable is 
#    explaining that the effect of experience on wage also depends on the number of years of education.

#############################################################
#Question 2
#############################################################

library(lmtest)
library(plm)
library(sandwich)
context2 <- fread("loanapp.csv")

model3 <- glm(approve~white, family = binomial(link = "logit"), data = context2)

summary(model3)
###White heteroskedasticity robust standard errors
vcovHC(model3)
coeftest(model3,vcov. = vcovHC)

model4 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, family = binomial(link = "logit"), data = context2)
summary(model4)
###White heteroskedasticity robust standard errors
vcovHC(model4)
coeftest(model4, vcov. = vcovHC)

context2$whiteobrat <- context2$white*context2$obrat

model5 <- glm(approve~white+hrat+obrat+loanprc+unem++male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+whiteobrat,family=binomial(link = "logit"),data=context2)

summary(model5)

###White heteroskedasticity robust standard errors
coeftest(model5,vcov. = vcovHC(model5))

##########Interpretations######################
#1. Coefficient of variable white is the change in log odds of approval for white compared to non white, the 
#   coefficient rougly tells us that chance of approval is high for white men.

#2. Coefficient of variable white remains significant even after adding 14 more variables in model4, 
#    however the coefficient value has reduced and standard error has increased.

#3. Coefficient of variable white has become insignificant after adding the interactive variable of white*obrat in 
#   model5.

#4. The interactive variable white*obrat is a variable that provides Other income obligation's for a white person.
#   The reason it has effected the model is because it provides information on the other obligations that a person 
#   white or non white has and this interaction improves the model significantly making it an important variable.

#############################################################
#Question 3
#############################################################

context3 <- fread("smoke.csv")

context3$lincome <- log(context3$income)
context3$ageage <- context3$age*context3$age

model6 <- glm(cigs~educ+age+ageage+lincome+restaurn,family = poisson(link ='log'),data = context3)

summary(model6)

###White heteroskedasticity robust standard errors
coeftest(model6,vcov. = vcovHC(model6))

############Interpretation#################

#1. We estiated that cigs for every 1 year additional education the no of cigrattes consumed by a person decreases by 5.9%

#2. To calculate the marginal effect with respect to change we use the differentiation 
# 
#      @ln(cigs)/ @age  = 100(.1139 - .0013*age)   note : @ is representation of Delta
#      Age : 20 
#      Ln(cigs) = 100*(.1139 - 2*.0013*20) = 6.19%
#      Age : 60 
#      Ln(cigs) = 100*(.1139 - 2*.0013*60) = -4.21%
#      
#   Hence as the persons age increases to 60 the rate of change in no of cigrattes decreases by -4.21% whereas, 
#   when the persons age is 20 the rate of change in no of cigrattes increases by 6.19%

################################################
#       Question4
################################################

install.packages("evtree")
install.packages("party")

library(evtree)
library(party)

context4 <- read.csv("hdisease.csv")

model7 <- evtree(hdisease~age+cp+trestbps+thalach+exang,data=context4)
summary(model7)

plot(model7)

context4$sex <- factor(context4$sex)
context4$exang <- factor(context4$exang)

model8 <- ctree(hdisease~age+cp+trestbps+thalach+exang,data=context4)

summary(model8)

plot(model8)

context5 <- read.csv("hdisease-new.csv")

context5$hdisease_pred <- predict(model8, context5)

#1. Model 8 is overfitting the data whereas Model 7 is underfitting the data.
# 
#1. dset variable is a categorical variable which only provides the name of the hospital,this variable doesn't really
#   explain heart discease stage and at the same time if we use dset to train the model then we cannot predict other
#   categorical values which are not in the training set and hence keeping this in the model would give incorrect 
#   prediction. 

