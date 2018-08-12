################################
#     QUESTION 1
################################

rm(list=ls(all=TRUE))   #remove all variables
library(data.table)
context1 <- fread("attend.csv")   #read data file
attendrt <- context1$attend/32
hwrt <- context1$hw/8

summary(context1)

model1 <- lm(termGPA~priGPA+ACT+attendrt+hwrt, data=context1)
summary(model1)

# Coefficients:
#               Estimate    Std. Error  t value      Pr(>|t|)    
# (Intercept)   -1.286983   0.164169    -7.839      1.77e-14 ***
#   priGPA       0.548962   0.042418    12.942      < 2e-16 ***
#   ACT          0.036099   0.006051    5.966       3.92e-09 ***
#   attendrt      0.155436    6.770       2.81e-11 ***
#   hwrt         0.913031   0.116932    7.808       2.22e-14 ***

#termGPA = -1.287 + 0.549*priGPA + 0.036*ACT + 1.052*attendrt + 0.913*hwrt

#Interpretations:-

#1. Estimated Coeffiecient for attendrt in model 1 is 1.052

#2. Estimated Coeffiecent for hwrt in model 1 is 0.913

#3. The termGPA of 2.906 for a student with a 32 ACT and a 2.2 priGPA who attended 28 lectures and turned-in 8 homework assignments 

#4. The termGPA of 3.407 for a student with a 20 ACT and 3.9 priGPA who attended 28 lectures and turned in 8 homework assignments.


####### to compare b1 and b2, b1*std(priGPA) > b2*std(ACT)

#5. priGPA has more impact to termGPA since higher priGPA tends to higher termGPA

#6. The termGPA of 2.771 for a student with a 25 ACT and a 3.0 priGPA who attends all the classes, but only finishes half the homework assignments

#7. The termGPA of 2.691 for a similarly qualifieed student who turns in all the homwork assignments, but only attends half the classes

#8. Attendance is of more importance to the change of termGPA as even homework rate increases termGPA do not increases to that extend.

#9. Because attendrt and hwrt gives us the relative value and it is easier to compare both of them as they dont have any units, but in ACT and priGPA , different types of values are present with no common comparable parameter.

###############################################################
#          QUESTION 2
###############################################################

rm(list=ls(all=TRUE))
library(data.table)
context2 <- fread("CEOSAL2.csv")

summary(context2)

lsalary <- log(context2$salary)
lmktval <- log(context2$mktval)

lsales <- log(context2$sales)

model2 <- lm(lsalary~lmktval+profits+ceoten, data=context2)
summary(model2)


#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 4.7095052  0.3954502  11.909  < 2e-16 ***
#  lmktval     0.2386220  0.0559166   4.267 3.25e-05 ***
#  profits     0.0000793  0.0001566   0.506   0.6132    
#ceoten      0.0114646  0.0055816   2.054   0.0415 *  

model3 <- lm(lsalary~lmktval+profits+ceoten+lsales, data=context2)
summary(model3)


#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 4.558e+00  3.803e-01  11.986  < 2e-16 ***
#  lmktval     1.018e-01  6.303e-02   1.614   0.1083    
#profits     2.905e-05  1.503e-04   0.193   0.8470    
#ceoten      1.168e-02  5.342e-03   2.187   0.0301 *  
#  lsales      1.622e-01  3.948e-02   4.109 6.14e-05 ***

#Interpretation:

#1. Since profits consist of negative values, Log transformation can't be performed on data with '-ve' values.

#2. Every 1% increase in the market value is associated with a 0.2386% increase in the salary of chief executive officers for U.S. corporations controlling profits and years as CEO of the company.

#3. Every 1% increase in the market value is associated with a 0.1018% increase in the salary of chief executive officers for U.S. corporations controlling profits, years as CEO of the company and % of sales.

#4. Model2 tells us that mktval is quite significant when it comes to salary whereas Model3 has a different story which says there is Omitted Variable Biasing in Model1 because Sales component is missing. From Model 2, it is clear sales is more significant to Salary as compared to mktval.

#5. Coefficient on profits is particularly not of such significance in model3 that can be clearly inferred from the p-value (>0.05) for profits is very high.

#6. Every 1% increase in the sales is associated with  0.1622% increase in the salary of chief executive officers for U.S. corporations controlling for profits, years as CEO of the company and % of mktval.

#################################################
# Question 3
################################################
rm(list=ls(all=TRUE))
library(data.table)
context3 <- fread("hprice1.csv")
summary(context3)

llotsize <- log(context3$lotsize)
lsqrft <- log(context3$sqrft)

model4 <- lm(price~bdrms+llotsize+lsqrft+colonial, data=context3)

summary(model4)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2030.455    210.967  -9.625 3.68e-15 ***
#  bdrms          18.572      9.308   1.995   0.0493 *  
#  llotsize       61.446     12.372   4.966 3.60e-06 ***
#  lsqrft        225.508     30.072   7.499 6.41e-11 ***
#  colonial        4.134     14.509   0.285   0.7764    

lprice <- log(context3$price)

model5 <- lm(lprice~bdrms+llotsize+lsqrft+colonial, data=context3)
summary(model5)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.34959    0.65104  -2.073   0.0413 *  
#  bdrms        0.02683    0.02872   0.934   0.3530    
#llotsize     0.16782    0.03818   4.395 3.25e-05 ***
#  lsqrft       0.70719    0.09280   7.620 3.69e-11 ***
#  colonial     0.05380    0.04477   1.202   0.2330

#Interpretation

#1. Every 1% increase in the lotsize is associated with a $614.66 increase in the price of the house controlling No. of bedrooms,size of the house and colonial style.

#2. Every 1% increase in the lotsize is associated with a 0.1678% increase in the price of the house controlling for No. of bedrooms,size of the house and colonial style.

#3. The price of the house increases by $4,134 for every colonial house

#4. R-squared value basically suggests that Model4 is a better fit for the given data set.

#5. As per model4 if we increase 1 bedroom master suite then that will eventually increase the price of house by $18572 and increase in 10% of sqft will increase the price of house by $22550. Total price will increase to approximately $342k from $300k. $42k is greater than my valued enjoyment rate of $20k so model 4 is appropriate model for pursuing the expansion


####################################################
# Question 4
###################################################
rm(list=ls(all=TRUE))
context4 <- fread("JTRAIN2.csv")
summary(context4)

model6 <- lm(re78~re75+train+educ+black, data = context4)
summary(model6)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  1.97686    1.89028   1.046   0.2962   
#re75         0.14697    0.09811   1.498   0.1349   
#train        1.68422    0.62700   2.686   0.0075 **
#  educ         0.41026    0.17267   2.376   0.0179 * 
#  black       -2.11277    0.82941  -2.547   0.0112 * 

#Interpretation

#1. Every $1000 increase in re75(Real earnings in 1975) is associated with a $146.97 increase in re78 controlling job training, years of education and black

#2. For every trained low income man, there is a $1684.22 increase in the Real earnings in 1978 controlling re75(Real earnings in 1975), years of education and black. It is significat at 1%

#3. For every black low income man, there is a $2112.28 decrease in the Real earnings in 1978 controlling re75(Real earnings in 1975), years of education, and job training
