rm(list = ls(all = TRUE))
library(data.table)
library(lmtest)
library(sandwich)

library(tseries)
library(plm)

context1 <- fread("hprice1.csv")

summary(context1)

lprice <- log(context1$price)
llotsize <- log(context1$lotsize)
lsqrft <- log(context1$sqrft)

model1 <- lm(price~bdrms+lotsize+sqrft, data = context1)
summary(model1)

coef(model1)
coeftest(model1,vcov. = vcovHC)

model2 <- lm(lprice~bdrms+llotsize+lsqrft, data = context1)
summary(model2)

coef(model2)
coeftest(model2,vcov. = vcovHC)

#Interpretations
#a. variables lotsize and sqrft are significant for model1 using the ols test and with increase of of every 1000 units in lotsize, price increases by 2.068 units. 
#   and with each 1000 units increase in sqrft the price increases by 122.8 units keeping rest variables controlled in both cases.

#b. It shows that there is heterodaskicity in the data set. In model1, both variables sqrft and lotsize are significant in ols model but 
#   after the white correction test, only variable sqrft is still significant. we can say bdrms and lotsize are insignificant.

#c. If we talk about the variables, llotsize or ln(lotsize) and lsqrft or ln(sqrft) are quite significant in the model2 for ols test, both with p value almost equal to zero with confidence level more than 99.9%

#d. ln(lotsize) or llotsize and ln(sqrft) or lsqrft are the two variables that are still significant after white corrected test on model2,
#   which is actually same as that of ols test on model2.Intercept and bdrms has no significance.

#e. Taking log in the model has helped to reduce the variance and hence removed heterodaskicity in the data.



context2 <- fread("beveridge.csv")
summary(context2)

model3 <- lm(urate~vrate, data = context2)
summary(model3)

coeftest(model3,vcov=NeweyWest(model3,lag=5))

kpss.test(context2$urate,null="Level")
kpss.test(context2$urate,null="Trend")
kpss.test(context2$vrate,null="Level")
kpss.test(context2$vrate,null="Trend")

kpss.test(diff(context2$urate), null="Level")   #good
kpss.test(diff(context2$urate), null = "Trend")
kpss.test(diff(context2$vrate), null="Level")    #good
kpss.test(diff(context2$vrate), null = "Trend")

kpss.test(diff(diff(context2$urate)), null = "Level")    #good
kpss.test(diff(diff(context2$urate)), null = "Trend")    #good
kpss.test(diff(diff(context2$vrate)), null = "Level")   #good
kpss.test(diff(diff(context2$vrate)), null = "Trend")   #good

model4 <- lm(diff(urate)~diff(vrate), data = context2)
summary(model4)

coeftest(model4,vcov=NeweyWest(model4,lag=5))

##Interpretations##

# f. As per the OLS significance test, the coefficient for vacancy rate is significant with a confidence level of more than 99.9% and after Newey West significance test, the coeffecient for vacancy rate is significant with a 
#    significance level of 0.001 with slight difference in the t value which implies there is autocorrelation.

# g. Based on the KPSS findings, first level difference transformation should be applied to to the unemployment
#    rate before modeling.

# h. Based on the KPSS findings, first level difference transformation should be applied to the vacancy
#    rate before modeling.

# i. From Model3 to Model4, the variable vacancy rate has changed from being most significant (Model 3) to 
#    insignificant (Model4).

# j. Model4 describes the data better than the model3.


context3 <- fread("JTRAIN.csv")
summary(context3)

context3$d88 <- ifelse(context3$year == '1988', 1, 0)
context3$d89 <- ifelse(context3$year == '1989',1,0)

context3$granttminus1 <- ifelse(context3$year == '1987', 0,shift(context3$grant,1L,type = "lag"))

lscrap <- log(context3$scrap)

context3    <- plm.data(context3,index=c("fcode","year"))

model5      <- plm(lscrap~d88+d89+grant+granttminus1,model="pooling",data=context3)
summary(model5)

model6      <- plm(lscrap~d88+d89+grant+granttminus1,model="within",data=context3)
summary(model6)

summary(model6, vcov=vcovHC(model6, method = "arellano"))

##Interpretations##

# k. Grant received for each year is associated with a 20% increase in the scrap rate (per 100 items).

# l. Firm that recieved grant last year is associated with a 4% increase in the scrap rate (per 100 items).

# m. If the firm has received grant for present year or last year then there will be increase in scrap rate (per 100 items)

# n. Grant recieved by firm for the current year with comparison to grant recieved last year will have a 25% decrease in the scrap rate (per 100 items)

# o. Grant recieved by firm for the last year with comparison to grant recieved last to last year will have a 42% decrease in the scrap rate (per 100 items)

# p. When compared to last year grant recieved by the firm is associated with a decrease in the scrap rate. Similarly
#    grant recieved by the firm last year compared to the previous year is associated with a decrease in the scrap rate.

# q. As per OLS significance test, variables d89 and grant are insignificant with a significance of level 0.1, grantlstyr 
#    is significant with a significancee of level 0.05. As per Arellano, all variables d89, grantlstyr and grant are insignificant.
