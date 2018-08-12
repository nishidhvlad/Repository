
# Reading file
context1 = read.csv('WAGE1.csv')

# Elbow plot with within group sum of squares
seed = 2 
maxClusters = 10
wss = matrix(data = 1:10, nrow = maxClusters, ncol = 2)

for (i in 1:maxClusters) 
  {
    set.seed(seed)
    model <- kmeans(context1,centers=i,nstart=10)
    wss[i,2] <- model$tot.withinss
  }
plot(x = wss[,1], y = wss[,2], type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")


# Model
set.seed(seed)
model1 = kmeans(context1,centers=3,nstart=10)
context1_wclusters = cbind(context1, model1$cluster)
model1$centers


# Summary
cluster_summary = aggregate( context1[,c('educ','exper','tenure')], by = list(model1$cluster), mean)
cluster_summary


model2 = lm(formula = wage~educ+exper+tenure, data = context1[context1_wclusters[,22] ==1,])
summary(model2)

model3 = lm(formula = wage~educ+exper+tenure, data = context1[context1_wclusters[,22] ==2,])
summary(model3)

model4 = lm(formula = wage~educ+exper+tenure, data = context1[context1_wclusters[,22] ==3,])
summary(model4)

## Interpretations
# 1. Based on the elbow plot k=4 is the optimal number of clusters that should be used
# 2. Based on the three clusters
#    Group 1 has most experienced people but leats education
#    Group 2 has the most educated people but least tenure
#    Group 3 has people with moderate of everything education, experience and tenure
# 3. Difference between model 1, 2 and 3
#    Model 1 - exper is not significant, as it has all people with most experience, so experience doesn't affect wages after such long time
#    Model 2 - Education has signiifcant effect on wages, that is shown by model. All variables are significant
#    Model 3 - Expeirence is not significant. Education and tenure has significant impact on wages of group 3 people




library(tseries)


#Question2
context2 = read.csv("ffportfolios.csv")

cnt =0 
for( i in 2:32)
{
  if(kpss.test(context2[,i])$statistic > 0.347)
    {
    print( paste('trend ',i,' is not level stationary for 90% confidence interval'))
    cnt = cnt+1
    }
}
print( paste('Total ',cnt,' trends that are not level stationary at 90% confidence interval'))


#
model5 <- prcomp(context2[,2:33])
screeplot(model5,type="lines")

factor <- model5$x[,1]
factor <- scale(factor)
hist(factor)
var(factor)
#years where factor is less than -2.58
years_less <- trunc(x = context2[ factor < -2.58, 'Year'], digits = 0)
years_less

# ## Question2
# 1. Based on the screeplot we we should use one principal components 
#
# 2. This principal component shows the years with major distinction. It highlights the years where portfolio was at the 
#    minimum, may be due to economic crises or something similar.   
   

