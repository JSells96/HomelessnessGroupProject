##Load Packages
install.packages("Ecdat")
library(Ecdat)
install.packages("car")
install.packages("caret")
install.packages("gvlma")
install.packages("e1071")
install.packages("lmtest")
library("car")
library("caret")
library("gvlma")
library("e1071")
library("lmtest")




##Let's see if there is a growth pattern we can analyze. 
##If we get the difference per year of each Top 6 State, we can predict the trend of increase in future years
##This was done in Excel 


#Linear Regression

##Does the number of total people in the state share any indication of the total people that are homeless in that state?
##x = IV, y = DV

##Test Assumptions
##Testing for linearity
scatter.smooth(x=Totals1$Est2010, y=Totals1$`2010`)

##Testing for homoscedasticity
##Create Linear Model
lm2010 <- lm(`2010` ~ Est2010, data=Totals1)
par(mfrow=c(2,2))
plot(lm2010)
##Looks to be heteroscedastic, fails assumption
##Correct violations
distBCMod1 <- caret::BoxCoxTrans(Totals1$`2010`)
print(distBCMod1)
##Add this to dataset
Totals2 <- cbind(Totals1, dist_newM=predict(distBCMod1, Totals1$`2010`))
##Create new linear model to see if the transformation worked
lmMod2010 <- lm(dist_newM ~ Est2010, data=Totals2)
lmtest::bptest(lmMod2010)
##p-value is more than .05, we are not significant and pass assumption

##Testing for homogeneity of variance
Var2010 <- fligner.test(dist_newM ~ Est2010, data = Totals2)
print(Var2010)
gvlma(lmMod2010)
#Looks like their could be a problem

##Test for outliers
summary(influence.measures(lmMod2010))
##Definitely influential outliers 

summary(lmMod2010)
##With F Statistic being 86, it is probably significant and p-value is much lower than .05, so we do have significance
#There is evidence that there is a linear relationship between the total number of people in the state and total number of the homeless in that state.
##This conclusion fits our intuition that the larger the state population, the larger the homeless population. It is more than plausible.

##Based on the Multiple R Squared, 64% of the variation in the total homeless persons can be explained by the total number of persons in the state. 


##Let's try to predict an amount based on this model.
##For every person in the state, there is a predicted .000000138 person homeless. 
##Lets say there are 200,000 people in a state...
y1 = (.000000138*(200000)) + 7.88
print(y1)
##This equation states there will be an estimated 8(7.9) homeless in the state of that total population
##Lets say there are 1,000,000 people in a state...
y2 = (.000000138*(1000000)) + 7.88
print(y2)
##This regression equation suggests that there will ALSO be an estimated 8(8.01) people.
##This leads me to believe that there are too many othere factors that could alter these numbers.

##Would like to examine further.
##May need different data to look at...


#Stepwise Regression






