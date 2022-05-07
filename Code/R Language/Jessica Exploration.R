##Load Libraries
library('readxl')
library('dplyr')
library('ggplot2')
library('Ecdat')
library('ggrepel')
library("PerformanceAnalytics")


##Load new data for easier graphing
StateHomelessPop <- read_xlsx("/Users/jessicasells/Desktop/School/Final/Datasets/HomelessTotals.xlsx")


##Data Wrangling
StateHomelessPopClean <- StateHomelessPop[, 16:18]


##Plot to see if the increase truly has been steady over the last 15 years
ggplot(StateHomelessPopClean, aes(x= Year, y= HomelessPopulation)) + geom_point() + geom_smooth(method=lm)
##This shows that the numbers are not consistently uprising but can't see any particular pattern.


##Look at largest populated states by number of Homeless and compare to numbers in Totals
StateHomelessPopClean %>% group_by(State) %>% summarize(Mean = mean(HomelessPopulation)) %>% arrange(desc(Mean))
##This shows different states than what we found before in our initial Maxes found in our wrangling last week because we were looking at the total populations

##I want to see that top 6 across the years.
data_ends <- StateHomelessPopClean %>% filter(Year == "2020")
ggplot(StateHomelessPopClean, aes(Year,HomelessPopulation)) + 
  geom_line(aes(colour = State)) +
  geom_text_repel(aes(label = State), data = data_ends) +
  theme(legend.position="none")
##Here it shows the top 6 on average is California, New York, Florida, Texas, Washington and Massachusetts


##Look at correlations of the total populations to the homeless populations
ggplot(Totals, aes(x= Est2010, y= `2010`)) + geom_point() + geom_smooth(method=lm)
cor.test(Totals$Est2010, Totals$`2010`, method="pearson", use = "complete.obs")
###Looks to be a significant and positive correlation
##I assume it should be the same for all the other years. Let's look at those another way below..
Totals_quant <- Totals[, c(2,3,4,5,6,7,8,9,10,11,13,17,18,19,20,21,22,23,24,25,26,27)]
chart.Correlation(Totals_quant, histogram=FALSE, method="pearson")
##Here we can see that there is a large amount of correlation among these numbers, but that is not surprising being that they are measuring across years.


##I want to see all the averages and make them into a Dataframe of their own
Totals1 <- Totals[-40,]
Homeless2010 <- Totals1 %>% group_by(State) %>% summarize(Mean2010 = mean(`2010`)) %>% arrange(desc(Mean2010))
Homeless2011 <- Totals1 %>% group_by(State) %>% summarize(Mean2011 = mean(`2011`)) %>% arrange(desc(Mean2011))
Homeless2012 <- Totals1 %>% group_by(State) %>% summarize(Mean2012 = mean(`2012`)) %>% arrange(desc(Mean2012))
Homeless2013 <- Totals1 %>% group_by(State) %>% summarize(Mean2013 = mean(`2013`)) %>% arrange(desc(Mean2013))
Homeless2014 <- Totals1 %>% group_by(State) %>% summarize(Mean2014 = mean(`2014`)) %>% arrange(desc(Mean2014))
Homeless2015 <- Totals1 %>% group_by(State) %>% summarize(Mean2015 = mean(`2015`)) %>% arrange(desc(Mean2015))
Homeless2016 <- Totals1 %>% group_by(State) %>% summarize(Mean2016 = mean(`2016`)) %>% arrange(desc(Mean2016))
Homeless2017 <- Totals1 %>% group_by(State) %>% summarize(Mean2017 = mean(`2017`)) %>% arrange(desc(Mean2017))
Homeless2018 <- Totals1 %>% group_by(State) %>% summarize(Mean2018 = mean(`2018`)) %>% arrange(desc(Mean2018))
Homeless2019 <- Totals1 %>% group_by(State) %>% summarize(Mean2019 = mean(`2019`)) %>% arrange(desc(Mean2019))
Homeless2020 <- Totals1 %>% group_by(State) %>% summarize(Mean2020 = mean(`2020`)) %>% arrange(desc(Mean2020))

##Combine these numbers
TopStateAvgs1 <- merge(Homeless2010, Homeless2011, by=c("State"), all=TRUE)
TopStateAvgs2 <- merge(TopStateAvgs1, Homeless2012, by=c("State"), all=TRUE)
TopStateAvgs3 <- merge(TopStateAvgs2, Homeless2013, by=c("State"), all=TRUE)
TopStateAvgs4 <- merge(TopStateAvgs3, Homeless2014, by=c("State"), all=TRUE)
TopStateAvgs5 <- merge(TopStateAvgs4, Homeless2015, by=c("State"), all=TRUE)
TopStateAvgs6 <- merge(TopStateAvgs5, Homeless2016, by=c("State"), all=TRUE)
TopStateAvgs7 <- merge(TopStateAvgs6, Homeless2017, by=c("State"), all=TRUE)
TopStateAvgs8 <- merge(TopStateAvgs7, Homeless2018, by=c("State"), all=TRUE)
TopStateAvgs9 <- merge(TopStateAvgs8, Homeless2019, by=c("State"), all=TRUE)
TopStateAvgs <- merge(TopStateAvgs9, Homeless2020, by=c("State"), all=TRUE)
View(TopStateAvgs)
