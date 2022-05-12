##Load Libraries
library('readxl')
library('dplyr')
library('ggplot2')
library('Ecdat')
library('ggrepel')
library("PerformanceAnalytics")


##Load new data for easier graphing
StateHomelessPop <- read_xlsx("../../Data/HomelessTotals.xlsx")


##Data Wrangling
StateHomelessPopClean <- StateHomelessPop[, 16:18]


##Plot to see if the increase truly has been steady over the last 15 years
ggplot(StateHomelessPopClean, aes(x= Year, y= HomelessPopulation)) + geom_point() + geom_smooth(method=lm)
##This shows that the numbers are not consistently uprising but can't see any particular pattern.


##Look at largest populated states by number of Homeless and compare to numbers in Totals
StateHomelessPopClean %>% group_by(State) %>% summarize(Mean = mean(HomelessPopulation)) %>% arrange(desc(Mean))
##This shows different states than what we found before in our initial Maxes found in our wrangling last week because we were looking at the total populations


##Top States in total population
TotalPopStates2010 <- USTotalKeeps %>% arrange(desc(Est2010)) %>% group_by(State)
head(TotalPopStates2010)
##CA, TX, NY, FL, IL, PA
TotalPopStates2011 <- USTotalKeeps %>% arrange(desc(Est2011)) %>% group_by(State)
head(TotalPopStates2011)
##CA, TX, NY, FL, IL, PA
TotalPopStates2012 <- USTotalKeeps %>% arrange(desc(Est2012)) %>% group_by(State)
head(TotalPopStates2012)
##CA, TX, NY, FL, IL, PA
TotalPopStates2013 <- USTotalKeeps %>% arrange(desc(Est2013)) %>% group_by(State)
head(TotalPopStates2013)
##CA, TX, NY, FL, IL, PA
TotalPopStates2014 <- USTotalKeeps %>% arrange(desc(Est2014)) %>% group_by(State)
head(TotalPopStates2014)
##CA, TX, NY, FL, IL, PA
TotalPopStates2015 <- USTotalKeeps %>% arrange(desc(Est2015)) %>% group_by(State)
head(TotalPopStates2015)
##Changes in 2015
##CA, TX, FL, NY, IL, PA
TotalPopStates2016 <- USTotalKeeps %>% arrange(desc(Est2016)) %>% group_by(State)
head(TotalPopStates2016)
##Same as year before, Florida is now #3
##CA, TX, FL, NY, IL, PA
TotalPopStates2017 <- USTotalKeeps %>% arrange(desc(Est2017)) %>% group_by(State)
head(TotalPopStates2017)
##Pennsylvania's numbers rose
##CA, TX, FL, NY, PA, IL
TotalPopStates2018 <- USTotalKeeps %>% arrange(desc(Est2018)) %>% group_by(State)
head(TotalPopStates2018)
##Same standings as the year before 
##CA, TX, FL, NY, PA, IL
TotalPopStates2019 <- USTotalKeeps %>% arrange(desc(Est2019)) %>% group_by(State)
head(TotalPopStates2019)
##CA, TX, FL, NY, PA, IL
TotalPopStates2020 <- USTotalKeeps %>% arrange(desc(Est2020)) %>% group_by(State)
head(TotalPopStates2020)
##Same standings

##Let's see the Homeless Population in comparison to these numbers
HomelessPopStates2010 <- HomelessStatesKeeps %>% arrange(desc(`2010`)) %>% group_by(State)
head(HomelessPopStates2010)
##CA, NY, FL, TX, WA, GA
##These numbers show that the largest total populated State does not always mean the largest homeless population
HomelessPopStates2011 <- HomelessStatesKeeps %>% arrange(desc(`2011`)) %>% group_by(State)
head(HomelessPopStates2011)
##Has shifted a little with Georgia coming in 5th and Washington going down to 6th
##CA, NY, FL, TX, GA, WA
HomelessPopStates2012 <- HomelessStatesKeeps %>% arrange(desc(`2012`)) %>% group_by(State)
head(HomelessPopStates2012)
##Same standings as the previous year
##CA, NY, FL, TX, GA, WA
HomelessPopStates2013 <- HomelessStatesKeeps %>% arrange(desc(`2013`)) %>% group_by(State)
head(HomelessPopStates2013)
##Top 4 are keeping straight but 5th has now changed and Georgia has bumped out of the top states
##CA, NY, FL, TX, MA, WA
HomelessPopStates2014 <- HomelessStatesKeeps %>% arrange(desc(`2014`)) %>% group_by(State)
head(HomelessPopStates2014)
##CA, NY, FL, TX, MA, WA
HomelessPopStates2015 <- HomelessStatesKeeps %>% arrange(desc(`2015`)) %>% group_by(State)
head(HomelessPopStates2015)
##CA, NY, FL, TX, MA, WA
HomelessPopStates2016 <- HomelessStatesKeeps %>% arrange(desc(`2016`)) %>% group_by(State)
head(HomelessPopStates2016)
##Pulled another switch of the 5th and 6th position
##CA, NY, FL, TX, WA, MA
HomelessPopStates2017 <- HomelessStatesKeeps %>% arrange(desc(`2017`)) %>% group_by(State)
head(HomelessPopStates2017)
##CA, NY, FL, TX, WA, MA
HomelessPopStates2018 <- HomelessStatesKeeps %>% arrange(desc(`2018`)) %>% group_by(State)
head(HomelessPopStates2018)
##CA, NY, FL, TX, WA, MA
HomelessPopStates2019 <- HomelessStatesKeeps %>% arrange(desc(`2019`)) %>% group_by(State)
head(HomelessPopStates2019)
##CA, NY, FL, TX, WA, MA
HomelessPopStates2020 <- HomelessStatesKeeps %>% arrange(desc(`2020`)) %>% group_by(State)
head(HomelessPopStates2020)
##Seems pretty consistent at this point. 
##CA, NY, FL, TX, WA, MA


##I want to see that top 6 across the years.
data_ends <- StateHomelessPopClean %>% filter(Year == "2020")
##made table for labeling purposes
ggplot(StateHomelessPopClean, aes(Year,HomelessPopulation)) + 
  ##sort by States
  geom_line(aes(colour = State)) +
  ##label properly so not cluttered
  geom_text_repel(aes(label = State), data = data_ends) +
  ##have to remove legend on side because it is so much info
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

