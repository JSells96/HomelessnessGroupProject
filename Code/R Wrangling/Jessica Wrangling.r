#Install and Load Libraries
install.packages('readr')
library('readr')
install.packages("tidyr")
install.packages("dplyr")
library('tidyr')
library('dplyr')
install.packages('readxl')
library('readxl')
install.packages('ggplot2')
install.packages('lattice')
library('ggplot2')
library('lattice')
install.packages("reshape")
library("reshape")

#Load Data 
NatlHomelessPops16_21 <- read_xlsx("../../Data/HomelessPopulationsReport2016-2021.xlsx")
NatlPopFacts <- read_xlsx("../../Data/homeless_population_usafacts.xlsx")
NatlPop <- read_csv("../../Data/nst-est2020.csv")


#Data Wrangle
##Rename Columns for Clarification
names(NatlPop)[names(NatlPop) == "NAME"] <- "State"
names(NatlPopFacts)[names(NatlPopFacts) == "Years"] <- "State"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2010"] <- "Est2010"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2011"] <- "Est2011"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2012"] <- "Est2012"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2013"] <- "Est2013"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2014"] <- "Est2014"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2015"] <- "Est2015"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2016"] <- "Est2016"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2017"] <- "Est2017"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2018"] <- "Est2018"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2019"] <- "Est2019"
names(NatlPop)[names(NatlPop) == "POPESTIMATE2020"] <- "Est2020"


##Subset using indexes
USTotal <- NatlPop[6:57, 5:19]
HomelessStates <- NatlPopFacts[31:81, ]


##Drop unnecessary columns
USTotalKeeps <- USTotal[-c(2:3)]
HomelessStatesKeeps <- HomelessStates[-c(2:28)]


##Remove (1) after each State
HomelessStatesKeeps$State <- gsub("[ (1)]", "", as.character(HomelessStatesKeeps$State))
##New package for this one
install.packages("stringr")
library("stringr")
str_replace_all(USTotalKeeps$State, " ", "")
#Didn't seem to work...
USTotalKeeps$State <- gsub(" ", "", as.character(USTotalKeeps$State))


##Separate for max function to run only on the numeric columns
homelessMax <- HomelessStatesKeeps[, -which(names(HomelessStatesKeeps) == "State")] %>% max(na.rm = FALSE)
sapply(HomelessStatesKeeps, max)
##Total Max = 161548 which was in 2020, of course
usMax <- USTotalKeeps[, -which(names(USTotalKeeps) == "State")] %>% max(na.rm = FALSE)
sapply(USTotalKeeps, max)
##Total Max = 329484123 which was also in 2020


##Check for proper variable type so I can combine
class(HomelessStatesKeeps$`2007`)
class(HomelessStatesKeeps$State)
class(USTotalKeeps$Est2010)
class(USTotalKeeps$State)


##Combine dataset
Totals <- merge(USTotalKeeps, HomelessStatesKeeps, by=c("State"), all=TRUE)


##Find max of columns to determine largest state populations
MaxStates2010 <- Totals %>% arrange(desc(Est2010)) %>% group_by(State)
MaxStates2020 <- Totals %>% arrange(desc(Est2020)) %>% group_by(State)
##The difference between the 10 years is a shifted order, but resulted in the same top 6 states.
head(MaxStates2010)
head(MaxStates2020)
##We see California is #1 always. We have Texas, Florida, New York, Pennsylvania and Illinois all taking turns for runners up. 

