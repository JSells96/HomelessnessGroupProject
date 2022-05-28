##Load libraries
library('readr')
install.packages('mvnormalTest')
library('mvnormalTest')
library('ggplot2')
library("car")
library("caret")
library("gvlma")
library("predictmeans")

##Load Data
Income21 <- read_csv("/Users/jessicasells/Desktop/School/Final/Datasets/hhpub21.csv")
Income20 <- read_csv("/Users/jessicasells/Desktop/School/Final/Datasets/hhpub20.csv")
Income19 <- read_csv("/Users/jessicasells/Desktop/School/Final/Datasets/hhpub19.csv")


##Data wrangling
##subset new dataframe by keeping certain columns
Income21Keeps <- data.frame(Income21$H_LIVQRT, Income21$H_TENURE, Income21$H_NUMPER, Income21$HHINC, Income21$HDIS_YN, Income21$HINC_UC, Income21$HPAW_YN)
Income20Keeps <- data.frame(Income20$H_LIVQRT, Income20$H_TENURE, Income20$H_NUMPER, Income20$HHINC, Income20$HDIS_YN, Income20$HINC_UC, Income20$HPAW_YN)
Income19Keeps <- data.frame(Income19$H_LIVQRT, Income19$H_TENURE, Income19$H_NUMPER, Income19$HHINC, Income19$HDIS_YN, Income19$HINC_UC, Income19$HPAW_YN)


##rename columns
names(Income21Keeps)[names(Income21Keeps) == "Income21.H_LIVQRT"] <- "TypeOfHousing21"
names(Income21Keeps)[names(Income21Keeps) == "Income21.H_TENURE"] <- "RentOrOwn21"
names(Income21Keeps)[names(Income21Keeps) == "Income21.H_NUMPER"] <- "PersonsInHousehold21"
names(Income21Keeps)[names(Income21Keeps) == "Income21.HHINC"] <- "HouseholdIncomeAmt21"
names(Income21Keeps)[names(Income21Keeps) == "Income21.HDIS_YN"] <- "Disability21"
names(Income21Keeps)[names(Income21Keeps) == "Income21.HINC_UC"] <- "Unemployment21"
names(Income21Keeps)[names(Income21Keeps) == "Income21.HPAW_YN"] <- "Welfare21"
names(Income20Keeps)[names(Income20Keeps) == "Income20.H_LIVQRT"] <- "TypeOfHousing20"
names(Income20Keeps)[names(Income20Keeps) == "Income20.H_TENURE"] <- "RentOrOwn20"
names(Income20Keeps)[names(Income20Keeps) == "Income20.H_NUMPER"] <- "PersonsInHousehold20"
names(Income20Keeps)[names(Income20Keeps) == "Income20.HHINC"] <- "HouseholdIncomeAmt20"
names(Income20Keeps)[names(Income20Keeps) == "Income20.HDIS_YN"] <- "Disability20"
names(Income20Keeps)[names(Income20Keeps) == "Income20.HINC_UC"] <- "Unemployment20"
names(Income20Keeps)[names(Income20Keeps) == "Income20.HPAW_YN"] <- "Welfare20"
names(Income19Keeps)[names(Income19Keeps) == "Income19.H_LIVQRT"] <- "TypeOfHousing19"
names(Income19Keeps)[names(Income19Keeps) == "Income19.H_TENURE"] <- "RentOrOwn19"
names(Income19Keeps)[names(Income19Keeps) == "Income19.H_NUMPER"] <- "PersonsInHousehold19"
names(Income19Keeps)[names(Income19Keeps) == "Income19.HHINC"] <- "HouseholdIncomeAmt19"
names(Income19Keeps)[names(Income19Keeps) == "Income19.HDIS_YN"] <- "Disability19"
names(Income19Keeps)[names(Income19Keeps) == "Income19.HINC_UC"] <- "Unemployment19"
names(Income19Keeps)[names(Income19Keeps) == "Income19.HPAW_YN"] <- "Welfare19"


##Number of occurrences of each coded variable
##0 = niu, 1 = yes, 2 = no
table(Income21Keeps$Disability21)
table(Income21Keeps$Unemployment21)
table(Income21Keeps$Welfare21)
##01 = House, apt., flat, 02 = HU in nontransient hotel, etc., 03 = HU, perm, in trans. hotel, motel, etc., 04 = HU in rooming house, 05 = Mobile home or trailer with no permanent room added, 06 = Mobile home or trailer with 1 or more perm rooms added, 07 = HU not specified above, 08 = Qtrs not hu in rooming or boarding house, 09 = Unit not perm in trans. hotel, motel, etc., 10 = Tent or trailer site, 11 = Student quarters in college dormitory, 12 = Other not HU
table(Income21Keeps$TypeOfHousing21)
##0=Not in universe, 1=Owned or being bought, 2=Rented, 3=No cash rent
table(Income21Keeps$RentOrOwn21)
##00=Noninterview household, 01-15 = Number of persons in HHLD
table(Income21Keeps$PersonsInHousehold21)



##Visualize the difference between the years
ggplot(Income21Keeps, aes(x = Disability21)) + geom_histogram(binwidth = 1) + ggtitle("Disabled Population in 2021")
ggplot(Income19Keeps, aes(x = Disability19)) + geom_histogram(binwidth = 1) + ggtitle("Disabled Population in 2019")
ggplot(Income21Keeps, aes(x = Unemployment21)) + geom_histogram(binwidth = 1) + ggtitle("Unemployment in 2021")
ggplot(Income19Keeps, aes(x = Unemployment19)) + geom_histogram(binwidth = 1) + ggtitle("Unemployment in 2019")
ggplot(Income21Keeps, aes(x = Welfare21)) + geom_histogram(binwidth = 1) + ggtitle("Welfare in 2021")
ggplot(Income19Keeps, aes(x = Welfare19)) + geom_histogram(binwidth = 1) + ggtitle("Welfare in 2019")
##These are not showing as in depth as we need, so we will stick to the numbers instead for emphasis.


##Let's see the differences in these numbers. But keep in mind, we are looking at recoded categories
summary(Income21Keeps$HouseholdIncomeAmt21)
summary(Income20Keeps$HouseholdIncomeAmt20)
summary(Income19Keeps$HouseholdIncomeAmt19)
table(Income20Keeps$HouseholdIncomeAmt20)
table(Income20Keeps$HouseholdIncomeAmt20)
table(Income19Keeps$HouseholdIncomeAmt19)


#Correlation
chart.Correlation(Income21Keeps, histogram=FALSE, method="pearson")


write.csv(Totals, "/Users/jessicasells/Desktop/School/Final/Datasets/Totals.csv", row.names = TRUE)


