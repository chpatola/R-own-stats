#PROBLEM 1

# We have a 12 samples for how many days there is between an (continously repeated, normally distributed) event takes place.
# Based on the samples, can we say that the average amount of days between the events is 30?

#H0: The population avarage for how often the event takes place is 30 days 
#H1: The population avarage for how often the event takes place is NOT 30 days 

#Import data
PData <-read.csv2("P data_.csv",stringsAsFactors =FALSE )
PData
str(PData)

#Choose only days
OnlyDays <- PData$Days
OnlyDays

#Chck mean and sd
mean(OnlyDays)
sd(OnlyDays)

#Test if averige is 30
t.test(OnlyDays, mu=30,alternative ="two.sided")
?t.test

#ANSWER 1: With a p value of 0.057, we cannot reject H1 at 5% significance level. However, it is possible at 10% significance level.
#If we choose 5% 5% significance level, we keep H1 and say that the average is 30 days.

#PROBLEM 2

#We have data on times worked out during specific weeks (normally distributed). We want to see if some other factors affect the amount of worked out times.

#2a)
#H0: The amount of hours spent at work does NOT affect amount of workout times
#H0: The amount of hours spent at work does affect amount of workout times

#2b)
#H0: None of the factors from the csv file affect amount of workout times
#H0: One or more of the factors from the csv file affect amount of workout times

#Import data
Workout <-read.csv2("workout.csv",stringsAsFactors =FALSE )
Workout

#Have  look at the 2a) data visually
Pic1 <-plot(Workout$Hours.at.work,Workout$Amount.of.times.worked.out)
#Does clearly not seem to be connected

#Create regression for 2a)
regr1 <- lm(Workout$Amount.of.times.worked.out~Workout$Hours.at.work)
summary(regr1)
abline(regr1)

#ANSWER 2a): The p value is 0.98 so H0 cannot be rejected. We say that The amount of hours spent at work does NOT affect amount of workout times

#Create regression for 2b)
regr2 <- lm(Workout$Amount.of.times.worked.out~Workout$Hours.studying.for.exam+Workout$Whole.days.booked.with.other.programme+Workout$Hours.at.work)
summary(regr2)

Pic2 <-plot(Workout$Whole.days.booked.with.other.programme,Workout$Amount.of.times.worked.out)
reg3 <- lm(Workout$Amount.of.times.worked.out~Workout$Whole.days.booked.with.other.programme)
abline(reg3)

#ANSWER 2b) The p value for all factors except Whole.days.booked.with.other.programme are >0.05.
#Only Whole.days.booked.with.other.programme effects Amount.of.times.worked.out. The effect is negative. 
#We can reject H0 and say that One factor from the csv file affects amount of workout times

#Problem 3

# A married couple wants to go on a date at least once/month. Below is the data for year 2018. 
# Does the amount of dates during one month vary significantly more than what can be expected, taking normal variation into account?

dates_per_month <- data.frame(month=1:12,dates=c(1,1,2,2,1,2,5,1,2,2,1,2))
dates_per_month

# We make an XmR chart of the data above
library(ggplot2)
library(ggQC)

XmR_PlotOfDates <- ggplot(dates_per_month, aes(x = month, y = dates)) +
  geom_point() + geom_line() + 
  stat_QC(method = "XmR")

XmR_PlotOfDates

# We can see that the amount of dates for the 7th month (July) falls just outside the upper natural process limit.
# The reason is, most likely, that the couple was on vacation in July and therefore had more time together then.

# We can also create a mR chart to have a closer look at the variation

mR_PlotOfDates <- ggplot(dates_per_month, aes(x = month, y = dates)) +
  stat_mR() + 
  stat_QC_labels(method="mR")

mR_PlotOfDates

# Here we can clearly see that the variation for the other months than July have been quite small but it makes a big jump between July and August.
# The big jump also makes the central line lay higher than it would without this "outlier".

# ANSWER: The amount of Dates in July differs significantly from what can be seen a normal variation.
