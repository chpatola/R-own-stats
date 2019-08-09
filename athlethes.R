
#Problem 1

#I have a table with info about crossfit athletes and would like to find out more about the relationship between the variables
crossfit <-read.csv(file="athletes.csv")
crossfit
str(crossfit)

#Correct error in column other sport, row 12 and check that it is correct
crossfit[12,5] <-"no"
crossfit
crossfit$other.sport <-factor(crossfit$other.sport)
summary(crossfit$other.sport)
#***********************************

#Sort table according to deadlift Max -> Min
Sorted_DL<-sort(crossfit$deadlift.1RM, decreasing=TRUE)
Sorted_DL
Ordered_DL <-order(crossfit$deadlift.1RM, decreasing = TRUE)
Ordered_DL

crossfit <-crossfit[Ordered_DL,]
crossfit
#***********************************
#Average value for age, height, weight separate for sexes
Cwomen <- subset(crossfit,sex=="woman")
CwomanAHW_mean <- apply(Cwomen[,2:4],2,mean)
round(CwomanAHW_mean, digits=2)

Cmen <- subset(crossfit,sex=="man")
CmanAHW_mean <- apply(Cmen[,2:4],2,mean)
round(CmanAHW_mean, digits =2)
#***********************************
#Are crossfit women generally shorter than crossfit men?
#We know that height in general is normally distrubuted. We could, however, also use the shapiro.test to test this.
#Assume confidence level 90 %

Wheights <-Cwomen[,"height"]
Wheights

hist(Wheights, col="pink",breaks = seq(150,180, by=5),main="Crossfit women heights")
shapiro.test(Wheights) 

Mheights <-Cmen[,"height"]
Mheights

hist(Mheights, col="purple",main="Crossfit men heights")
shapiro.test(Mheights) 

#Both shapiro.tests p values are larger than 0.1 -> we cannot reject the hypothesis that they are normally distributed

t.test(Wheights,Mheights, alternativ="less")
#Whith a p value below 0.05, we can say reject the hypothesis that crossfit males are of same or smaller height than crossfit women

#***********************************
#Overview on how different values correlate with each other
#Except exkcluding non numerical values, I also exclude "Isabel" due to the vast ampunt of NA:s

?cor

corey <- cor(crossfit[c(-1,-5,-7)],use="na.or.complete")
corey
#height correlates with weight and both weight and length correlate with deadlift 1RM
#***********************************

#How strongly does BMI correlate with how much an athlete can take in deadlift?
#We start by counting BMI

bmi <- function(we,he){
  result<- round((we/((he/100)*(he/100))),digits=2)
}

bmi_results <- bmi(crossfit$weight,crossfit$height)

bmi_results

crossfit <- cbind(crossfit,bmi_results)
crossfit

coreyBMI <-cor(crossfit$deadlift.1RM, crossfit$bmi_results,use="na.or.complete")
coreyBMI
#The BMI correlation with deadlift seems to be a bit weaker than the one for weight and deadlift
#but a bit stronger than for height and deadlift

#install.packages("ggplot2")
library(ggplot2)
ggplot(crossfit, aes(x=bmi_results,y=deadlift.1RM)) +geom_point()+stat_smooth(method=lm)+
ggtitle("Relation between athlete BMI and deadlift 1RM results")
#**********************************

#Could the sport the athlete has done before affect how much she/he can take in deadlift?
#women
sport_deadlift_woman <- Cwomen[,5:6]
sport_deadlift_woman 
sport_deadlift_woman$other.sport <- as.factor(sport_deadlift_woman$other.sport)
summary(sport_deadlift_woman$other.sport)

summary(sport_deadlift_woman)

plot(sport_deadlift_woman, main="Previous sport and deadlift 1RM, women",ylab="kg deadlift")
text(sport_deadlift_woman$other.sport,labels=names(sport_deadlift_woman$other.sport))


#men
sport_deadlift_man <- Cmen[,5:6]
sport_deadlift_man
plot(sport_deadlift_man, main="Previous sport and deadlift 1RM, men",ylab="kg deadlift")

#A look at the vizualisations tells us it would be difficult to draw any conslusions about possible relationship based on our sample



