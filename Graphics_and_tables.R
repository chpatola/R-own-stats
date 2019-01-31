# A hairdresser has three services which each has a different price for different customers.
# The three services are: cut, color and wash&dry
# The three customer types are: long hair, short hair and children
# The prices for cut are: 30, 50 and 20 €, for color 60,80 and 50 € and for wash&dry 20,20 and 15 €

# 1: Create a matrix with the prices for the different combinations

HairdresserPrices <-matrix(c(30,60,20,50,80,20,20,50,15),ncol=3)
rownames(HairdresserPrices)<-c("cut","color","wash")
colnames(HairdresserPrices)<-c("short", "long","child")
HairdresserPrices

#Checking that we have the right amount of columns, rows and data entires and aggregated data
nrow(HairdresserPrices)
ncol(HairdresserPrices)
length(HairdresserPrices)

rowSums(HairdresserPrices)
colSums(HairdresserPrices)
mean(HairdresserPrices)


#Create a data frame with info on the clients and services sold from 11.1.2109

ClientName <-c("Suzie","Beth","John","Deborah","Billie")
clientType <- c("child","short hair","long hair", "long hair","short hair")
services <-c("cut","color","color","wash&dry","cut")
income <-c(20,60,80,20,30)

business2019_01_11 <-data.frame(clientType,services,income, row.names = ClientName)
business2019_01_11

#Create function where we can see what the VAT is for each sales

Vat <- function(x){
  vatfreesum <- (x/1.24)
  vatsum <-round(x-vatfreesum)
  vatsumText <-paste("The VAT for sale",x, "€ is",vatsum,"€")
  return(vatsumText)
}

Vat(124)

#Use the function Vat on the price list

apply(HairdresserPrices,1,Vat)

