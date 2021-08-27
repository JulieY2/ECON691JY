#Homework one
#Due September 1, 2021

rm(list=ls())

library(tidyverse)

#Function####
DIF<- function(x){
  temp<-((x-lag(x))/x)
  return(temp)
}

library(readr)
ILCovid19 <- read_csv("~/Desktop/ECON 691/Data Files/ILCovid19.csv", show_col_types = FALSE)

covidIL<-ILCovid19 %>%
  mutate(pc_tests=DIF(Tests),
         pc_positives=DIF(Positives),
         pc_deaths=DIF(Deaths))

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

covidIL$Date<-as.Date(covidIL$Date, format="%m/%d/%Y")

plot(covidIL$Date, covidIL$pc_tests)

plot(covidIL$Date, covidIL$pc_tests,
     main = "Daily Percentage Change in Tests",
     xlab = "",
     ylab = "",
     type = "l",
     col = "green")

plot(covidIL$Date, covidIL$pc_positives)

plot(covidIL$Date, covidIL$pc_positives,
     main = "Daily Percentage Change in Cases",
     xlab = "",
     ylab = "",
     type = "l",
     col = "pink")

plot(covidIL$Date, covidIL$pc_deaths)

plot(covidIL$Date, covidIL$pc_deaths,
     main = "Daily Percentage Change in Deaths",
     xlab = "",
     ylab = "",
     type = "l",
     col = "red")
