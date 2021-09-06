#Homework one
#Due September 1, 2021

rm(list=ls())

library(tidyverse)

#Function####
DIF<- function(x){
  temp<-(x-lag(x))
  return(temp)
}

library(readr)
ILCovid19 <- read_csv("~/Desktop/ECON 691/Data Files/ILCovid19.csv", show_col_types = FALSE)

covidIL<-ILCovid19 %>%
  mutate(new_tests=DIF(Tests),
         new_positives=DIF(Positives),
         new_deaths=DIF(Deaths))

delta<-function(x){
  temp<-((x-lag(x))/lag(x))
  return(temp)
}

covidIL<-covidIL %>%
mutate(pc_tests=delta(new_tests),
       pc_positives=delta(new_positives),
       pc_deaths=delta(new_deaths))

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

