library(tidyverse)
library(lubridate)

#Reading in and cleaning weather and population data
weather <- read.csv("./weath.csv")
pop <- read.csv("./popo.csv") 
pop <- pop %>% 
  mutate(Province_State = str_replace(State, ".", "")) 
us <- weather %>% 
  filter(Country_Region == 'US') %>% 
  mutate(newcases = newcase(ConfirmedCases))

#Function for calculating daily new cases
newcase <- function(cases) {
  size <- length(cases)
  new <- cases
  for (i in 2:size) {
    if ((cases[i] - cases[i-1]) >= 0) {
    new[i] <- cases[i] - cases[i-1]
    }
    else {
      new[i] = 0
    }
  }
  
  return(new)
}

#Merging weather and population data to create confirmed cases proportion variable
merged <- merge(us, pop, by = "Province_State")

merged <- merged %>% 
  mutate(prop = as.numeric(ConfirmedCases)/as.numeric(trimws(gsub(",", "", as.character(pop[1,2])))),
         newprop = newcases/as.numeric(trimws(gsub(",", "", as.character(pop[1,2]))))) %>% 
  select(-c(stp, slp, dewp, rh, ah, day_from_jan_first, country.province, Id))

#Creating new dataframe with average windspeeds variable
winds <- merged %>% 
  group_by(Province_State) %>% 
  filter(wdsp < 100) %>% 
  summarise(avgwdsp = mean(wdsp))

#Creating new dataframe with average precipitation variable
rain <- merged %>% 
  group_by(Province_State) %>% 
  filter(prcp < 10) %>% 
  summarise(avgprcp = mean(prcp))
  
#Creating data frame with just the last day of the time interval for each state
last <- merged %>% 
  filter(as.character(Date) == "2020-04-08")

#Merging dataframes to create datasets with averages for each meteorological condition
#relating them to cumulative confirmed cases count
windy <- merge(winds, last, by = "Province_State")

rainy <- merge(rain, last, by = "Province_State")

#Scatterplots relating each condition to cumulative confirmed cases count
windy %>% 
  group_by(Province_State) %>% 
  ggplot() +
  geom_point(mapping = aes(avgwdsp, prop, color = Province_State)) +
  xlab("Confirmed Cases as a Ratio of Total Population") +
  ylab("Average Windspeed") +
  ggtitle("Cumulative Confirmed Cases Relative to Total Population by Average Windspeed")

rainy %>% 
  group_by(Province_State) %>% 
  ggplot() +
  geom_point(mapping = aes(avgprcp, prop, color = Province_State)) +
  xlab("Confirmed Cases as a Ratio of Total Population") +
  ylab("Average Precipitation") +
  ggtitle("Cumulative Confirmed Cases Relative to Total Population by Average Precipitation")

#Scatterplot relating daily temperature to daily new cases as a ratio of total population
merged %>% 
  filter(newcases>0) %>% 
  ggplot() +
  geom_point(mapping = aes(newprop, temp, color = Province_State)) +
  xlab("New Cases Relative as a Ratio of Population") +
  ylab("Average Daily Temperature") +
  ggtitle("Daily New Cases Relative to Total Population by Daily Temperature")


#Linear Regression Analysis
win <- lm(prop~avgwdsp, data=windy)
summary(win)

temp <- lm(newprop~temp, data=merged)
summary(temp)

prec <- lm(prop~avgprcp, data = rainy)
summary(prec)

#Residual Plots for regression assumptions
par(mfcol=c(2,2))
qqPlot(rstudent(temp),pch=16,envelope=FALSE)
hist(rstudent(temp),main="")
residualPlots(temp,type="rstudent",terms=~1,quadratic=FALSE,pch=16)
plot(rstudent(temp),type="o",pch=16)
abline(h=0,lty="dashed")
par(mfcol=c(1,1))

par(mfcol=c(2,2))
qqPlot(rstudent(win),pch=16,envelope=FALSE)
hist(rstudent(win),main="")
residualPlots(win,type="rstudent",terms=~1,quadratic=FALSE,pch=16)
plot(rstudent(win),type="o",pch=16)
abline(h=0,lty="dashed")
par(mfcol=c(1,1))

par(mfcol=c(2,2))
qqPlot(rstudent(prec),pch=16,envelope=FALSE)
hist(rstudent(prec),main="")
residualPlots(prec,type="rstudent",terms=~1,quadratic=FALSE,pch=16)
plot(rstudent(prec),type="o",pch=16)
abline(h=0,lty="dashed")
par(mfcol=c(1,1))