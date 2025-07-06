library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(glmmTMB)
library(DHARMa)
library(performance)


setwd('D:\\Nayeem\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue CFR PM2.5')
Dengue <- read.csv("PM2.5_and_dengue_CFR_20_countires_20250604.csv")

#Normalization
Dengue$CFRss <- (Dengue$CFR - mean(Dengue$CFR))/sd(Dengue$CFR)
Dengue$PM2.5ss <- (Dengue$PM2.5 - mean(Dengue$PM2.5))/sd(Dengue$PM2.5)
Dengue$Rainfallss <- (Dengue$Rainfall - mean(Dengue$Rainfall))/sd(Dengue$Rainfall)
Dengue$RHss <- (Dengue$RH - mean(Dengue$RH))/sd(Dengue$RH)
Dengue$Tempss <- (Dengue$Temp - mean(Dengue$Temp))/sd(Dengue$Temp)
Dengue$Urbanss <- (Dengue$Urban - mean(Dengue$Urban))/sd(Dengue$Urban)
Dengue$Pop.densityss <- (Dengue$Pop.density - mean(Dengue$Pop.density))/sd(Dengue$Pop.density)
Dengue$Obesity.ss <- (Dengue$Obesity. - mean(Dengue$Obesity.))/sd(Dengue$Obesity.)
Dengue$Diabetesss <- (Dengue$Diabetes - mean(Dengue$Diabetes))/sd(Dengue$Diabetes)
Dengue$Hypertension.ss <- (Dengue$Hypertension. - mean(Dengue$Hypertension.))/sd(Dengue$Hypertension.)
Dengue$oldagess <- (Dengue$oldage - mean(Dengue$oldage))/sd(Dengue$oldage)
Dengue$GDPss <- (Dengue$GDP - mean(Dengue$GDP))/sd(Dengue$GDP)
Dengue$Yearss <- (Dengue$Year - mean(Dengue$Year))/sd(Dengue$Year)

fit <- glmmTMB(CFRss ~ PM2.5ss + Rainfallss + Tempss
               + Urbanss + Pop.densityss + Diabetesss
               + Hypertension.ss + GDPss + (1|Region) + (1|Yearss), 
               na.action=na.omit, data = Dengue)

fit <- glm(CFRss ~ PM2.5ss + Rainfallss + Tempss
               + Urbanss + Pop.densityss + Diabetesss
               + Hypertension.ss + GDPss , 
               na.action=na.omit, data = Dengue)


library(car)
summary(fit)
round(exp(confint(fit)),2)
options(scipen = 999) 
performance::performance(fit)
