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


setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue CFR PM2.5')
Dengue <- read.csv("PM2.5_and_dengue_CFR_20_countires_20250604.csv")

datadeng <- data.fra(Dengue$PM2.5, Dengue$CFR, Dengue$Rainfall, Dengue$Temp, Dengue$Urban, Dengue$Pop.density, Dengue$GDP)
res <- cor(datadeng)
round(res, 2)

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

# fit <- glm(CFRss ~ PM2.5ss + Rainfallss + Tempss
#                + Urbanss + Pop.densityss + Diabetesss
#                + Hypertension.ss + GDPss ,
#                na.action=na.omit, data = Dengue)


library(car)
summary(fit)
round(exp(confint(fit)),2)
options(scipen = 999) 
performance::performance(fit)





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
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue CFR PM2.5')

worldDeng <- read.csv("Dengue_PM2.5_Map.csv")

# worldDeng$Caselog <- log10(worldDeng$Cases+1)
# worldDeng$Deathlog <- log10(worldDeng$Deaths+1)

# COVID2022$location[COVID2022$location == 'United States'] <- 'USA'
# COVID2022$location[COVID2022$location == 'United Kingdom'] <- 'UK'
# COVID2022$location[COVID2022$location == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'
# COVID2022$location[COVID2022$location == 'Congo'] <- 'Republic of Congo'


library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot


worldgovt <- dplyr::select(worldDeng, region = Country, DC = CFR)
head(worldgovt)


## Make the HDI numeric
worldgovt$DC <- as.numeric(as.character(worldgovt$DC))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

x <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = DC)) +
  scale_fill_distiller(palette ="YlOrRd", direction = 1) + # or direction=1
  ggtitle("Average Dengue CFR (%) from 2000–2024") + labs(fill = "CFR") +
  plain
x <- plot(x)
x


#Death

# worldDeng <- read.csv("Dengue_Data_2024.csv")
# worldDeng$Deathlog <- log10(worldDeng$Deaths+1)

# COVID2022$location[COVID2022$location == 'United States'] <- 'USA'
# COVID2022$location[COVID2022$location == 'United Kingdom'] <- 'UK'
# COVID2022$location[COVID2022$location == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'
# COVID2022$location[COVID2022$location == 'Congo'] <- 'Republic of Congo'


library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#Deaths
worldgovt <- dplyr::select(worldDeng, region = Country, DD = PM2.5)
head(worldgovt)


## Make the HDI numeric
worldgovt$DC <- as.numeric(as.character(worldgovt$DD))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

y <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = DD)) +
  scale_fill_distiller(palette ="YlOrRd", direction = 1) + # or direction=1
  ggtitle(expression("Average PM"[2.5]~"from 2000–2024")) + labs(fill = "PM"[2.5]~"") +
  plain
y <- plot(y)
y





library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#Deaths
worldgovt <- dplyr::select(worldDeng, region = Country, DD = GDP)
head(worldgovt)


## Make the HDI numeric
worldgovt$DC <- as.numeric(as.character(worldgovt$DD))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

z <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = DD)) +
  scale_fill_distiller(palette ="YlOrRd", direction = 1) + # or direction=1
  ggtitle(expression("Average GDP (in thousands) from 2000–2024")) + labs(fill = "GDP") +
  plain
z <- plot(z)
z


library(gridExtra)
tiff("Dengue_PM_Map.tiff", units="in", width=5, height=6, res=300)
library(cowplot)
gridExtra::grid.arrange(plot_grid(x, y, z, labels = "AUTO", ncol = 1, nrow = 3))
dev.off()

