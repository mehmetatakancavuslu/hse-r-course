## COVID-19 Cases Outside Hubei Provence, China
library(dplyr)
library(ggplot2)

# Data pre-processing
covid19 <- read.csv("2019_nCoV_data.csv")
summary(covid19)
covid19$Last.Update <- NULL
covid19$Sno <- NULL
head(covid19)
covid19$Date <- format(as.Date(covid19$Date, format = "%m/%d/%Y %H:%M:%S"),
                       "%d-%m-%Y")
head(covid19)
covid19[covid19$Country == "Mainland China",]$Country <- "China"

# Getting the total number of cases in each country
country_df <- covid19 %>% group_by(Country) %>% summarise(Confirmed = sum(Confirmed),
                                   Recovered = sum(Recovered),
                                   Deaths = sum(Deaths))
