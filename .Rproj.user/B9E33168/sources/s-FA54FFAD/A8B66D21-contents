library(dplyr)

# Ingesting Nursing Data from Covid
Covid_Nursing_Home <- read.CSV("Covid_Nursing_Home.csv")
install.packages("readr")
install.packages("janitor")
library(janitor)
library(dplyr)
Covid_Nursing_Home <- Covid_Nursing_Home %>%
  row_to_names(row_number = 1)
Covid_Nursing_Home$total_deaths <- as.numeric(Covid_Nursing_Home$total_deaths)
Covid_Nursing_Home <- na.omit(Covid_Nursing_Home)
mean(Covid_Nursing_Home$total_deaths)
#Preprocessing
#Renaming Columns
install.packages("plyr")
library(plyr)
Covid_Nursing_Home <- rename(Covid_Nursing_Home, c("Provider State"="State", "Residents Total COVID-19 Deaths"="total_deaths"))

Fl <- Covid_Nursing_Home %>% filter(State=="FL")
library(ggplot2)
Fl$total_deaths <- as.numeric(Fl$total_deaths)
ggplot(Fl, aes(x = State, y = total_deaths)) +
  geom_boxplot()
Fl <- na.omit(Fl)
mean(Fl$total_deaths)
mean(death_20$total_deaths)

#Fl deaths greater than 
Fl$total_deaths


Covid_Nursing_Home$total_deaths <- as.numeric(Covid_Nursing_Home$total_deaths)
mean(Covid_Nursing_Home$total_deaths)

#Fl deaths greater than 
death_20 <- Covid_Nursing_Home %>% filter(total_deaths>20)

#1/4th of the nursing homes have death greater than 20

ggplot(death_20, aes(x = State, y = total_deaths)) +
  geom_boxplot()

#When deaths are more than 20 the mean death is 34 

#distribution of states with their count who have more than 20 deaths. 
state_distribution_more_20<- death_20 %>% group_by(State) %>% dplyr:: summarise(mean= n())
state_distribution_more_20
