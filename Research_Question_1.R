Covid_Nursing_Home
#1st Research Question : any difference in number of deaths from administrative deficiencies to in general. 

#joining deaths on administrative deficiencies and quality ratings. 

library(janitor)
library(dplyr)
Quality_Measures <- MDS_Quality_Measures %>%
  row_to_names(row_number = 1)

a <- Quality_Measures %>% filter(`Federal Provider Number`== 155719)

Administrative_Deficiency <- Health_Deficiencies %>% filter(Deficiency.Category == "Administration Deficiencies")
#gg
Distinct_federal_number_deficiency <- Administrative_Deficiency %>% distinct(Federal.Provider.Number)

names(Covid_Nursing_Home)[names(Covid_Nursing_Home) == "Federal Provider Number"] <- "Federal.Provider.Number"

covid_administrative_deficiencies <-  merge(x=Covid_Nursing_Home,y=Administrative_Deficiency ,by="Federal.Provider.Number")
Distinct_federal_number_at_covid_data <- covid_administrative_deficiencies %>% distinct(Federal.Provider.Number)

print(Distinct_federal_number_at_covid_data)                                                                                       

max_death_nursing_home <- Covid_Nursing_Home %>% group_by(Federal.Provider.Number) %>% summarise(deaths = max(total_deaths))
covid_nursing_selected_variables <- Covid_Nursing_Home %>% 
  select(Federal.Provider.Number,`Provider Name`,`Provider Address`,`Provider City`,State,`Number of All Beds`,total_deaths,`Total Number of Occupied Beds`,`Provider Zip Code`)

covid_nursing_selected_variables <- distinct(covid_nursing_selected_variables)

Nursing_home_deaths_bed <-  merge(x=max_death_nursing_home,y=covid_nursing_selected_variables ,by="Federal.Provider.Number")
Nursing_home_deaths_bed

drops <- c("Total Number of Occupied Beds")
Nursing_home_deaths_bed <- Nursing_home_deaths_bed[ , !(names(Nursing_home_deaths_bed) %in% drops)]

Nursing_home_deaths_bed <- distinct(Nursing_home_deaths_bed)

Nursing_home_deaths_bed$`Number of All Beds` <- as.numeric(Nursing_home_deaths_bed$`Number of All Beds`)
cor(Nursing_home_deaths_bed$`Number of All Beds`,Nursing_home_deaths_bed$deaths)

plot(Nursing_home_deaths_bed$`Number of All Beds`,Nursing_home_deaths_bed$deaths)
mean(Nursing_home_deaths_bed$deaths)

Administrative_Deficiency <- Administrative_Deficiency %>% select(Federal.Provider.Number,Deficiency.Category,Deficiency.Description) %>% distinct()

Nursing_home_deaths_bed <-  merge(x=Administrative_Deficiency,y=Nursing_home_deaths_bed ,by="Federal.Provider.Number")
Nursing_home_deaths_bed <- distinct(Nursing_home_deaths_bed)

# Nursing_Home_deaths_bed has one to many relationship to descriptions
Nursing_home_administrative_deficiency_description <- Nursing_home_deaths_bed %>% select(Deficiency.Description.x,Federal.Provider.Number)

drops <- c("Deficiency.Description.x", "Deficiency.Description.y")
Nursing_home_deaths_bed <- Nursing_home_deaths_bed[ , !(names(Nursing_home_deaths_bed) %in% drops)]
Nursing_home_deaths_bed <- distinct(Nursing_home_deaths_bed)
hist(Nursing_home_deaths_bed$deaths)
hist(Covid_Nursing_Home$total_deaths)

#Statistical Tests
b <- Nursing_home_deaths_bed$deaths ##deaths where administrative deficient
a <- Covid_Nursing_Home$total_deaths ##original Data
t.test(a,b,paired = FALSE)
library(dplyr)
b <- Nursing_home_deaths_bed %>% filter(deaths!=0)
mean(b$deaths)

b$deaths_per_bed <- b$deaths/b$`Number of All Beds`
hist(b$deaths_per_bed)
covid_deaths_more_1 <- Covid_Nursing_Home %>% filter(total_deaths!=0)
mean(covid_deaths_more_1$total_deaths)
