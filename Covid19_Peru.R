#############################################################################
#############################################################################
### ---- COVID19_Peru ---- ###

# libraries 
library(coronavirus)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(maptools)
library(tibble)
library(ggrepel)
library(png)
library(grid)
library(sp)
library(skimr)

# data
data(coronavirus)
data(wrld_simpl)
data(covid19_vaccine) 

peru_corona <- coronavirus %>% filter(country == "Peru")

# generating the map
p <- ggplot()+
  geom_polygon(
    data = wrld_simpl,
    aes(x = long, y = lat, group = group), fill = "gray", colour = "white" 
  ) + 
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90)) + 
  scale_x_continuous(breaks = seq(-180, 180, 120)) + 
  scale_y_continuous(breaks = seq(-90, 90, 100)) 

peru <- p + geom_point(
  data = peru_corona, aes(x = long, y = lat), color = 'red', size = 2
) 

peru

# peru 
class(peru_corona)
as_tibble(peru_corona)
tbl_df(peru_corona)
class(peru_corona)

skim(peru_corona, cases)
summary(peru_corona)

negative_values <- peru_corona %>% filter(cases < 0)
negative_values

# replacing negative cases with NA

peru_corona_data <- peru_corona %>% mutate(cases = replace(cases, which(cases<0), NA))
peru_corona_data

cases_with_NA <- peru_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(peru_corona_data)

# province is NA for all entries, country, lat and long are same for all entries.

# selecting only the variables needed for the analysis

peru_covid19 <- select(peru_corona_data, c("date", "country", "type", "cases"))
peru_covid19

summary(peru_covid19)

peru_covid19_by_type <- peru_covid19 %>% 
  pivot_wider(names_from = type, values_from = cases)

summary(peru_covid19_by_type) 

peru_covid19_by_type_new <- peru_covid19_by_type %>% 
  mutate(active=(confirmed-recovered-death))

peru_covid19_by_type_new

peru_covid19_by_type_new_1 <- peru_covid19_by_type_new %>% 
  pivot_longer(3:6, names_to="type", values_to="cases")

peru_covid19_by_type_new_negative <- peru_covid19_by_type_new_1 %>% filter(cases < 0)
peru_covid19_by_type_new_negative

# larger negative values were  resulting in the recovered column, might be due to the situation where more people recover and die on some days than confirmed

# Most probably at the start of the pandemic, there might be a time period where lot of confirmed cases were recorded but none were recovered or sometimes it might be some very low death and recovered cases so then with the data available in this data set they might be included as recovered cases and sometimes death cases.

# replacing negative cases with NA

peru_covid19_new_data <- peru_covid19_by_type_new_1 %>% 
  mutate(cases = replace(cases, which(cases<0), NA))
peru_covid19_new_data

cases_with_NA <- peru_covid19_new_data %>% filter(is.na(cases))
cases_with_NA

summary(peru_covid19_new_data)


peru_data_all_types <- peru_covid19_new_data %>% 
  pivot_wider(names_from=type, values_from=cases)

summary(peru_data_all_types)

ggplot(peru_covid19_new_data, 
       aes(x=cases, y=type, fill=type, col=type)) +
  geom_boxplot(outlier.shape=NA, alpha=0.05) + geom_jitter(aes(col=type)) +
  ggtitle("Figure 5: Peru: Active cases, Confirmed cases, Deaths and Recovered cases")

peru_covid19_new_data %>% 
  ggplot(aes(x=date, y=cases, fill="type")) +
  geom_line(aes(col=type)) + facet_wrap(type~. ,nrow=4, scale="free") +
  ggtitle("Figure 6: Peru: Daily Active cases, Confirmed cases, Deaths and Recovered cases") 

peru_covid19_new <- peru_covid19_new_data %>% 
  mutate(cases = replace(cases, is.na(cases), 0))

peru_covid19_new

# since we can not find cumulative with with NA terms, we replaced NA with zero

peru_covid19_new_by_type <- select(peru_covid19_new, -c("country")) %>% 
  pivot_wider(names_from = type, values_from = cases)

peru_covid19_1 <- peru_covid19_new_by_type %>% 
  mutate(cumulative_confirmed=cumsum(confirmed), cumulative_death=cumsum(death), cumulative_recovered=cumsum(recovered), cumulative_active=cumsum(active))   

peru_covid19_2 <- select(peru_covid19_1, -(active:recovered))

peru_cumulative_cases <- peru_covid19_2 %>% 
  pivot_longer(cols=c(4:7), names_to = "type", values_to = "cumulative_cases")


ggplot(peru_cumulative_cases,
       aes(date, cumulative_cases, color=type)) + 
  geom_line(size=1) + ggtitle("Figure 7: Peru: Cumulative Active cases, Confirmed cases, Deaths and Recovered cases")

# compare the covid19 situation with nearby countries

bolivia_corona <- coronavirus %>% filter(country =="Bolivia")
summary(bolivia_corona)

colombia_corona <- coronavirus %>% filter(country =="Colombia")
summary(colombia_corona)

chile_corona <- coronavirus %>% filter(country =="Chile")
summary(chile_corona)

ecuador_corona <- coronavirus %>% filter(country =="Ecuador")
summary(ecuador_corona)

# if we find negative or NA cases so necessary adjustments should be done.

# Working with COVID-19 in Bolivia

negative_values <- bolivia_corona %>% filter(cases < 0)
negative_values

bolivia_corona_data <- bolivia_corona %>% 
  mutate(cases = replace(cases, which(cases<0), NA))
bolivia_corona_data

cases_with_NA <- bolivia_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(bolivia_corona_data)

# province is NA for all entries, country, lat and long are same for all entries.

# selecting only the variables needed for the analysis

bolivia_covid19 <- select(bolivia_corona_data, c("date", "country", "type", "cases"))
bolivia_covid19

summary(bolivia_covid19)

bolivia_covid19_by_type <- bolivia_covid19 %>% 
  pivot_wider(names_from = type, values_from = cases)

summary(bolivia_covid19_by_type)

# Working with COVID-19 in Colombia

negative_values <- colombia_corona %>% filter(cases < 0)
negative_values

colombia_corona_data <- colombia_corona %>% mutate(cases = replace(cases, which(cases<0), NA))
colombia_corona_data

cases_with_NA <- colombia_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(colombia_corona_data)

# province is NA for all entries, country, lat and long are same for all entries.

# selecting only the variables needed for the analysis

colombia_covid19 <- select(colombia_corona_data, c("date", "country", "type", "cases"))
colombia_covid19

summary(colombia_covid19)

colombia_covid19_by_type <- colombia_covid19 %>% 
  pivot_wider(names_from = type, values_from = cases)

summary(colombia_covid19_by_type)

# Working with COVID-19 in Chile

negative_values <- chile_corona %>% filter(cases < 0)
negative_values

chile_corona_data <- chile_corona %>% mutate(cases = replace(cases, which(cases<0), NA))
chile_corona_data

cases_with_NA <- chile_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(chile_corona_data)

# province is NA for all entries, country, lat and long are same for all entries.

# selecting only the variables needed for the analysis

chile_covid19 <- select(chile_corona_data, c("date", "country", "type", "cases"))
chile_covid19

summary(chile_covid19)

chile_covid19_by_type <- chile_covid19 %>% 
  pivot_wider(names_from = type, values_from = cases)

summary(chile_covid19_by_type)

# Working with COVID-19 in Ecuador

negative_values <- ecuador_corona %>% filter(cases < 0)
negative_values

ecuador_corona_data <- ecuador_corona %>% mutate(cases = replace(cases, which(cases<0), NA))
ecuador_corona_data

cases_with_NA <- ecuador_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(ecuador_corona_data)

# province is NA for all entries, country, lat and long are same for all entries.

# selecting only the variables needed for the analysis

ecuador_covid19 <- select(ecuador_corona_data, c("date", "country", "type", "cases"))
ecuador_covid19

summary(ecuador_covid19)

ecuador_covid19_by_type <- ecuador_covid19 %>% 
  pivot_wider(names_from = type, values_from = cases)

summary(ecuador_covid19_by_type)

peru_bolivia <- union(peru_covid19, bolivia_covid19)
peru_bolivia

peru_bolivia_colombia <- union(peru_bolivia, colombia_covid19)
peru_bolivia_colombia

peru_bolivia_colombia_chile <- union(peru_bolivia_colombia, chile_covid19)
peru_bolivia_colombia_chile

peru_bolivia_colombia_chile_ecuador <- union(peru_bolivia_colombia_chile, ecuador_covid19)
peru_bolivia_colombia_chile_ecuador

# The following plot shows how the cumulative confirmed cases in Peru and selected neighboring countries vary with time.  

peru_bolivia_colombia_chile_ecuador_confirmed <- peru_bolivia_colombia_chile_ecuador %>% 
  filter(type == "confirmed")

peru_bolivia_colombia_chile_ecuador_confirmednew <- peru_bolivia_colombia_chile_ecuador_confirmed %>% mutate(cases = replace(cases, is.na(cases), 0))

peru_bolivia_colombia_chile_ecuador_group_conf <- peru_bolivia_colombia_chile_ecuador_confirmednew %>% group_by(country)

peru_bolivia_colombia_chile_ecuador_group_conf_cum <- peru_bolivia_colombia_chile_ecuador_group_conf %>% mutate(confirmed_cumulative=cumsum(cases))

ggplot(peru_bolivia_colombia_chile_ecuador_group_conf_cum,
       aes(date, confirmed_cumulative, color=country)) + 
  geom_line(size=1) + ggtitle("Figure 8: Peru and nearby countries: Cumulative confirmed cases")


peru_bolivia_colombia_chile_ecuador_death <- peru_bolivia_colombia_chile_ecuador %>% 
  filter(type == "death")

peru_bolivia_colombia_chile_ecuador_deathnew <- peru_bolivia_colombia_chile_ecuador_death %>% mutate(cases = replace(cases, is.na(cases), 0))

peru_bolivia_colombia_chile_ecuador_group_dea <- peru_bolivia_colombia_chile_ecuador_deathnew %>% group_by(country)

peru_bolivia_colombia_chile_ecuador_group_dea_cum <- peru_bolivia_colombia_chile_ecuador_group_dea %>% mutate(death_cumulative=cumsum(cases))

ggplot(peru_bolivia_colombia_chile_ecuador_group_dea_cum,
       aes(date, death_cumulative, color=country)) + 
  geom_line(size=1) + ggtitle("Figure 9: Peru and nearby countries: Cumulative deaths")

# The following plot shows how the recovered cases in Peru and neighboring countries vary with time.  

peru_bolivia_colombia_chile_ecuador_recovered <- peru_bolivia_colombia_chile_ecuador %>% 
  filter(type == "recovered")

peru_bolivia_colombia_chile_ecuador_recoverednew <- peru_bolivia_colombia_chile_ecuador_recovered %>% mutate(cases = replace(cases, is.na(cases), 0))

peru_bolivia_colombia_chile_ecuador_group_rec <- peru_bolivia_colombia_chile_ecuador_recoverednew %>% group_by(country)

peru_bolivia_colombia_chile_ecuador_group_rec_cum <- peru_bolivia_colombia_chile_ecuador_group_rec %>% mutate(recovered_cumulative=cumsum(cases))

ggplot(peru_bolivia_colombia_chile_ecuador_group_rec_cum,
       aes(date, recovered_cumulative, color=country)) + 
  geom_line(size=1) + ggtitle("Figure 10: Peru and nearby countries: Cumulative recovered cases")


# Comparing the COVID-19 situation in Peru with countries which have the population roughly equal to the population of Peru**  

Iraq_corona <- coronavirus %>% filter(country =="Iraq")
summary(Iraq_corona) 

negative_values <- Iraq_corona %>% filter(cases < 0)
negative_values

Iraq_corona_data <- Iraq_corona %>% mutate(cases = replace(cases, which(cases<0), NA))
Iraq_corona_data

cases_with_NA <- Iraq_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(Iraq_corona_data)

Iraq_covid19 <- select(Iraq_corona_data, c("date", "country", "type", "cases"))
Iraq_covid19

summary(Iraq_covid19)

Morocco_corona <- coronavirus %>% filter(country =="Morocco")
summary(Morocco_corona) 

negative_values <- Morocco_corona %>% filter(cases < 0)
negative_values

Morocco_corona_data <- Morocco_corona %>% mutate(cases = replace(cases, which(cases<0), NA))
Morocco_corona_data

cases_with_NA <- Morocco_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(Morocco_corona_data)

Morocco_covid19 <- select(Morocco_corona_data, c("date", "country", "type", "cases"))
Morocco_covid19

summary(Morocco_covid19)

Poland_corona <- coronavirus %>% filter(country =="Poland")
summary(Poland_corona) 

negative_values <- Poland_corona %>% filter(cases < 0)
negative_values

Poland_corona_data <- Poland_corona %>% mutate(cases = replace(cases, which(cases<0), NA))
Poland_corona_data

cases_with_NA <- Poland_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(Poland_corona_data)

Poland_covid19 <- select(Poland_corona_data, c("date", "country", "type", "cases"))
Poland_covid19

summary(Poland_covid19)

Malaysia_corona <- coronavirus %>% filter(country =="Malaysia")
summary(Malaysia_corona) 

negative_values <- Malaysia_corona %>% filter(cases < 0)
negative_values

Malaysia_corona_data <- Malaysia_corona %>% mutate(cases = replace(cases, which(cases<0), NA))
Malaysia_corona_data

cases_with_NA <- Malaysia_corona_data %>% filter(is.na(cases))
cases_with_NA

summary(Malaysia_corona_data)

Malaysia_covid19 <- select(Malaysia_corona_data, c("date", "country", "type", "cases"))
Malaysia_covid19

summary(Malaysia_covid19)

peru_Iraq <- union(peru_covid19, Iraq_covid19)
peru_Iraq

peru_Iraq_Morocco <- union(peru_Iraq, Morocco_covid19)
peru_Iraq_Morocco

peru_Iraq_Morocco_Poland <- union(peru_Iraq_Morocco, Poland_covid19)
peru_Iraq_Morocco_Poland

peru_Iraq_Morocco_Poland_Malaysia <- union(peru_Iraq_Morocco_Poland, Malaysia_covid19)
peru_Iraq_Morocco_Poland_Malaysia


peru_Iraq_Morocco_Poland_Malaysia_confirmed <- peru_Iraq_Morocco_Poland_Malaysia %>% 
  filter(type == "confirmed")

peru_Iraq_Morocco_Poland_Malaysia_confirmednew <- peru_Iraq_Morocco_Poland_Malaysia_confirmed %>% mutate(cases = replace(cases, is.na(cases), 0))

peru_Iraq_Morocco_Poland_Malaysia_group_conf <- peru_Iraq_Morocco_Poland_Malaysia_confirmednew %>% group_by(country)

peru_Iraq_Morocco_Poland_Malaysia_group_conf_cum <- peru_Iraq_Morocco_Poland_Malaysia_group_conf %>% mutate(confirmed_cumulative=cumsum(cases))

ggplot(peru_Iraq_Morocco_Poland_Malaysia_group_conf_cum,
       aes(date, confirmed_cumulative, color=country)) + 
  geom_line(size=1) + ggtitle("Figure 11: Cumulative confirmed cases in Peru and selected other countries")

peru_Iraq_Morocco_Poland_Malaysia_death <- peru_Iraq_Morocco_Poland_Malaysia %>% 
  filter(type == "death")

peru_Iraq_Morocco_Poland_Malaysia_deathnew <- peru_Iraq_Morocco_Poland_Malaysia_death %>% mutate(cases = replace(cases, is.na(cases), 0))

peru_Iraq_Morocco_Poland_Malaysia_group_dea <- peru_Iraq_Morocco_Poland_Malaysia_deathnew %>% group_by(country)

peru_Iraq_Morocco_Poland_Malaysia_group_dea_cum <- peru_Iraq_Morocco_Poland_Malaysia_group_dea %>% mutate(death_cumulative=cumsum(cases))

ggplot(peru_Iraq_Morocco_Poland_Malaysia_group_dea_cum,
       aes(date, death_cumulative, color=country)) + 
  geom_line(size=1) + ggtitle("Figure 12: Cumulative deaths in Peru and selected other countries")


peru_Iraq_Morocco_Poland_Malaysia_recovered <- peru_Iraq_Morocco_Poland_Malaysia %>% 
  filter(type == "recovered")

peru_Iraq_Morocco_Poland_Malaysiarecoverednew <- peru_Iraq_Morocco_Poland_Malaysia_recovered %>% mutate(cases = replace(cases, is.na(cases), 0))

peru_Iraq_Morocco_Poland_Malaysia_group_rec <- peru_Iraq_Morocco_Poland_Malaysiarecoverednew %>% group_by(country)

peru_Iraq_Morocco_Poland_Malaysia_group_rec_cum <- peru_Iraq_Morocco_Poland_Malaysia_group_rec %>% mutate(recovered_cumulative=cumsum(cases))

ggplot(peru_Iraq_Morocco_Poland_Malaysia_group_rec_cum,
       aes(date, recovered_cumulative, color=country)) + 
  geom_line(size=1) + ggtitle("Figure 13: Cumulative recovered cases in Peru and selected other countries")

# Condition of the COVID-19 vaccination process in Peru 

head(covid19_vaccine)

peru_vaccination <- covid19_vaccine %>%
  filter(country_region=="Peru")

head(peru_vaccination)

peru_vaccination_data <- select(peru_vaccination, -c(1, 6:18))

head(peru_vaccination_data)

summary(peru_vaccination_data)


peru_vaccination_processs <- peru_vaccination_data %>%
  pivot_longer(2:4,
               names_to="vaccination", 
               values_to="count") 

vaccination_group <- peru_vaccination_processs %>% group_by(vaccination)

peru_vaccination_process_cumulative <- vaccination_group %>% mutate(vaccination_cumulative=cumsum(count))

ggplot(peru_vaccination_process_cumulative,
       aes(date, vaccination_cumulative, color=vaccination)) + 
  geom_line(size=1) + ggtitle("Figure 14: Peru: Vaccination process")


# Comparing the COVID-19 vaccination process in Peru with the countries leading the worldwide vaccine roll-out

israel_vaccination <- covid19_vaccine %>%
  filter(country_region=="Israel")

head(israel_vaccination)

israel_vaccination_data <- select(israel_vaccination, -c(6:18))

head(israel_vaccination_data)

summary(israel_vaccination_data)

# no any negative or NA cases recorded

peru_vaccination_data_1 <- select(peru_vaccination, -c(6:18))

peru_israel <- union(peru_vaccination_data_1, israel_vaccination_data)

peru_israel 

peru_israel_new <- peru_israel %>% 
  pivot_longer(3:5,
               names_to="vaccination", 
               values_to="count")

peru_israel_doses_administered <- peru_israel_new %>% 
  filter(vaccination == "doses_admin")

peru_israel_doses_administered_group <- peru_israel_doses_administered %>% group_by(country_region)

peru_israel_doses_administered_group_cumulative <- peru_israel_doses_administered_group %>% mutate(doses_administered_cumulative=cumsum(count))


ggplot(peru_israel_doses_administered_group_cumulative ,
       aes(date,doses_administered_cumulative, color=country_region)) + 
  geom_line(size=1) + ggtitle("Figure 15: Cumulative doses administered in Peru and Israel") 


peru_israel_fully_vaccinated <- peru_israel_new %>% 
  filter(vaccination == "people_fully_vaccinated")

peru_israel_fully_vaccinated_group <- peru_israel_fully_vaccinated %>% group_by(country_region)

peru_israel_fully_vaccinated_group_cumulative <- peru_israel_fully_vaccinated_group %>% mutate(fully_vaccinated_cumulative=cumsum(count))


ggplot(peru_israel_fully_vaccinated_group_cumulative ,
       aes(date, fully_vaccinated_cumulative, color=country_region)) + 
  geom_line(size=1) + ggtitle("Figure 16: Cumulative people who are fully vaccinated in Peru and Israel") 


