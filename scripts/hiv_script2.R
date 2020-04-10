##############################################################################
#                            
#                            SCRIPT II
#               --------------------------------------         
#                   HIV Pandemic and its Impact on Nigeria
#             Data source: WorldBank Development Indicator API
#             Date:       April 01, 2020
#             
#############################################################################



###############################################################################
#                              PART I
#               loading required packages, functions and data
###############################################################################

source("./scripts/my_functions.R")  # loads required packages and functions


# read-in the downloaded data ---------------------------------------------

hiv_total <- read_csv("./scripts/hiv_prevalence_total")
hiv_children <- read_csv("./scripts/hiv_prevalence_children")
hiv_adult <- read_csv("./scripts/hiv_prevalence_adult")
hiv_adult_female <- read_csv("./scripts/hiv_prevalence_adult_female")
hiv_workforce <- read_csv("./scripts/hiv_prevalence_workforce")


### ---------------------  end of part I  -------------------------- ###


###############################################################################
#                              PART II
#                   Data cleaning and wrangling
###############################################################################


# clean the data
h_total <- hiv_total %>% 
  rename(total = SH.HIV.TOTL) %>%
  clean_data("total")

h_children <- hiv_children %>%  
  rename(children = SH.HIV.0014) %>% 
  clean_data("children")

h_adult <- hiv_adult %>% 
  rename(adult = SH.DYN.AIDS) %>%
  clean_data("adult")

h_adult_f <- hiv_adult_female %>%
  rename(adult_female = SH.DYN.AIDS.FE.ZS) %>%
  clean_data("adult_female")

h_workforce <- hiv_workforce %>%
  rename(workforce = SH.DYN.AIDS.ZS) %>%
  clean_data("workforce")


## merge all the prevalence data together and name it h_data
## 

h_data <- h_children %>%
  left_join(h_adult) %>%
  left_join(h_adult_f) %>%
  left_join(h_workforce) %>%
  left_join(h_total) %>%
  select(-mdg_years, mdg_years)



#### add population each country and population of children less than 15years from the wpp2019 package

data(pop)


### wpp2019 records population every 5years
### 
h_data2 <- h_data %>%
  filter(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year == 2015)
h_data2$country <- as.factor(h_data2$country)


## population of each country
population <- pop %>% select(-country_code) %>%
  gather(year, pop_total, - name) %>%
  mutate(pop_total = pop_total * 1000) %>%
  rename_("country" = "name") %>%
  as_tibble()
population$year <- parse_number(population$year)


## merge population of each country to h_data2
h_data2 <- h_data2 %>%
  left_join(population, by = c("country", "year"))
  

### population of children

## female children
data(popF)
f_child_pop <- popF %>%
  filter(age == "0-4" | age == "5-9" | age == "10-14") %>%
  select(-country_code) %>%
  gather(year, pop_child_f, - c(country, age)) %>%
  spread(age, pop_child_f) %>%
  as_tibble() %>%
  mutate(pop_child_f = (`0-4` + `5-9` + `10-14`) * 1000) %>%
  select(country, year, pop_child_f)

## male children
data(popM)
m_child_pop <- popM %>%
  filter(age == "0-4" | age == "5-9" | age == "10-14") %>%
  select(-country_code) %>%
  gather(year, pop_child_m, - c(country, age)) %>%
  spread(age, pop_child_m) %>%
  mutate(pop_child_m = (`0-4` + `5-9` + `10-14`) * 1000) %>%
  select(country, year, pop_child_m) %>%
  as_tibble()

## add population of male and female children together
child_pop <- f_child_pop %>%
  left_join(m_child_pop) %>%
  mutate(child_pop = pop_child_f + pop_child_m) %>%
  select(country, year, child_pop)
child_pop$year <- parse_number(child_pop$year)

## merge population of children with h_data2
h_data2 <- h_data2 %>%
  left_join(child_pop) %>%
  mutate(percent_child = (children / child_pop) * 100) %>%
  mutate(percent_tot = c(total / pop_total) * 100)


#### wpp2015 is causing trouble using dplyr verbs. So I am going to save h_data and h_data2 and start analysis in another script

saveRDS(h_data, "./scripts/h_data.rds")
saveRDS(h_data2, "./scripts/h_data2.rds")

