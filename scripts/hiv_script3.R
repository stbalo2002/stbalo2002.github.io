##############################################################################
#                            
#                            SCRIPT III
#               --------------------------------------         
#                   HIV Pandemic and its Impact on Nigeria
#             Data source: WorldBank Development Indicator API
#             Date:       JULY 19, 2017
#             
#############################################################################



###############################################################################
#                              PART I
#               loading required packages, functions and data
###############################################################################

source("./scripts/my_functions.R")  # loads required packages and functions
options(digits = 4)


#### Restart R session before running this script

#### Load files

h_data <- readRDS("./scripts/h_data.rds")
h_data2 <- readRDS("./scripts/h_data2.rds")
hiv_prev_young_men <- read_csv("./scripts/hiv_prevalence_young_men")
hiv_prev_young_women <- read_csv("./scripts/hiv_prevalence_young_women")
hiv_new <- read_csv("./scripts/hiv_new_infection_rate_total")
hiv_new_children <- read_csv("./scripts/hiv_new_infection_rate_children")


### clean the data
h_new <- hiv_new %>% 
  rename(new_infection = SH.HIV.NEW.TOTL.NUM) %>%
  clean_data("new_infection")

hc_new <- hiv_new_children %>%  
  rename(new_infection_children = SH.HIV.NEW.0014.NUM) %>% 
  clean_data("new_infection_children")

df <- h_new %>%
  left_join(hc_new)


hp_ym <- hiv_prev_young_men %>%  ## prevalence in young men
  rename(prev_young_men = SH.HIV.1524.MA.ZS) %>%
  clean_data("prev_young_men")



hp_yw <- hiv_prev_young_women %>%  ## prevalence in young women
  rename(prev_young_women = SH.HIV.1524.FE.ZS) %>%
  clean_data("prev_young_women")

df2 <- hp_ym %>%
  left_join(hp_yw)



### Trends in global population of HIV

h_trend <- h_data %>%
  group_by(year) %>%
  summarise(n  = sum(total, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = n / 10^6)) + 
  geom_point(color = "lightblue") +
  geom_line(color = "lightblue") + 
  theme_minimal() + 
  labs(x = "year", 
       y = "population of people with HIV (in millions)",
       title = "Global trends in HIV")

h_prev <- h_data2 %>%
  group_by(year) %>%
  summarise(prev = round(
    (sum(total, na.rm = TRUE) / sum(pop_total, na.rm = TRUE) * 100),
    2)) %>%
  ggplot(aes(x = year, y = prev)) + 
  geom_point(color = "lightblue") +
  geom_line(color = "lightblue") + 
  theme_minimal() + 
  labs(x = "year", 
       y = "Prevalence of HIV (%)",
       title = "Global Trend in the Prevalence of HIV")


h_trend2 <- h_data %>%
  group_by(year, continent) %>%
  summarise(n = sum(total, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = n /10^6, color = continent)) +
  geom_point() + 
  geom_line() +
  theme_minimal() + 
  labs(x = "year", 
       y = "population of people with HIV (in millions)",
       title = "Population of People living with HIV per Continent")

h_1 <- h_data %>%
  filter(year == 2015) %>%
  group_by(continent) %>%
  summarise(n = sum(total, na.rm = TRUE)) %>%
  mutate(percent = (n / sum(n)))
h_1$percent <- scales::percent(h_1$percent) 

h_1 <- h_1 %>%
  ggplot(aes(x = continent, y = n / 10^6, label = percent)) + 
  geom_bar(aes(fill = continent), stat = "identity", color = "gray", width = 0.5) +
  theme_minimal() +
  geom_text(vjust = -2, color = "black", size = 3) +
  ggExtra::removeGridX() +
  labs(y = "population of people with HIV (in millions)",
       title = "Population of people with HIV in 2015 per continent")

h_2 <- h_data %>%
  filter(year == 2015) %>%
  arrange(desc(total, na.rm = TRUE)) %>%
  top_20_plot("total") + 
  scale_y_continuous(labels = scales::comma)

## percentage of the population infected with HIV (I.E. prevalence)
h_3 <- h_data2 %>%
  filter(year == 2015) %>%
  arrange(desc(percent_tot, na.rm = TRUE)) %>%
  top_20_plot("percent_tot") + 
  labs(y = "prevalence", 
       title = "Top 20 countries with the highest prevalence of HIV") + 
  scale_y_continuous(breaks = seq(0, 16, 4))


###### childhood HIV

## global trend in HIV in children
hc_trend <- h_data %>%
  group_by(year) %>%
  summarise(n  = sum(children, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = n / 10^6)) + 
  geom_point(color = "lightblue") +
  geom_line(color = "lightblue") + 
  theme_minimal() + 
  labs(x = "year", 
       y = "population (in millions)",
       title = "Global trends of HIV in children")

## childhood global prevalence

hc_prev <- h_data2 %>%
  group_by(year) %>%
  summarise(prev = round(
    (sum(children, na.rm = TRUE) / sum(child_pop, na.rm = TRUE) * 100),
    2)) %>%
  ggplot(aes(x = year, y = prev)) + 
  geom_point(color = "lightblue") +
  geom_line(color = "lightblue") + 
  theme_minimal() + 
  labs(x = "year", 
       y = "Prevalence of HIV in children (%)",
       title = "Global Trend in the Prevalence of HIV in children")

## continental trend
hc_trend2 <- h_data %>%
  group_by(year, continent) %>%
  summarise(n = sum(children, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = n /10^6, color = continent)) +
  geom_point() + 
  geom_line() +
  theme_minimal() + 
  labs(x = "year", 
       y = "population of people with HIV (in millions)",
       title = "Population of children living with HIV per Continent")

hc_1 <- h_data %>%
  filter(year == 2015) %>%
  group_by(continent) %>%
  summarise(n = sum(children, na.rm = TRUE)) %>%
  mutate(percent = (n / sum(n)))
hc_1$percent <- scales::percent(hc_1$percent) 

hc_1 <- hc_1 %>%
  ggplot(aes(x = continent, y = n / 10^6, label = percent)) + 
  geom_bar(aes(fill = continent), stat = "identity", color = "gray", width = 0.5) +
  theme_minimal() +
  geom_text(vjust = -2, color = "black", size = 3) +
  ggExtra::removeGridX() +
  labs(y = "population of children with HIV (in millions)",
       title = "Population of children with HIV in 2015 per continent")

## top 20 countries with highest population of HIV in children
hc_2 <- h_data %>%
  filter(year == 2015) %>%
  mutate(children = children /10^3) %>%
  arrange(desc(children, na.rm = TRUE)) %>%
  top_20_plot("children") + 
  labs(y = "population (in thousands)", 
       title = "Top 20 countries with highest population of children living with HIV") + 
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))

## top 20 countries with highest childhood prevalence of HIV
hc_3 <- h_data2 %>%
  filter(year == 2015) %>%
  arrange(desc(percent_child, na.rm = TRUE)) %>%
  top_20_plot("percent_child") + 
  labs(y = "prevalence of HIV in children (%)", 
       title = "Top 20 countries with highest prevalence of HIV")

### trends in HIV population in the top 10 countries


h_top_20_1 <- h_data %>%
  filter(country == "South Africa" | 
           country == "Nigeria" |
           country == "India" |
           country == "Uganda" |
           country == "Mozambique" |
           country == "Kenya" |
           country == "Zimbabwe" |
           country == "Tanzania" |
           country == "Zambia" |
           country == "Malawi") %>%
  mutate(adult = adult / 10^6)

a1 <- h_top_20_1 %>%
  ggplot(aes(x = year, y = total, group = country, color = continent)) +
  geom_point() +
  geom_line() + 
  theme_light() + 
  labs(y = "population of adults with HIV( in millions)")


## children


h_top_20_2 <- h_data %>%
  filter(country == "South Africa" | 
           country == "Nigeria" |
           country == "Congo, Dem. Rep." |
           country == "Uganda" |
           country == "Mozambique" |
           country == "Kenya" |
           country == "Zimbabwe" |
           country == "Tanzania" |
           country == "Zambia" |
           country == "Malawi")

a2 <- h_top_20_2 %>%
  ggplot(aes(x = year, y = children, group = country, color = continent)) +
  geom_point() +
  geom_line() + 
  theme_light() + 
  scale_y_continuous(labels = scales::comma) + 
  labs(y = "population of children with HIV", 
       title = "Trend in population of children with HIV in the leading 10 countries")



animate <- df2 %>% 
  ggplot(aes(x = prev_young_men, y = prev_young_women, color = continent)) + 
  geom_point(aes(frame = year, ids = country, na.rm = TRUE), alpha = .5, show.legend = FALSE, size = 3, shape = 21) +
  labs(x = "prevalence in young men (%)", 
       y = "prevalence in young women (%)",
       title = "Prevalence of HIV in young adults (15-24)") +
  # scale_size(range = c(1, 10)) +
  theme_minimal()


anim <- ggplotly(animate) %>%
  # animation_opts(easing = "linear") %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )

prev_ywplot <- df2 %>%
  filter(year == 2015) %>%
  arrange(desc(prev_young_women, na.rm = TRUE)) %>%
  top_20_plot("prev_young_women") + 
  labs(y = "prevalence (%)", 
       title = "Top 20 countries with the highest prevalence of HIV in young women")


prev_ymplot <- df2 %>%
  filter(year == 2015) %>%
  arrange(desc(prev_young_men, na.rm = TRUE)) %>%
  top_20_plot("prev_young_men") + 
  labs(y = "prevalence (%)", 
       title = "Top 20 countries with the highest prevalence of HIV in young men")




# nig <- df2 %>%
#   left_join(h_data2) %>%
#   filter(country == "Nigeria") %>%
#   # select(year, prev_young_men, prev_young_women) %>%
#   # gather(young, value, - year) %>%
#   mutate(total_prev = (total / pop_total) * 100) %>%
#   mutate(child_prev = (children / child_pop) * 100)
#  
# nig1 <- nig %>%
#   drop_na(total_prev) %>%
#   ggplot(aes(x = year)) +
#   geom_line(aes(y = total_prev), color = "dimgray") + 
#   geom_point(aes(y = total_prev), color = "dimgray") +
#   geom_point(aes(y = child_prev), color = "lightblue") +
#   geom_line(aes(y = child_prev), color = "lightblue") + 
#   geom_point(data = nig, aes(x = year, y = prev_young_men), color = "red", alpha = 0.4) +
#   geom_line(data = nig, aes(x = year, y = prev_young_men), color = "red", alpha = 0.4) +
#   geom_point(data = nig, aes(x = year, y = prev_young_women), color = "lightgreen") + 
#   geom_line(data = nig, aes(x = year, y = prev_young_women), color = "lightgreen") +
  # theme_minimal() + 
  # labs(y = "prevalence", 
  #      title = "Trend in prevalence of HIV in Nigeria")



nig <- df2 %>%
  left_join(h_data2) %>%
  filter(country == "Nigeria") %>%
  gather(group, value, -c(year, region, country, iso3c, continent, mdg_years)) %>%
  filter(group == "prev_young_men" |
           group == "prev_young_women"| 
           group == "percent_child"| 
           group == "percent_tot")

nig$group <- factor(nig$group, 
                         levels = c("prev_young_men", "prev_young_women", "percent_child", "percent_tot"), 
                         labels = c("young men", "young women", "children", "total"))

nig1 <- nig %>%
  drop_na() %>%
  ggplot(aes(x = year, y = value, color = group)) + geom_point() + 
  geom_line() + 
  theme_minimal() + 
  labs(y = "prevalence (%)", 
       title = "Trends in HIV prevalence in Nigeria")


nig2 <- h_data %>%
   mutate(total = total / 10^6) %>%
  filter(country == "Nigeria") %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "lightblue") +
  geom_line(color = "lightblue") +
  theme_minimal() +
   labs(x = "year",
     y = "Population (in millions)",
     title = "Population of people with HIV in Nigeria") + 
   scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5))
 
 
 
 
