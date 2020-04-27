## load required libraries

library(tidyverse)
library(plotly)
library(lubridate)
library(ggExtra)
library(gganimate)
library(magick)
# library(wpp2019)



###################### Global Confirmed Cases ##


# file_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

file_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"


############ Africa Regions


## Add Africa Regions

west_africa <- c("Benin", "Burkina Faso", "Cabo Verde", "Cote d'Ivoire", "Gambia", "Ghana", "Guinea", "Equatorial Guinea", "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")

south_africa <- c("Botswana","Eswatini", "Lesotho", "Namibia", "South Africa")

north_africa <- c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia", "Mauritania") 

east_africa <- c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Madagascar", "Malawi", "Mauritius", "Mayotte", "Mozambique", "Reunion", "Seychelles", "Somalia", "South Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe")

central_africa <-  c("Angola", "Cameroon", "Central African Republic", "Chad", "Congo (Kinshasa)", "Congo (Brazzaville)", "Equatorial Guinea", "Gabon", "Sao Tome and Príncipe")


africa <- c(west_africa, north_africa, east_africa, central_africa, south_africa)

# Africa <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Congo (Brazzaville)", "Congo (Kinshasa)", "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Mali","Malawi", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

asia <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakstan", "Korea, South", "Kuwait", "Kyrgyzstan", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Nepal", "Oman", "Pakistan", "Peru", "Qatar", "Saudi Arabia", "Singapore", "Sri Lanka", "Taiwan*", "Thailand", "United Arab Emirates", "Uzbekistan", "Vietnam", "Syria", "Timor-Leste", "Laos", "West Bank and Gaza", "Burma")

south_america <- c("Argentina", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Suriname", "Trinidad and Tobago", "Venezuela")

oceania <- c("Australia", "Fiji", "New Zealand", "Papua New Guinea")

europe <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bolivia", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Greece", "Holy See", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Philippines", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Uruguay", "Kosovo")

north_america <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Canada", "Costa Rica", "Cuba", "Dominican Republic", "El Salvador", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Lucia", "US", "Dominica", "Grenada", "Belize", "Saint Kitts and Nevis", "Saint Vincent and the Grenadines")

others <- c("Diamond Princess", "MS Zaandam")



download.file(url = file_cases, 
              destfile = "./covid_cases.csv")

covid_cases <- read_csv("./covid_cases.csv") 

names(covid_cases) <- stringr::str_to_lower(names(covid_cases))

covid_cases2 <- covid_cases %>%
  
  mutate(continent = case_when(`country/region` %in% africa ~ "Africa",
                               `country/region` %in% asia ~ "Asia",
                               `country/region` %in% europe ~ "Europe",
                               `country/region` %in% north_america ~ "North America",
                               `country/region` %in% south_america ~ "South America",
                               `country/region` %in% oceania ~ "Oceania",
                               `country/region` %in% others ~ "Others")
  ) %>%
  
  rename(country = `country/region`)


covid_cases2 <- covid_cases2 %>%
  
  select(`province/state`, 
         country, 
         continent, 
         # population,
         lat, 
         long, 
         everything()) %>%

  pivot_longer(- c(`province/state`, 
                   `country`, 
                   `continent`,
                   # `population`,
                   `lat`, 
                   `long`), 
               names_to = "date", 
               values_to = "cases")
  

covid_cases2$date <- lubridate::mdy(covid_cases2$date)




####################### mortality data ####

file_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

download.file(url = file_deaths, 
              destfile = "./covid_deaths.csv")

covid_deaths <- read_csv("./covid_deaths.csv") 

names(covid_deaths) <- stringr::str_to_lower(names(covid_deaths))

covid_deaths2 <- covid_deaths %>% 
  
  pivot_longer(- c(`province/state`, 
                   `country/region`, 
                   `lat`, 
                   `long`), 
               names_to = "date", 
               values_to = "deaths") %>%
  
  rename(country = `country/region`)

covid_deaths2$date <- lubridate::mdy(covid_deaths2$date)

##################################################################




### merged data ###


covid_df <- left_join(covid_cases2, 
                      covid_deaths2, 
                      by = c("province/state", 
                             "country",
                             "lat", 
                             "long", 
                             "date"))

covid_df$country <- as_factor(covid_df$country)



################### Africa data  ##############################

covid_africa <- covid_df %>%
  filter(continent == "Africa") %>%
  rename(region = `province/state`) %>%
  mutate(region = case_when(country %in% west_africa ~ "West Africa",
                            country %in% east_africa ~ "East Africa",
                            country %in% north_africa ~ "North Africa",
                            country %in% central_africa ~ "Central Africa",
                            country %in% south_africa ~ "South Africa",
                            TRUE ~ "Others")
         )
#######################################################################


## number of countries affected at the beginning of March

n_count <- covid_df %>%
  filter(date == as.Date("2020-03-01")) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0)




## no of cases as at yesterday

n_count2 <- covid_df %>%
  filter(date == Sys.Date() - 1) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0)


current_cases <- covid_df %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  arrange(desc(cases))

global_trend <- covid_df %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) + 
  geom_line(size = 1, colour = "darkred") + 
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 5000000, by = 500000), labels = scales::comma, minor_breaks = NULL) + 
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d", minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of cases",
       title = "Global reported number of confirmed COVID19 cases") +
  rotateTextX(angle = 45)



## no of countries affected

n_country <- covid_df %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(cases != 0) %>%
  group_by(date) %>%
  count() %>%
  rename(count = n)

n_countries <- n_country %>%
  ggplot(aes(x = date, y = count)) + 
  geom_line(colour = "powderblue", size = 1.5) +
  theme_minimal() +
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 200, by = 20), minor_breaks = NULL) + 
  labs(y = "No of countries affected", 
       title = "No of countries affected by COVID-19") +
  rotateTextX(angle = 45)




world <- covid_df %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases, color = country)) + 
  geom_line(size = 1) + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 2000000, by = 200000), labels = scales::comma, minor_breaks = NULL) +
  rotateTextX(angle = 45) +
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend around the World") 




## africa data analysis


africa <- covid_df %>%
  dplyr::filter(continent == "Africa")

africa_cases <- africa %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  arrange(desc(cases))

africa_trend <- africa %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) + 
  geom_line(size = 1, colour = "darkred") + 
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 50000, by = 5000), labels = scales::comma, minor_breaks = NULL) + 
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d", minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend in Africa") +
  rotateTextX(angle = 45)



africa2 <- africa %>%  # select the data
  group_by(country, date) %>%  # one entry per country per day
  summarise(cases = sum(cases)) %>%  # add all the cases for the country
  arrange(desc(date), desc(cases))

a <- africa2$country[1:10]

africa2_plot <-  africa2 %>%  # select the data
  group_by(country, date) %>%  # one entry per country per day
  summarise(cases = sum(cases)) %>%
  arrange(desc(date), desc(cases)) %>%
  filter(country == a[1] | 
           country == a[2] |
           country == a[3] |
           country == a[4] |
           country == a[5] |
           country == a[6] |
           country == a[7] |
           country == a[8] |
           country == a[9] |
           country == a[10]) %>%
  ggplot(aes(x = date, y = cases, colour = country)) +
  geom_line(size = 1) +
  theme_light() + 
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 1000), labels = scales::comma, minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend in top 10 hit countries in Africa") +
  rotateTextX(angle = 45)



ngr <- covid_df %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  filter(country == "Nigeria") %>%
  ggplot(aes(x = date, y = cases)) + 
  geom_line(size = 1.5, color = "lightblue") + 
  theme_minimal() +
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d", minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = 0, to = 500, by = 50), minor_breaks = NULL) + 
  labs(x = "Date", 
       y = "No of Cases",
       title = "COVID19 trend in Nigeria as at today") +
  rotateTextX(angle = 45)





## animated plot ##

df <- covid_df %>%
  mutate(month = month(covid_df$date),
         day = day(covid_df$date)) %>%
  filter(month == 4) %>%
  dplyr::rename(April = day) %>%
  group_by(April, country, continent) %>%
  summarise(deaths = sum(deaths),
            cases = sum(cases))


animate <- df %>% 
  ggplot(aes(x = cases, 
             y = deaths, 
             color = continent)) + 
  
  geom_point(aes(frame = April, 
                 ids = country, 
                 na.rm = FALSE),
             size = 5,
             alpha = .4, 
             show.legend = FALSE) +
  
  labs(x = "No of confirmed cases",
       y = "No of confirmed deaths", 
       title = "Trend Analysis of the COVID-19 Pandemic") +
  
  theme_minimal() +
  
  scale_x_continuous(breaks = seq(from = 0, 
                                  to = 1000000, 
                                  by = 100000), 
                     labels = scales::comma, 
                     minor_breaks = NULL) + 
  
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 50000,
                                  by = 10000),
                     labels = scales::comma,
                     minor_breaks = NULL)



########################### more analysis for Nigeria

df_ng <- read_csv("C:/Users/stbal/Google Drive/R projects/covid_nigeria/nigeria_data.csv")

df_ng2 <- read_csv("C:/Users/stbal/Google Drive/R projects/covid_nigeria/nigeria_data2.csv")




# trend_plot <- function(df) {
#   
#   p <-  df %>%
#     ggplot(aes(x = date, 
#                y = count, 
#                color = region_province)) +
#     geom_line(aes(group = state)) +
#     theme_minimal() + 
#     scale_x_date(date_breaks = "2 week", 
#                  date_labels = "%b %d", 
#                  minor_breaks = NULL) +
#     labs(y = "cummulative")
#   
#   ggplotly(p, tooltip = c("date", "state", "count"))
#     
# }




indicator_plots <-  df_ng2 %>%
  filter(time_stamp == "cummulative") %>%
  group_by(indicator, date) %>%
  summarise(across(is.numeric, sum)) %>%
  ggplot(aes(x = date, 
             y = number, 
             color = indicator)) +
  geom_line() +
  theme_minimal() + 
  scale_x_date(date_breaks = "2 week", 
               date_labels = "%b %d", 
               minor_breaks = NULL) +
  labs(y = "cummulative")
  

  

##### make animated plots for top seven affected states by cases

p <-  df_ng %>%
  filter(date > Sys.Date() - 30) %>% # select most recent 40 days
  group_by(date) %>%
  arrange(desc(cases_cum)) %>% # arrange by desc the assigned indicator
  slice(1:7) %>% # take first 7 for each day
  mutate(rank = 1:7) %>%
    
  ggplot(aes(x = rank, 
             group = state,
             fill = state)) + 
    
  geom_tile(aes(y = cases_cum / 2,
                height = cases_cum,
                width = 0.8),
            alpha = 0.8, 
            color = NA) +
  
  geom_text(aes(y = 0, 
                label = paste(state, " ")), 
            vjust = 0.2, 
            hjust = 1) + 
  
  geom_text(aes(y = cases_cum, 
                label = cases_cum, 
                hjust = 0)) + 
  
  coord_flip(clip = "off", 
             expand = FALSE) +
  
  scale_x_reverse() +
  
  guides(color = FALSE, 
         fill = FALSE) +
  
  theme_minimal() +
  removeGridY() +
  
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_line( size=.1, color="grey" ),
    panel.grid.minor.x = element_line( size=.1, color="grey" ),
    plot.title = element_text(size = 25, 
                              hjust = 0.5, 
                              face = "bold", 
                              colour= "grey", 
                              vjust = -1),
    plot.subtitle = element_text(size = 18, 
                                 hjust = 0.5, 
                                 face = "italic", 
                                 color = "grey"),
    plot.caption = element_text(size = 10, 
                                hjust = 0.5, 
                                face = "italic", 
                                color="grey"),
    plot.margin = margin(2,2, 2, 4, "cm")
  )

anim <- p + 
  transition_states(date, 
                    transition_length = 5, 
                    state_length = 1) +
  ease_aes("cubic-in-out") +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Total number cases at: {closest_state}',  
       subtitle  =  "Top 7 affected States",
       caption  = "Data Source: Nigeria Centre for Diseases Control")
  

# anim_save("animated_plot.gif", anim)

animate(anim, 100, fps = 10, 
        renderer = gifski_renderer("gganim.gif"))




