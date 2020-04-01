##############################################################################
#                            
#                            Version 2
#               --------------------------------------         
#                   Infant Mortality Data Analysis
#             Data source: WorldBank Development Indicator API
#             Date:       APRIL 27, 2017
#             
#############################################################################



###############################################################################
#                              PART I
#               loading required packages, functions and data
###############################################################################

source("./scripts/my_functions.R")  # loads required packages and functions

# download data and read-in the downloaded data -------------------------------
##### first date of data download 28th May, 2017 #####

library(downloader)

### download the mortality file if not already present in the "data" folder" ####

file_url <- "http://www.childmortality.org/files_v20/download/RatesDeaths_AllIndicators.xlsx"

if (!file.exists("./data/RatesDeaths_AllIndicators.xlsx")) {
    download(url = file_url, destfile = "./data")
}

### download the live birth data (zipped) from UN website ####
file_url2 <- "https://data.unicef.org/wp-content/uploads/2015/12/U5MR_mortality_rate_39.xlsx"
  
if (!file.exists("./data/U5MR_deaths_40.xlsx")) {
  download(url = file_url2, destfile = "./data")
}

#### read-in the downloaded data####

child_mortality <- read_excel("./data/RatesDeaths_AllIndicators.xlsx", skip = 6)
u5_deaths <- read_excel("./data/U5MR_deaths_40.xlsx", sheet = 1, skip = 14)

####  convert all column names to lower letters  #####

names(child_mortality) <- str_to_lower(names(child_mortality))
names(u5_deaths) <- str_to_lower(names(u5_deaths))

### ---------------------  end of part I  -------------------------- ###



###############################################################################
#                              PART II
#                   Data cleaning and wrangling
###############################################################################

u5_deaths <- u5_deaths %>%
  select(`iso code`, `uncertainty bounds*`, countryname, `1990.5`:`2015.5`) %>%
  clean_child_mortality_data("u5_deaths") %>%
  select(-year) %>% 
  rename(year = rate) %>%
  mutate(year = parse_double(year)) %>%
  mutate(mdg_years = ifelse(.$year <= 2000, "no", "yes"))


u5mr <- child_mortality %>%
  select(`iso code`, `uncertainty bounds*`, countryname, u5mr.1990:u5mr.2015) %>%
  clean_child_mortality_data("u5mr") %>%
  factorise() %>%
  add_decline("u5mr")

u5_data <- u5_deaths %>%
  left_join(u5mr, by = c("year", "continent", "region", "income", "country", "iso3c", "mdg_years"))


nmr_data <- child_mortality %>%
  select(`iso code`, `uncertainty bounds*`, countryname, nmr.1990:nmr.2015) %>% 
  clean_child_mortality_data("nmr") %>%
  factorise() %>%
  add_decline("nmr")



imr_data <- child_mortality %>%
  select(`iso code`, `uncertainty bounds*`, countryname, imr.1990:imr.2015) %>% 
  clean_child_mortality_data("imr") %>%
  factorise() %>%
  add_decline("imr")


### ---------------------  end of part II  -------------------------- ###



###############################################################################
#                              PART III
#                           Data analysis
###############################################################################


# plot of children deaths pre-mdgs ----------------------------------------



# plot of children deaths pre-mdgs ----------------------------------------

nm_plot <- nmr_data %>%
  filter(year == 1990) %>%
  arrange(desc(nmr))%>%
  top_20_plot("nmr") +
  labs(title = "Countries with the Highest Neonatal Mortality Rate", 
       subtitle = "Year 1990",
       x = "Countries",
       y = "Neonatal Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

im_plot <- imr_data %>%
  filter(year == 1990) %>%
  arrange(desc(imr)) %>%
  top_20_plot("imr") +
  labs(title = "Countries with the Highest Infant Mortality Rate", 
       subtitle = "Year 1990",
       x = "Countries",
       y = "Infant Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

u5m_plot <- u5_data %>%
  filter(year == 1990) %>%
  arrange(desc(u5mr)) %>%
  top_20_plot("u5mr") +
  labs(title = "Countries with the Highest Under-five Mortality Rate", 
       subtitle = "Year 1990",
       x = "Countries",
       y = "Under-five Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")


nm_plot2 <- nmr_data %>%
  filter(year == 2000) %>%
  arrange(desc(nmr)) %>%
  top_20_plot("nmr") +
  labs(title = "Countries with the Highest Neonatal Mortality Rate", 
       subtitle = "Year 2000",
       x = "Countries",
       y = "Neonatal Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

im_plot2 <- imr_data %>%
  filter(year == 2000) %>%
  arrange(desc(imr)) %>%
  top_20_plot("imr") +
  labs(title = "Countries with the Highest Infant Mortality Rate", 
       subtitle = "Year 2000",
       x = "Countries",
       y = "Infant Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

u5m_plot2 <- u5_data %>%
  filter(year == 2000) %>%
  arrange(desc(u5mr)) %>%
  top_20_plot("u5mr") +
  labs(title = "Countries with the Highest Under-five Mortality Rate", 
       subtitle = "Year 2000",
       x = "Countries",
       y = "Under-five Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")


# plot of children deaths after mdg ---------------------------------------


nm_plot3 <- nmr_data %>%
  filter(year == 2015) %>%
  arrange(desc(nmr)) %>%
  top_20_plot("nmr") +
  labs(title = "Countries with the Highest Neonatal Mortality Rate", 
       subtitle = "Year 2015",
       x = "Countries",
       y = "Neonatal Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

im_plot3 <- imr_data %>%
  filter(year == 2015) %>%
  arrange(desc(imr)) %>%
  top_20_plot("imr") +
  labs(title = "Countries with the Highest Infant Mortality Rate", 
       subtitle = "Year 2015",
       x = "Countries",
       y = "Infant Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")

u5m_plot3 <- u5_data %>%
  filter(year == 2015) %>%
  arrange(desc(u5mr)) %>%
  top_20_plot("u5mr") +
  labs(title = "Countries with the Highest Under-five Mortality Rate", 
       subtitle = "Year 2015",
       x = "Countries",
       y = "Under-five Mortality Rate",
       caption = "Data from World Bank through the `WDI` package")



# does the MDG have impact childhood death rates? ---------------

# a) globally
# 
# annual_u5death_decline <- u5data %>% 
#   group_by(year) %>%
#   summarise(annual_death = signif((sum(u5_death, na.rm = TRUE))/10^6, 4)) %>%
#   filter(annual_death != 0)

global_stat <- map(list(nmr_data, imr_data, u5_data), tidy_stat, alt = "less", paired = TRUE) %>%
  reduce(dplyr::full_join) %>% 
  mutate(mortality_rate = c("neonatal", "infant", "under-five")) %>% 
  select(mortality_rate, statistic, p.value)

summary_nmr <- nmr_data %>% 
  group_by(year) %>% 
  summarise(nmr = mean(nmr, na.rm = TRUE))

summary_imr <- imr_data %>% 
  group_by(year) %>% 
  summarise(imr = mean(imr, na.rm = TRUE))

summary_u5mr <- u5_data %>% 
  group_by(year) %>% 
  summarise(u5mr = mean(u5mr, na.rm = TRUE))

child_mortalities <- summary_nmr %>%
  left_join(summary_imr, by = "year") %>%
  left_join(summary_u5mr, by = "year") %>%
  gather("child mortality", value, -year)


# b) Nigeria



child_mr <- list(nmr_data %>% filter(country == "Nigeria"),
                 imr_data %>% filter(country == "Nigeria"),
                 u5_data %>% filter(country == "Nigeria"))

nig_stat <- map(child_mr, test_stat, alt = "less") %>%
  reduce(dplyr::full_join) %>%  
  mutate(mortality_rate = c("neonatal", "infant", "under-five")) %>% 
  select(mortality_rate, statistic, p.value)

# nig_u5m <- u5_data %>% 
#   filter(country == "Nigeria")
# 
# nig_u5m_stat <-  t.test(nig_u5m$u5mr ~ nig_u5m$mdg_years, alt = "greater") %>%
#   tidy()

# despite the significant improvement, childhood mortality rates in Nigeria is still significantly high compared to some other countries in Africa
# 
# in 2015, a total of ----- under-five deaths occured in Nigeria

# nig_u5death_2015 <- u5data %>%
#   filter(country == "Nigeria" & year == 2015) %>%
#   select(year, country, u5mr, u5_death, u5_pop) %>%
#   mutate(u5_death = signif((u5_death / 10^6), 3))


# plot trend in mortality rate in Nigeria  ----------------------------------

nm_trend <- nmr_data %>%
  filter(country == "Nigeria") %>%
  trend_plot("nmr") + 
  labs(title = "Neonatal Mortality Trend in Nigeria", 
       subtitle = "1990 - 2015",
       y = "neonatal mortality rate")

im_trend <- imr_data %>% 
  filter(country == "Nigeria") %>%
  trend_plot("imr") + 
  labs(title = "Infant Mortality Trend in Nigeria", 
       subtitle = "1990 - 2015",
       y = "infant mortality rate")

u5m_trend <- u5_data %>% 
  filter(country == "Nigeria") %>%
  trend_plot("u5mr") + 
  labs(title = "Under-five Mortality Trend in Nigeria", 
       subtitle = "1990 - 2015",
       y = "under-five mortality rate")




# finally, i will be parting with this animated chart. Two countries (apart from Nigieria) are of particular interest: Niger (highest under-five mortality rate in 1990), and Haiti (highest under-five mortality rate in 2010)
# 
# 
animate <- u5_data %>% 
  filter(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year == 2015) %>%
  # mutate(u5_pop = signif((u5_pop / 10^3), 4)) %>% # convert to per million
  mutate(u5_deaths = signif((u5_deaths / 10^6), 4)) %>% # convert also to per million
  ggplot(aes(x = u5_deaths, y = u5mr, color = continent)) + 
  geom_point(aes(frame = year, ids = country, na.rm = TRUE), alpha = .3, show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, 0.5)) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50)) +
  scale_size(range = c(1, 10)) +
  labs(x = "no of under-five deaths (in millions)",
       y = "mortality rate ('000)", 
       title = "Trends in under-five mortality") +
  theme_minimal()


ggplotly(animate, tooltip = c("country", "u5_pop", "u5_death", "u5mr")) %>%
  animation_opts(2000, easing = "linear") %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) 

