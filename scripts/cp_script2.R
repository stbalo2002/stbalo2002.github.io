##############################################################################
#                            
#                            SCRIPT II
#               --------------------------------------         
#                   Contraceptive Prevalence Rate
#             Data source: WorldBank Development Indicator API
#             Date:       May 13, 2017
#             
#############################################################################



###############################################################################
#                              PART I
#               loading required packages, functions and data
###############################################################################

source("./scripts/my_functions.R")  # loads required packages and functions

# read-in the downloaded data ---------------------------------------------

cp <- read_csv("./scripts/contraceptive_prevalence")
unmet_need <- read_csv("./scripts/unmet_need")
fr_total <- read_csv("./scripts/fertility_rate_total")

mr_maternal <- read_csv("./scripts/maternal_mortality")
u5m <- read_csv("./scripts/under_five_mortality")

### ---------------------  end of part I  -------------------------- ###



###############################################################################
#                              PART II
#                   Data cleaning and wrangling
###############################################################################

# clean, convert categorical variables to factors and add two variables -------

# clean the data
cp_cleaned <- cp %>% 
  rename(cont_prev = SP.DYN.CONU.ZS) %>%
  clean_data("cont_prev") %>% 
  factorise()

unmet_need_cleaned <- unmet_need %>% 
  rename(unmet_need = SP.UWT.TFRT) %>%
  clean_data("unmet_need") %>% 
  factorise()

fr_total_cleaned <- fr_total %>%  
  rename(fert_rate_total = SP.DYN.TFRT.IN) %>% 
  clean_data("fert_rate_total") %>%
  factorise()

mm <- mr_maternal %>%  
  rename(mmr = SH.STA.MMRT) %>% 
  clean_data("mmr") %>%
  factorise()

u5m_rate <- u5m %>%
  rename(u5mr = SH.DYN.MORT) %>%
  clean_data("u5mr") %>%
  factorise()

fert_data <- cp_cleaned %>%
  left_join(fr_total_cleaned) %>% 
  left_join(mm) %>%
  left_join(u5m_rate) %>%
  left_join(unmet_need_cleaned) %>%
  select(-cont_prev, cont_prev)
  # drop_na()





###############################################################################
#                              PART III
#                           Data analysis
###############################################################################


# plot of trends in contraceptive prevalance --------------------------------

# fert_data_summ <- fert_data %>%
#   group_by(continent, year) %>%
#   summarise(cont_prev = mean(cont_prev))

cp_trend <- fert_data %>%
  ggplot(aes(x = year, y = cont_prev)) + 
  geom_smooth(aes(color = continent),method = "lm", se = FALSE) +
  geom_line(aes(group = country, color = continent), alpha = 0.2) +
  geom_line(data = fert_data %>%
              filter(country == "Nigeria"), aes(color = continent), alpha = 1) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  labs(y = "contraceptive prevalence rate",
       title = "Trend in Contraceptive Prevalence")


top_20 <- fert_data %>% 
  group_by(country) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>%
  arrange(desc(cont_prev)) %>%
  top_20_plot("cont_prev") + 
  labs(y = "contraceptive prevalence rate (CPR)", 
       title = "countries with the highest CPR")


bottom_20 <- fert_data %>% 
  group_by(country) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>%
  arrange(cont_prev) %>%
  top_20_plot("cont_prev") + 
  labs(y = "contraceptive prevalence rate (CPR)", 
       title = "Countries with the lowest CPR")


# correlation between mmr, u5mr and cont_prev ------------------------------


mmr_cp <- ggplot(fert_data, aes(x = cont_prev, y = mmr)) + 
  geom_point(aes(color = continent, label = country), alpha = 0.3, size = 2) + 
  geom_smooth(method = "lm", color = "dimgray") + 
  theme_minimal() +
  labs(x = "contraceptive prevalence rate", 
       y = "maternal mortality ratio", 
       title = "Relationship between MMR and CPR")


u5mr_cp <- ggplot(fert_data, aes(x = cont_prev, y = u5mr)) + 
  geom_point(aes(color = continent, label = country), alpha = 0.3, size = 2, show.legend = FALSE) + 
  geom_smooth(method = "lm", color = "dimgray") + 
  theme_minimal() +
  labs(y = "under_five mortality rate",
       x = "contraceptive prevalence rate",
       title = "Relationship between child mortaliy and CPR")


mmr_cp_u5mr <- ggplot(fert_data, aes(x = cont_prev, y = mmr, color = continent, label = country)) + 
  geom_point(aes(size = u5mr), alpha = 0.2) + 
  scale_size(range = c(1, 10)) + 
  theme_minimal()


fit <- lm(cont_prev ~ mmr * u5mr * continent * year, data = fert_data)

fit_plot <- fert_data %>%
  drop_na() %>%
  add_predictions(fit) %>%
  ggplot(aes(x = cont_prev, label = country)) + 
  geom_point(aes(y = pred), alpha = 0.4) + 
  # geom_line(aes(y = cont_prev), color = "blue", size = 1) + 
  # geom_smooth(aes(y = pred), method = "lm", color = "green") +
  theme_minimal() +
  labs(x = "actual values",
       y = "predicted values",
       title = "Correlation between predicted and actual CPR")


# contraceptive prevalence and unmet needs for family planning ---------------


cp_unmet <- fert_data %>% 
  group_by(country) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  ggplot(aes(x = unmet_need, y = cont_prev, color = continent)) + 
  geom_point(aes(label = country), alpha = 0.3, size = 3) + 
  theme_minimal() + 
  labs(x = "unmet needs for family planning",
       y = "contraceptive prevalence rate", 
       title = "Relationship between CPR and its unmet needs")


# contraceptive prevalence and unmet needs in Nigeria  -----------------------

cp_unmet_nig <- fert_data %>%
  filter(country == "Nigeria") %>%
  rename(contraceptive_prev = cont_prev) %>%
  gather(family_planning, percent, c(unmet_need, contraceptive_prev)) %>%
  ggplot(aes(x = year, y = percent, color = family_planning)) + 
  geom_point() + 
  geom_line() + 
  theme_minimal() + 
  labs(color = "family planning",
    title = "Trend in Family planning in Nigeria")

