##############################################################################
#                            
#                            SCRIPT IV
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


# read-in the downloaded data ---------------------------------------------


hiv_know_young_men <- read_csv("./scripts/hiv_knowledge_young_men")
hiv_know_young_women <- read_csv("./scripts/hiv_knowledge_young_women")
art_cov_tot <- read_csv("./scripts/art_coverage_total")
art_cov_preg <- read_csv("./scripts/art_coverage_in_pregnancy")


### ---------------------  end of part I  -------------------------- ###



###############################################################################
#                              PART II
#                   Data cleaning and wrangling
###############################################################################

### a)

## "% of males ages 15-49 having comprehensive correct knowledge about HIV (2 prevent ways and reject 3 misconceptions)" 
hn_ym <- hiv_know_young_men %>% 
  rename(knowledge_men = SH.HIV.1524.KW.MA.ZS) %>%
  clean_data("knowledge_men")


## % of females ages 15-49 having comprehensive correct knowledge about HIV (2 prevent ways and reject 3 misconceptions)"
hn_yf <- hiv_know_young_women %>%
  rename(knowledge_women = SH.HIV.1524.KW.FE.ZS) %>%
  clean_data("knowledge_women")


df3 <- hn_ym %>%
  left_join(hn_yf)


#### b)

art_cov_t <- art_cov_tot %>% 
  rename(coverage_total = SH.HIV.ARTC.ZS) %>%
  clean_data("coverage_total")

art_cov_p <- art_cov_preg %>%
  rename(coverage_preg = SH.HIV.PREG.VIRALS.NUM) %>%
  clean_data("coverage_preg")

df4 <- art_cov_t %>%
  left_join(art_cov_p)

## new infection plots

plot_new1 <- df %>%
  mutate(new_infection = new_infection / 1000) %>%
  ggplot(aes(x = year, y = new_infection, group = country)) + 
  geom_point(color = "lightgreen", alpha = 0.5) + 
  geom_line(color = "lightblue") + 
  theme_minimal() + 
  labs(y = "Number of new infection (per thousands)", 
       title = "Number of new infection by country")



plot_new2 <- df %>%
  mutate(new_infection_children = new_infection_children / 1000) %>%
  ggplot(aes(x = year, y = new_infection_children, group = country)) + 
  geom_point(color = "lightblue", alpha = 0.5) + 
  geom_line(color = "lightgreen") + 
  theme_minimal() + 
  labs(y = "Number of new infection in children (per thousands)", 
       title = "New infection in children by country")


### knowledge plots

a <- df3 %>%
  group_by(country) %>%
  arrange(desc(knowledge_men)) %>%
  slice(1) %>%
  ggplot(aes(x = year, y = knowledge_men, color = continent, group = country)) + 
  geom_point(alpha = 0.5, size = 2) + 
  theme_minimal() + 
  labs(y = "percent of young men", 
       title = "Percentage of young men with comprehensive knowledge of HIV")

b <- df3 %>%
  group_by(country) %>%
  arrange(desc(knowledge_women)) %>%
  slice(1) %>%
  ggplot(aes(x = year, y = knowledge_women, color = continent, group = country)) + 
  geom_point(alpha = 0.5, size = 2) + 
  theme_minimal() + 
  labs(y = "percent of young women", 
       title = "Percentage of young women with comprehensive knowledge of HIV")

c <- df3 %>%
  group_by(country) %>%
  arrange(desc(knowledge_men)) %>%
  slice(1) %>%
  ggplot(aes(x = knowledge_women, y = knowledge_men, color = continent, group = country)) + 
  geom_point(alpha = 0.5, size = 2) + 
  theme_minimal() + 
  labs(y = "percent of young men", 
       x = "percent of young women",
       title = "Percentage of young adults with comprehensive knowledge of HIV")
