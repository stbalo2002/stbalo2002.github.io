##############################################################################
#                            
#                            SCRIPT I
#               --------------------------------------         
#                   Analysis of HIV prevalence
#             Data source: WorldBank Development Indicator API
#             Date:       April 01, 2020
#             
#############################################################################



###############################################################################
#                           PART I
#                    Downloading the data
###############################################################################


# loading the required libraries  ------------------------------------------

suppressMessages(c(library("WDI"),
                   library("readr")
))

# download the data -------------------------------------------------------

##  a) summary

hiv_total <- WDI(country = "all", 
                   indicator = "SH.HIV.TOTL", 
                   start = 1990, 
                   end = 2015, 
                   extra = TRUE, 
                   cache = NULL)  

hiv_children <- WDI(country = "all", 
                       indicator = "SH.HIV.0014", 
                       start = 1990, 
                       end = 2015, 
                       extra = TRUE, 
                       cache = NULL)  

hiv_adult <- WDI(country = "all", 
                        indicator = "SH.DYN.AIDS", 
                        start = 1990, 
                        end = 2015, 
                        extra = TRUE, 
                        cache = NULL)

## "Female adults with HIV (% of population ages 15+ with HIV)"

hiv_adult_female <- WDI(country = "all", 
                 indicator = "SH.DYN.AIDS.FE.ZS", 
                 start = 1990, 
                 end = 2015, 
                 extra = TRUE, 
                 cache = NULL)


##  b) adult

## hiv in age 15-49 years "Prevalence of HIV, total (% of population ages 15-49)" 

hiv_workforce <- WDI(country = "all", 
                 indicator = "SH.DYN.AIDS.ZS", 
                 start = 1990, 
                 end = 2015, 
                 extra = TRUE, 
                 cache = NULL)

## young women 
#  "Prevalence of HIV, female (% ages 15-24)" 
hiv_young_women <- WDI(country = "all", 
                     indicator = "SH.HIV.1524.FE.ZS", 
                     start = 1990, 
                     end = 2015, 
                     extra = TRUE, 
                     cache = NULL)

#"Comprehensive correct knowledge of HIV/AIDS, ages 15-24, female (2 prevent ways and reject 3 misconceptions)"  
hiv_knowledge_young_women <- WDI(country = "all", 
                       indicator = "SH.HIV.1524.KW.FE.ZS", 
                       start = 1990, 
                       end = 2015, 
                       extra = TRUE, 
                       cache = NULL)

## young men
#  "Prevalence of HIV, male (% ages 15-24)" 
hiv_young_men <- WDI(country = "all", 
                       indicator = "SH.HIV.1524.MA.ZS", 
                       start = 1990, 
                       end = 2015, 
                       extra = TRUE, 
                       cache = NULL)

# "Comprehensive correct knowledge of HIV/AIDS, ages 15-24, male (2 prevent ways and reject 3 misconceptions)"  
hiv_knowledge_young_men <- WDI(country = "all", 
                       indicator = "SH.HIV.1524.KW.MA.ZS", 
                       start = 1990, 
                       end = 2019, 
                       extra = TRUE, 
                       cache = NULL)

# "Antiretroviral therapy coverage (% of people with advanced HIV infection)"
art_coverage <- WDI(country = "all", 
                    indicator = "SH.HIV.ARTC.ZS", 
                    start = 1990, 
                    end = 2015, 
                    extra = TRUE, 
                    cache = NULL)

# "Number of HIV positive pregnant women receiving antiretrovirals "
art_coverage_preg <- WDI(country = "all", 
                    indicator = "SH.HIV.PREG.VIRALS.NUM", 
                    start = 1990, 
                    end = 2015, 
                    extra = TRUE, 
                    cache = NULL)


hiv_new_infection_total <- WDI(country = "all", 
                         indicator = "SH.HIV.NEW.TOTL.NUM", 
                         start = 1990, 
                         end = 2015, 
                         extra = TRUE, 
                         cache = NULL)
hiv_new_infection_children <- WDI(country = "all", 
                                  indicator = "SH.HIV.NEW.0014.NUM", 
                                  start = 1990, 
                                  end = 2015, 
                                  extra = TRUE, 
                                  cache = NULL)



###############################################################################
#                           PART II
#                     saving the data
###############################################################################

write_csv(hiv_total, "./scripts/hiv_prevalence_total")
write_csv(hiv_adult, "./scripts/hiv_prevalence_adult")
write_csv(hiv_adult_female, "./scripts/hiv_prevalence_adult_female")
write_csv(hiv_workforce, "./scripts/hiv_prevalence_workforce")
write_csv(hiv_children, "./scripts/hiv_prevalence_children")
write_csv(hiv_young_men, "./scripts/hiv_prevalence_young_men")
write_csv(hiv_knowledge_young_men, "./scripts/hiv_knowledge_young_men")
write_csv(hiv_young_women, "./scripts/hiv_prevalence_young_women")
write_csv(hiv_knowledge_young_women, "./scripts/hiv_knowledge_young_women")
write_csv(hiv_new_infection_total, "./scripts/hiv_new_infection_rate_total")
write_csv(hiv_new_infection_children, "./scripts/hiv_new_infection_rate_children")
write_csv(art_coverage, "./scripts/art_coverage_total")
write_csv(art_coverage_preg, "./scripts/art_coverage_in_pregnancy")


### ---------------------  end of part II  -------------------------- ###

