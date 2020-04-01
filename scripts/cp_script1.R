##############################################################################
#                            
#                            SCRIPT I
#               --------------------------------------         
#                   Contraceptive Prevalence
#             Data source: WorldBank Development Indicator API
#             Date:       April 28, 2017
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


cont_prev <- WDI(country = "all", 
                 indicator = "SP.DYN.CONU.ZS", 
                 start = 1990, 
                 end = 2015, 
                 extra = TRUE, 
                 cache = NULL)  

fert_rate_total <- WDI(country = "all", 
                 indicator = "SP.DYN.TFRT.IN", 
                 start = 1990, 
                 end = 2015, 
                 extra = TRUE, 
                 cache = NULL)  

unmet_need <- WDI(country = "all", 
                  indicator = "SP.UWT.TFRT", 
                  start = 1990, 
                  end = 2015, 
                  extra = TRUE, 
                  cache = NULL)  
   

fert_rate_wanted <- WDI(country = "all", 
                        indicator = "SP.DYN.WFRT", 
                        start = 1990, 
                        end = 2015, 
                        extra = TRUE, 
                        cache = NULL)

###############################################################################
#                           PART II
#                     saving the data
###############################################################################

write_csv(cont_prev, "./scripts/contraceptive_prevalence")
write_csv(unmet_need, "./scripts/unmet_need")
write_csv(fert_rate_total, "./scripts/fertility_rate_total")
write_csv(fert_rate_wanted, "./scripts/fertility_rate_wanted")

### ---------------------  end of part II  -------------------------- ###

