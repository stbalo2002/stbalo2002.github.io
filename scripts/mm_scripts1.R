##############################################################################
#                            
#                            SCRIPT I
#                   Maternal Mortality Data Analysis
#             Data source: WorldBank Development Indicator API
#             Date:       April 27, 2017
#             
#############################################################################



###############################################################################
#                           PART I
#                    Downloading the data
###############################################################################


# loading the required libraries  ------------------------------------------

suppressMessages(
  c(
    library("WDI"),
    library("readr"),
    library("tidyverse")
    )
)


# downloading the data ----------------------------------------------------

mr_maternal <- WDI(country = "all", 
                   indicator = "SH.STA.MMRT", 
                   start = 1990, 
                   end = 2015, 
                   extra = TRUE, 
                   cache = NULL)


## b) maternal deaths

deaths_maternal <- WDI(country = "all", 
                       indicator = "SH.MMR.DTHS", 
                       start = 1990, end = 2015, 
                       extra = TRUE, 
                       cache = NULL)   
  ### ---------------------  end of part I  -------------------------- ###



###############################################################################
#                           PART II
#                       Saving the data
###############################################################################

write_csv(mr_maternal, "./scripts/maternal_mortality")
write_csv(deaths_maternal, "./scripts/maternal_deaths")

  ### ---------------------  end of part I I -------------------------- ###

