# R Script: aca_enrollment_by_demo.R
# Pulls MEPS data from 2018 - 2022 and shows the total member months enrolled
# total individuals enrolled for at least one month in private insurance through
# a federally-facilitated, state-based, or state partnership exchange/marketplace.

## REQUIRED LIBRARIES
library(tidyverse)
library(cli)
library(furrr)
library(future)
library(haven)

# No scientific notation
options(scipen = 999)

# This R Script utilizes a custom MEPS package maintained by Emily Mitchell (AHRQ)
# to retrieve MEPS data files that contain key demographic, truncated ICD-10-CM, and 
# NDC inputs on thousands of individual respondent data contained in the MEPS
# survey data PUFs. 

# MEPS Data PUFs:
# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp

# MEPS R Package on Github:
# https://github.com/e-mitchell/meps_r_pkg

# This must be installed "from scratch" via Github, consider using devtools::install_github()
library(MEPS)

# Let's get all the info we can for expenditure varaibles for the last 5 years

meps_years <- list(meps_year = c("2018", "2019", "2020", "2021", "2022"))
num_years <- length(meps_years$meps_year)
fyc_datasets <- NULL

# Function to rename columns
rename_fields <- function(names, yr2d) {
  # Iterate over each name and apply the renaming logic
  names <- sapply(names, function(name) {
    if (str_detect(name, "DIABDX_M18")) {
      # Don't touch this one!
      name <- name
    }
    # Check if the name matches the pattern
    else if (str_detect(name, "^(PRI|PEG|PNG|POG|PRX|MCR|MCD)[A-Z]{2}\\d{2}$")) {
      # Replace the last two digits with "YY"
      name <- str_sub(name, 1, -3) %>% str_c("YY")
    }
    else if (str_detect(name, "^INS[A-Z]{2}\\d{2}X$")) {
      # Replace the last two digits with "YY"
      name <- str_remove(name, "\\d{2}X") %>% str_c("YY")
    }
    else if (str_detect(name, yr2d)) {
      # Replace the last two digits with "YY"
      name <- str_replace(name, yr2d, "YY")
    }
    return(name)  # Return the modified or original name
  })
  return(names)
}

for (year in meps_years$meps_year)
{
  yr2d <- str_sub(year, 3)
  select_variables <- c("DUPERSID", "PANEL", "VARPSU", "VARSTR", paste0("PERWT",yr2d,"F"),
                        # Expenditure variables for all health services
                        paste0("TOTEXP",yr2d), paste0("TOTPRV",yr2d), paste0("TOTMCR",yr2d), 
                        paste0("TOTMCD",yr2d), paste0("TOTSLF",yr2d), 
                        # Utilization variables for office-based visits
                        paste0("OBTOTV",yr2d),
                        # Expenditure variables for office-based visits
                        paste0("OBVEXP",yr2d), paste0("OBVPRV",yr2d), paste0("OBVMCR",yr2d), 
                        paste0("OBVMCD",yr2d), paste0("OBVSLF",yr2d), 
                        # Utilization variables for hospital outpatient visits
                        paste0("OPTOTV",yr2d),
                        # Expenditure variables for hospital outpatient visits
                        paste0("OPTEXP",yr2d), paste0("OPTPRV",yr2d), paste0("OPTMCR",yr2d), 
                        paste0("OPTMCD",yr2d), paste0("OPTSLF",yr2d), 
                        # Utilization variables for ED visits
                        paste0("ERTOT",yr2d),
                        # Expenditure variables for ED visits
                        paste0("ERTEXP",yr2d), paste0("ERTPRV",yr2d), paste0("ERTMCR",yr2d), 
                        paste0("ERTMCD",yr2d), paste0("ERTSLF",yr2d), 
                        # Utilization vars for Inpatient stays
                        paste0("IPDIS",yr2d), paste0("IPNGTD",yr2d),
                        # Expenditure vars for Inpatient stays
                        paste0("IPTEXP",yr2d), paste0("IPTPRV",yr2d), paste0("IPTMCR",yr2d), 
                        paste0("IPTMCD",yr2d), paste0("IPTSLF",yr2d), 
                        # Utilization vars for RX
                        paste0("RXTOT",yr2d), 
                        # Expenditure vars for RX
                        paste0("RXEXP",yr2d), paste0("RXPRV",yr2d), paste0("RXMCR",yr2d), 
                        paste0("RXMCD",yr2d), paste0("RXSLF",yr2d),
                        # Utilization vars for Dental
                        paste0("DVTOT", yr2d),
                        # Expenditure vars for Dental
                        paste0("DVTEXP",yr2d), paste0("DVTPRV",yr2d), paste0("DVTMCR",yr2d), 
                        paste0("DVTMCD",yr2d), paste0("DVTSLF",yr2d),
                        # Other variables go to other
                        paste0("INSCOV",yr2d), paste0("REGION",yr2d), paste0("POVLEV",yr2d),
                        paste0("POVCAT",yr2d),
                        "RACETHX", "DOBYY", "DOBMM", "SEX", paste0("AGE", yr2d, "X"),
                        paste0("MARRY", yr2d, "X"), "DIABDX_M18", "ASTHDX", "CANCERDX", "EMPHDX")
                        
  
  fyc_data <- MEPS::read_MEPS(type = "FYC", year=year) %>% 
    select(all_of(select_variables), 
           matches("^(PRI|PEG|PNG|POG|PRX|MCR|MCD)(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE)\\d{2}$"),
           matches("^INS(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE)\\d{2}X$")) %>% 
    rename_with(~ rename_fields(.x, yr2d)) %>%
    mutate(meps_year = year) %>%
    relocate(meps_year, .after = DUPERSID)
  
  fyc_datasets <- fyc_datasets %>% 
    union_all(fyc_data)
}

# Bring in pooled weights here!
pooled_linkage_file  <- MEPS::read_MEPS(type = "PL") %>% 
  select(DUPERSID, PANEL, STRA9622, PSU9622) 

fyc_data_pooled <- fyc_datasets %>% 
  left_join(pooled_linkage_file, join_by(DUPERSID, PANEL)) %>% 
  mutate(POOLWTYYF = PERWTYYF / num_years) %>% 
  mutate(RACETHX_DSC = str_to_title(str_remove(labelled::to_factor(RACETHX), paste0(RACETHX, " "))),
         INSCOV_DSC = str_to_title(str_remove(labelled::to_factor(INSCOVYY), paste0(INSCOVYY, " "))),
         REGION_DSC = str_to_title(str_remove(labelled::to_factor(REGIONYY), paste0(REGIONYY, " "))),
         MARRY_DSC = str_to_title(str_remove(labelled::to_factor(MARRYYYX), paste0(MARRYYYX, " "))),
         POVCAT_DSC = str_to_title(str_remove(labelled::to_factor(POVCATYY), paste0(POVCATYY, " "))),
         DIABDX_DSC = str_to_title(str_remove(labelled::to_factor(DIABDX_M18), paste0(DIABDX_M18, " "))),
         ASTHDX_DSC = str_to_title(str_remove(labelled::to_factor(ASTHDX), paste0(ASTHDX, " "))),
         CANCERDX_DSC = str_to_title(str_remove(labelled::to_factor(CANCERDX), paste0(CANCERDX, " "))),
         EMPHDX_DSC = str_to_title(str_remove(labelled::to_factor(EMPHDX), paste0(EMPHDX, " "))),
         SEX_DSC = str_to_title(str_remove(labelled::to_factor(SEX), paste0(SEX, " "))),
         AGE_GRP_3 = case_when(
           AGEYYX < 18 ~ "Under 18",
           AGEYYX < 65 ~ "18 - 64", 
           AGEYYX >= 65 ~ "65 and over",
           .default = "N/A")) %>% 
  mutate(AGE_GRP_9 = case_when(AGEYYX < 5 ~ "Under 5",
                               AGEYYX >= 5 & AGEYYX < 18 ~ "5 - 17",
                               AGEYYX >= 18 & AGEYYX < 30 ~ "18 - 29",
                               AGEYYX >= 30 & AGEYYX < 40 ~ "30 - 39",
                               AGEYYX >= 40 & AGEYYX < 50 ~ "40 - 49",
                               AGEYYX >= 50 & AGEYYX < 60 ~ "50 - 59",
                               AGEYYX >= 60 & AGEYYX < 70 ~ "60 - 69",
                               AGEYYX >= 70 & AGEYYX < 80 ~ "70 - 79",
                               AGEYYX >= 80 ~ "80 and over",
                               .default = "N/A"))
