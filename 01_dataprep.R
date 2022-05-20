##----call package loading source-------------------------------------
source("00_package_loading.R")
##----data import-----------------------------------------------------------------------

mtpl <- read_delim(file = "Assignment.csv",
                   delim = ",",
                   col_names = T)

inspost <- read_xls("inspost.xls",
                    sheet = "inspost",
                    col_names = T)

mtpl <- mtpl %>% left_join(inspost, by = "CODPOSS")

mtpl <- as_tibble(mtpl) %>%
  dplyr::select(-COMMUNE, -INS) %>%
  rename("ageph" = AGEPH,
         "codposs" = CODPOSS,
         "lat" = LAT,
         "long" = LONG) %>%
  mutate_if(is.character, as.factor) %>%
  mutate("idph" = 1:nrow(mtpl))

##----adding insights from data exploration----------------------------------------------

source("02_data_exploration.R") #run this line before compiling line 31 and onwards

##----data prep for frequency--------------------------------------------------------------------

set.seed(1234)

mtpl_split <- initial_split(mtpl, prop = 0.75, strata = nbrtotc)
mtpl_training <- training(mtpl_split)
mtpl_test <- testing(mtpl_split)

##----data prep for severity------------------------------------------------------------------

set.seed(1234)

mtpl_sev_split <- initial_split(mtpl_sev, prop = 0.75, strata = sev)
mtpl_sev_training <- training(mtpl_sev_split)
mtpl_sev_test <- testing(mtpl_sev_split)


