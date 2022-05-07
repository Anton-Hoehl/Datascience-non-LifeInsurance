##----package_loading-------------------------------------------------------------------
packages <- c("tidyverse",
              "readr",
              "tidymodels",
              "readxl",
              "gridExtra")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))

##----data_import-----------------------------------------------------------------------
mtpl <- read_delim(file = "Assignment.csv",
                   delim = ",",
                   col_names = T)

inspost <- read_xls("inspost.xls",
                    sheet = "inspost",
                    col_names = T)

str(mtpl)
str(inspost)

mtpl <- mtpl %>% left_join(inspost, by = "CODPOSS")

mtpl <- mtpl %>%
        select(-COMMUNE) %>%
        rename("ageph" = AGEPH,
               "codposs" = CODPOSS,
               "ins" = INS,
               "lat" = LAT,
               "long" = LONG) %>%
        mutate_if(is.character, as.factor)
            
##---------------------------------------------------------------------------
sum(is.na(mtpl))


mtpl <- mtpl %>%
        mutate(sev = ifelse(nbrtotc == 0, NA, chargtot / nbrtotc))
        

mtpl %>% select_if(is.numeric) %>% summary()

min(mtpl$ageph)
max(mtpl$ageph)
##----data_viz_prep--------------------------------------------------------------
pantone <- "#D0417E"
col <- pantone
fill <- pantone
ylab <- "Relative frequency"

gg.bar <- function(data, variable) {
  ggplot(data = data, aes_string(x = variable)) +
    theme_bw() +
    geom_bar(aes(y = (..count..) / sum(..count..)), 
             col = col, fill = fill, alpha = 0.5) +
    labs(x = variable, y = ylab)
}

gg.hist <- function(data, variable, binwidth = 30) {
  ggplot(data = data, aes_string(x = variable)) +
    theme_bw() +
    geom_histogram(aes(y = (..count..) / sum(..count..)),
                   col = col, binwidth = binwidth, fill = fill, alpha = 0.5) +
    labs(x = variable, y = ylab)
}

gg.dens <- function(data, variable) {
  ggplot(data = data, aes_string(x = variable)) +
    theme_bw() +
    geom_density(col = col, fill = fill, alpha = 0.5) +
    labs(x = variable, y = "density") +
    xlim(0.0001,10000)
}

##----bar_plots--------------------------------------------------------------
char_cols <- mtpl %>% select_if(is.factor) %>% names()
g_bars <- lapply(char_cols, gg.bar, data = mtpl)

##----hist_plots-------------------------------------------------------------
g_duree <- gg.hist(mtpl, "duree", binwidth = 0.05)
g_nbrtotc <- gg.hist(mtpl, "nbrtotc", binwidth = 1)
g_nbrtotan <- gg.hist(mtpl, "nbrtotan", binwidth = 3)
g_ageph <- gg.hist(mtpl, "ageph", binwidth = 2)
g_hists <- list(g_duree, g_nbrtotan, g_nbrtotc, g_ageph)

##----dens_plots------------------------------------------------------------
g_sev <- gg.dens(mtpl %>% filter(nbrtotc > 0, sev < 60000), "sev")     #severities over 60000 omitted
g_sevs <- list(g_sev)

##----data_viz---------------------------------------------------------------------
g_list <- c(g_hists,g_sevs,g_bars)
grid.arrange(grobs = g_list)
