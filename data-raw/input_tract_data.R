#` this is a description
#`
## code to prepare `census_tract` dataset goes here

date <- format(Sys.time(), "%Y%m%d")
# pkgload::load_all()

requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("tigris", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)

###################
# download data sources of interest and select relevant variables
###################

## ----------- acs data
library(tidycensus)
library(dplyr)
library(tidyverse)
library(readxl)
library(purrr)
df<- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
             variables = c(totalhh ="B28001_001", with_broadband ="B28002_004",
                           medinc = "B19326_001", 
                           pop = "B02001_001", black = "B02001_003", 
                           white = "B02001_002",
                           somecol = "B16010_028", BA = "B16010_041",
                           LFP = "B16010_001", cell = "B28002_005",
                           cell_only = "B28002_006", over75k = "B28004_024",
                           total75 = "B28004_022", povtotal = "B17010_001",
                           poverty = "B17010_002", over65 = "B28005_014",
                           over65_comp = "B28005_015", over65_broadband = "B28005_017",
                           computer = "B28008_002", totes = "B19101_001"), survey = "acs1", output = "wide", year  = 2019, geometry = FALSE)

## --------------variables of interest from acs data
df <- subset(df, select = -c(totalhhM, with_broadbandM, medincM, popM, blackM, whiteM, somecolM, BAM, LFPM, cellM, cell_onlyM,
                             over75kM, total75M, povtotalM, povertyM, over65M, over65_compM, over65_broadbandM, 
                             computerM, totesM))

df <- df %>%
  mutate(broadband_pct = with_broadbandE/totalhhE,
         cell_only_pct = cell_onlyE/totalhhE,
         cell_pct = cellE/totalhhE,
         black_pct = blackE/popE,
         white_pct = whiteE/popE,
         poverty_pct = povertyE/povtotalE,
         over65_pct = over65E/popE,
         comp65_pct = over65_compE/over65E,
         broadband65_pct = over65_broadbandE/over65E,
         computer_pct = computerE/totalhhE,
         BA_pct = BAE/LFPE) %>%
  mutate(without = totalhhE - with_broadbandE,
         nonwhite_pct = 1 - white_pct,
         wireline_pct = broadband_pct - cell_only_pct)

df$majority <- "No Majority"
df$majority[df$black_pct > .5] <- "Majority Black"
df$majority[df$white_pct > .5] <- "Majority White"

df$dig_pov <- "60% or greater adoption"
df$dig_pov[df$broadband_pct < .6] <- "40-60% adoption"
df$dig_pov[df$broadband_pct < .4] <- "20-40% adoption"
df$dig_pov[df$broadband_pct < .2] <- "< 20% adoption"

df$inc_pct_rank <- percent_rank(df$medincE)

df<-subset(df, select = c(GEOID, NAME, broadband_pct, cell_only_pct, cell_pct, black_pct, white_pct, poverty_pct, over65_pct, comp65_pct, broadband65_pct,
                          computer_pct, BA_pct, without, nonwhite_pct, wireline_pct))
df <-  rename(df, tract_string = "GEOID")

eva_data_raw <- df

###################
# add some human-readable metadata
###################

## -------------------------------describe data
eva_data_codes <- tribble(~variable, ~name, ~type, ~interpret_high_value,
                          "broadband_pct",	"Percent with broadband",	"market",	"high_opportunity",
                          "cell_only_pct",	"Percent with cell only",	"market",	"high_opportunity",
                          "black_pct",	"Percent black",	"market",	"high_opportunity",
                          "white_pct",	"Percent white",	"market",	"high_opportunity",
                          "poverty_pct",	"Percent Poverty",	"market",	"high_opportunity",
                          "over65_pct",	"Percent over 65",	"market",	"high_opportunity",
                          "comp65_pct",	"Percent over 65 with computer",	"market",	"high_opportunity",
                          "broadband65_pct",	"Percent over 65 with broadband",	"market",	"high_opportunity",
                          "computer_pct",	"Percent with computer",	"market",	"high_opportunity",
                          "BA_pct",	"Percent with BA",	"market",	"high_opportunity",
                          "without",	"Percent without broadband",	"market",	"high_opportunity",
                          "nonwhite_pct",	"Percent nonwhite",	"market",	"high_opportunity",
                          "wireline_pct",	"Percent with wireline broadband",	"market",	"high_opportunity")


 ###################
 # gather spatial elements
 ###################
 ## ---------------get cbsas via tigris
library(tidycensus)
library(dplyr)
library(tidyverse)
library(readxl)
library(purrr)
library(sf)
 allcbsas <- tigris::core_based_statistical_areas(
   class = "sf")%>%
   select(GEOID)
 
 eva_tract_geometry <- allcbsas %>% 
    st_transform(4326)

  usethis::use_data(eva_tract_geometry, overwrite = TRUE)
 
###################
# create final dataset - no spatial data
#note spatial data should be joined after any summarizing is done to save some computation time
###################

# #long data
eva_data_main <- eva_data_raw %>%
  gather("variable", "raw_value", -tract_string, -NAME) %>%
  group_by(variable) %>%
  mutate(MEAN = mean(raw_value, na.rm = T),
         SD = sd(raw_value, na.rm = T),
         MIN = min(raw_value, na.rm = T),
         MAX = max(raw_value, na.rm = T),
         COUNT = as.numeric(sum(!is.na(raw_value))),
         z_score = (raw_value - MEAN)/SD) %>%
  left_join(eva_data_codes) %>%
  
  #create nominal weights
  mutate(weights_nominal = case_when(interpret_high_value == "high_opportunity" ~ (raw_value - MIN) / (MAX - MIN) * 10,
                                     interpret_high_value == "low_opportunity" ~ 10 - (raw_value - MIN) / (MAX - MIN) * 10,
                                     TRUE ~ NA_real_)) %>%
  
  #Weights Standard Score
  mutate(weights_scaled = case_when(interpret_high_value == "high_opportunity" ~ pnorm(z_score) * 10,
                                    interpret_high_value == "low_opportunity" ~ (10 - pnorm(z_score) * 10),
                                    TRUE ~ NA_real_)) %>%
  
  #weights rank
  mutate(weights_rank = case_when(interpret_high_value == "high_opportunity" ~ min_rank((weights_nominal)) / COUNT * 10,
                                  interpret_high_value == "low_opportunity" ~ min_rank(desc(weights_nominal)) / COUNT * 10,
                                  TRUE ~ NA_real_)) %>%
  
  # #rank
  mutate(overall_rank = case_when(interpret_high_value == "high_opportunity" ~ min_rank(as.numeric(weights_nominal)),
                                  interpret_high_value == "low_opportunity" ~ min_rank(desc(as.numeric(weights_nominal))))) %>%
  
  #clean dataframe
  select(-MEAN, -SD, -MIN, -MAX)

usethis::use_data(eva_data_main, overwrite = TRUE)
