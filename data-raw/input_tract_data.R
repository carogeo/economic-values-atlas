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

## ----------- equity considerations data

temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_equity_considerations/xlsx_society_equity_considerations.zip",
  destfile = temp
)

equity <- readxl::read_xlsx(unzip(temp, "EquityConsiderations_Full.xlsx")) %>%
  janitor::clean_names() 

fs::file_delete("EquityConsiderations_Full.xlsx")


## --------------variables of interest from equity considerations
eva_data_raw <- equity %>%
  select(tr10, 
         p_0017,
         p_65up,
         avg_temp,
         phhi_qntl1,
         green_roof,
         env_cancer,
         luse_green) %>%
  rowwise() %>%
  mutate(luse_notgreen = 1 - luse_green) %>%
  select(-luse_notgreen) %>%
  rename(tract_string = tr10)

###################
# add some human-readable metadata
###################

## -------------------------------describe data
eva_data_codes <- tribble(~variable, ~name, ~type, ~interpret_high_value,
                          "p_0017", "% people age 17 or younger", "people",  "high_opportunity",
                          "p_65up", "% people age 65 or older", "people",  "high_opportunity",
                          "avg_temp", "Land surface temp on hot summer day", "environment",  "high_opportunity",
                          "phhi_qntl1", "% households with annual income less than $35,000", "people",  "high_opportunity",
                          "green_roof", "Water holding potential of green roofs on commercial bldgs", "environment",  "high_opportunity",
                          "env_cancer", "Lifetime cancer risk from air toxics", "people", "high_opportunity",
                          "luse_notgreen", "% of tract NOT used for green space", "environment", "high_opportunity"
                          )


# ###################
# # gather spatial elements
# ###################
# ## ---------------get tracts via tigris
# MNtract <- tigris::tracts(
#   state = "MN",
#   county = c(
#     "Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"#,
#     # "Sherburne", "Isanti", "Chisago", "Goodhue", "Rice", "Le Sueur", "Sibley", "McLeod", "Wright" #if want to add collar counties
#   ),
#   class = "sf"
# ) %>%
#   select(GEOID)
# 
# eva_tract_geometry <- MNtract %>% 
#   st_transform(4326)
# 
# usethis::use_data(eva_tract_geometry, overwrite = TRUE)

###################
# create final dataset - no spatial data
#note spatial data should be joined after any summarizing is done to save some computation time
###################

# #long data
eva_data_main <- eva_data_raw %>%
  gather("variable", "raw_value", -tract_string) %>%
  group_by(variable) %>%
  mutate(MEAN = mean(raw_value, na.rm = T),
         SD = sd(raw_value, na.rm = T),
         MIN = min(raw_value, na.rm = T),
         MAX = max(raw_value, na.rm = T),
         COUNT = as.numeric(sum(!is.na(raw_value))),
         z_score = (raw_value - MEAN)/SD) %>%
  
  left_join(eva_data_codes, by = 'variable') %>%
  
  # #we want high opportunity to be a high value, so this reorders those values if needed
  # mutate(opportunity_zscore = case_when(interpret_high_value == "high_opportunity" ~ z_score,
  #                                       interpret_high_value == "low_opportunity" ~ z_score * (-1),
  #                                         TRUE ~ NA_real_)) %>%
  
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
  # 
  #clean
  select(-MEAN, -SD, -MIN, -MAX) 
  

usethis::use_data(eva_data_main, overwrite = TRUE)

