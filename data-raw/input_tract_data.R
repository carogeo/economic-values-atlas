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
df_maryland <- get_acs(geography = "tract", 
                       variables = c(totalpop = "B01001_001", raceethtotal = "B03002_001", 
                                     whitenonhl = "B03002_003", povtotal = "B17010_001",
                                     poverty = "B17010_002", medincome = "B06011_001"), 
                       state = 24, geometry = FALSE, output = "wide", year = 2019)
## --------------variables of interest from acs data
eva_data_raw <- df_maryland %>%
  mutate(Median_Income=medincomeE,
         Poverty_Level=povertyE/povtotalE,
         Diversity=1-(whitenonhlE/raceethtotalE),
         CountyFIPS=str_sub(df_maryland$GEOID, start = 1, end = 5))%>%
  filter(CountyFIPS=="24003"|CountyFIPS=="24005"|CountyFIPS=="24510"|
           CountyFIPS=="24013"|CountyFIPS=="24025"|CountyFIPS=="24027"|
           CountyFIPS=="24035")%>%
##If you want to show all metro tracts, comment this section out           
  filter(GEOID=="24510010200"|GEOID=="24510040100"|GEOID=="24510040200"|GEOID=="24510090300"
         |GEOID=="24510090400"|GEOID=="24510090500"|GEOID=="24510090800"|GEOID=="24510120201"
         |GEOID=="24510120300"|GEOID=="24510120400"|GEOID=="24510140200"|GEOID=="24510140300"
         |GEOID=="24510150100"|GEOID=="24510150800"|GEOID=="24510150900"|GEOID=="24510151000"
         |GEOID=="24510151100"|GEOID=="24510160100"|GEOID=="24510160200"|GEOID=="24510170100"
         |GEOID=="24510170200"|GEOID=="24510170300"|GEOID=="24510180100"|GEOID=="24510180200"
         |GEOID=="24510180300"|GEOID=="24510190100"|GEOID=="24510190200"|GEOID=="24510190300"
         |GEOID=="24510200600"|GEOID=="24510200701"|GEOID=="24510200702"|GEOID=="24510210100"
         |GEOID=="24510210200"|GEOID=="24510260501"|GEOID=="24510260604"|GEOID=="24510260700"
         |GEOID=="24510260800"|GEOID=="24510260900"|GEOID=="24510261000"|GEOID=="24510261100"
)%>%
  select("GEOID","Median_Income","Poverty_Level","Diversity")%>%
  rename(tract_string = "GEOID")
## --------------corridor data
#library(readxl)
#Corridor_Walkshed_Tracts_v2 <- read_excel("data-raw/Corridor_Walkshed_Tracts_v2.xlsx", 
#                                          sheet = "Corridor Tracts")
#eva_data_raw<-merge(eva_data_raw, Corridor_Walkshed_Tracts_v2, by.x="tract_string", by.y="CT2010")
#eva_data_raw<-eva_data_raw%>%
#  select(Median_Income, Poverty_Level, Diversity, Commercial_Corridor_Number)%>%
#  group_by(Commercial_Corridor_Number)%>%
#  summarise(Median_Income=mean(Median_Income), Poverty_Level=mean(Poverty_Level), Diversity=(mean(Diversity)))%>%
#  rename(tract_string="Commercial_Corridor_Number")

###################
# add some human-readable metadata
###################

## -------------------------------describe data
eva_data_codes <- tribble(~variable, ~name, ~type, ~interpret_high_value,
                          "Median_Income",	"Median income",	"market",	"high_opportunity",
                          "Poverty_Level",	"Share of population in poverty",	"inclusivity",	"low_opportunity",
                          "Diversity",	"Share of population who are BIPOC and/or Hispanic or Latino",	"inclusivity",	"high_opportunity")


 ###################
 # gather spatial elements
 ###################
 ## ---------------get tracts via tigris

 MDtract <- tigris::tracts(
   state = "MD",
   county = c("Anne Arundel", "Baltimore City", "Baltimore County", "Carroll", "Harford", "Howard", "Queen Anne's"),
   class = "sf")%>%
   mutate(GEOID=paste0(STATEFP,COUNTYFP,TRACTCE))%>%
#If you want to  show all metro tracts, comment this section out         
  filter(GEOID=="24510010200"|GEOID=="24510040100"|GEOID=="24510040200"|GEOID=="24510090300"
         |GEOID=="24510090400"|GEOID=="24510090500"|GEOID=="24510090800"|GEOID=="24510120201"
         |GEOID=="24510120300"|GEOID=="24510120400"|GEOID=="24510140200"|GEOID=="24510140300"
         |GEOID=="24510150100"|GEOID=="24510150800"|GEOID=="24510150900"|GEOID=="24510151000"
         |GEOID=="24510151100"|GEOID=="24510160100"|GEOID=="24510160200"|GEOID=="24510170100"
         |GEOID=="24510170200"|GEOID=="24510170300"|GEOID=="24510180100"|GEOID=="24510180200"
         |GEOID=="24510180300"|GEOID=="24510190100"|GEOID=="24510190200"|GEOID=="24510190300"
         |GEOID=="24510200600"|GEOID=="24510200701"|GEOID=="24510200702"|GEOID=="24510210100"
         |GEOID=="24510210200"|GEOID=="24510260501"|GEOID=="24510260604"|GEOID=="24510260700"
         |GEOID=="24510260800"|GEOID=="24510260900"|GEOID=="24510261000"|GEOID=="24510261100"
  )%>%
   select(GEOID)
#If you want to just show corridor geometries, uncomment            
# MDtract3<-merge(MDtract2,Corridor_Walkshed_Tracts_v2, by.x='GEOID',by.y='CT2010')
# MDtract4<-MDtract3%>%select(Commercial_Corridor_Number,geometry)
# MDtract<-MDtract4%>%
#   group_by(Commercial_Corridor_Number)%>%
#   summarise(geometry=st_union(geometry))%>%
#   rename(tract_string="Commercial_Corridor_Number")%>%
#   select(tract_string)
 
 eva_tract_geometry <- MDtract %>% 
    st_transform(4326)

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
