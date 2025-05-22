###Install packages
#library(usmap)
#library(tidyverse)
#library(readxl)
#library(tidygeocoder)
#library(mapview)
#library(writexl)
#library(mapview)

### Vaccines For Children Data
vfc_providers <- read_csv("https://github.com/jakebharmon/saphl_vfc/raw/refs/heads/main/data/ActiveVFCProviders.csv") %>% 
  mutate(County = str_to_title(County))

vfc_providers[vfc_providers=="Mccormick"]<-"McCormick" #Standardize troublesome county name

### Medicaid data
#Rates of child vs adult vs senior
medicaid_coverage_rates <- read.csv("https://github.com/jakebharmon/saphl_vfc/raw/refs/heads/main/data/data-eUlDP.csv") %>% 
  mutate(County.Name =  str_remove(County.Name," County"))

#Amounts and Percentages of uninsured by adult and child
insurance_gap <- read.csv("https://github.com/jakebharmon/saphl_vfc/raw/refs/heads/main/data/Estimated%20Number%20&%20Percent%20without%20Health%20Insurance%20by%20County%202020.csv") %>% 
  select(1,5,6,7)
insurance_gap <- insurance_gap[-c(1,48,49),] #Remove extraneous cols
insurance_gap$County <- str_replace_all(insurance_gap$County, " ", "") #Remove white space around text
insurance_gap[,2]<-parse_number(insurance_gap[,2])
insurance_gap[,3]<-parse_number(insurance_gap[,3])

### Create joined dataset
data(countypop) #call 2022 county South Carolina data from usmap
sc_county_pop<-countypop %>% 
  filter(abbr=="SC") %>% 
  mutate(county = str_remove(county," County"))

vfc_per_county<-vfc_providers %>% #Makes table
  summarize(n=n(),.by=County) 

vfc_per_county<-full_join(vfc_per_county,sc_county_pop,by=c("County"="county"))

vfc_per_county<-left_join(vfc_per_county,medicaid_coverage_rates[c(1,3)],by=c("County"="County.Name"))

vfc_per_county<-left_join(vfc_per_county,insurance_gap,by=c("County"="County"))

vfc_per_county<-vfc_per_county%>% 
  rename(
    "county"=1,
    "vfc_num"=2,
    "chip_pcnt"=6,
    "child_num"=7,
    "child_unins_num"=8,
    "child_unins_perc"=9
  ) %>% 
  mutate(chip_num=round(as.numeric(child_num)*chip_pcnt)/100) %>% 
  mutate(patient_provider_ratio=round((chip_num+child_unins_num)/vfc_num))

### Data analysis


# VA counties - downloaded via the awesome tigris package
shape <- tigris::counties(state = "SC", class = "sf") %>% 
  mutate(fips=paste0(STATEFP,COUNTYFP)) %>% 
  full_join(vfc_per_county,shape,by="fips")

mapview(shape,zcol="patient_provider_ratio")
