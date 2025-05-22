###Install packages
install.packages("usmap")
install.packages("tidygeocoder")
install.packages("mapview")
install.packages("writexl")
library(usmap)
library(tidyverse)
library(readxl)
library(tidygeocoder)
library(mapview)
library(writexl)


###Vaccines For Children Data
vfc_providers <- read_excel("~/Desktop/ActiveVFCProviders.xlsx") %>% 
  mutate(County = str_to_title(County))

vfc_providers[vfc_providers=="Mccormick"]<-"McCormick" #Standardize troublesome county name

data(countypop) #call 2022 county South Carolina data from usmap

vfc_coord<- vfc_providers %>%  #Currently produces 516/671 coordinates for location
  mutate(address = str_c(`Address 1`, City, `State/Territory`, Zip,sep=", ")) %>% 
  select(`Provider Name`, address) #%>%
# geocode(address, method = 'osm', lat = latitude , long = longitude)

###Medicaid data
#Rates of child vs adult vs senior
medicaid_coverage_rates <- read.csv("~/Desktop/data-eUlDP.csv") %>% 
  mutate(County.Name =  str_remove(County.Name," County"))

#Amounts and Percentages of uninsured by adult and child
insurance_gap <- read.csv("~/Desktop/Estimated Number & Percent without Health Insurance by County 2020.csv") %>% 
  select(1,5,6,7)
insurance_gap <- insurance_gap[-c(1,48,49),] #Remove extraneous cols
insurance_gap$County <- str_replace_all(insurance_gap$County, " ", "") #Remove white space around text
insurance_gap[,2]<-parse_number(insurance_gap[,2])
insurance_gap[,3]<-parse_number(insurance_gap[,3])
###Create joined dataset
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
  mutate(patient_provider_ratio=(chip_num+child_unins_num)/vfc_num)
summary(vfc_per_county)

###Data analysis
plot(vfc_per_county$vfc_num,vfc_per_county$chip_num)
plot(vfc_per_county$vfc_num,vfc_per_county$child_unins_num)

plot_usmap(data=vfc_per_county,
           linewidth = 0.1,
           values="patient_provider_ratio",
           region="counties",
           include="SC") +
  scale_fill_gradient(
    labels = scales::label_number(big.mark = ','),
    high = '#003366', #South Carolina Blue
    low = 'white'
  ) +
  theme(legend.position = 'left', plot.title = element_text(face="bold")) +
  labs(fill = 'Ratio',title="Ratio of Uninsured/CHIP Patients per VFC Provider") 
)

ggsave("sc_ppp_ratio.png")
?ggsave
mapview(vfc_per_county,zcol="patient_provider_ratio")

write.csv(vfc_per_county, "/Users/jake/Desktop/purple air/vfc_per_county.csv")
