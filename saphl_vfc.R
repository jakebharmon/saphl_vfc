###Install packages
#library(dplyr)
#library(readr)
#library(usmap)

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
insurance_gap[,2]<-parse_number(insurance_gap[,2]) #Character -> Number
insurance_gap[,3]<-parse_number(insurance_gap[,3]) #Character -> Number

### Create joined dataset
data(countypop) #Call 2022 county South Carolina data from usmap
sc_county_pop<-countypop %>% 
  filter(abbr=="SC") %>% 
  mutate(county = str_remove(county," County"))

vfc_per_county<-vfc_providers %>% #Makes table of VFC's per county
  summarize(n=n(),.by=County) 

#Join VFC with Census data
vfc_per_county<-full_join(vfc_per_county,sc_county_pop,by=c("County"="county"))

#Join VFCs with CHIP data
vfc_per_county<-left_join(vfc_per_county,medicaid_coverage_rates[c(1,3)],by=c("County"="County.Name"))

#Join VFCs with Uninsurance Data
vfc_per_county<-left_join(vfc_per_county,insurance_gap,by=c("County"="County"))

vfc_per_county<-vfc_per_county%>% 
  rename( #Clean county names
    "county"=1,
    "vfc_num"=2,
    "chip_pcnt"=6,
    "child_num"=7,
    "child_unins_num"=8,
    "child_unins_perc"=9
  ) %>% 
  mutate(chip_num=round(as.numeric(child_num)*chip_pcnt)/100) %>% 
  mutate(patient_provider_ratio=round((chip_num+child_unins_num)/vfc_num)) %>% #Create ppr
  arrange(county) %>% #Sort alphabetically for merging
  mutate(grp=FALSE) #Create column for highlighting counties on graph

### Data analysis 
shape <- tigris::counties(state = "SC", class = "sf") %>% #Call county shape files
  mutate(fips=paste0(STATEFP,COUNTYFP)) %>% 
  full_join(vfc_per_county,shape,by="fips") %>% 
  arrange(county)

#Create base graph
p<-ggplot(vfc_per_county,aes(x=vfc_num,(child_unins_num+chip_num),color=grp),) +
  geom_jitter(alpha=0.8,size=3,shape=18,width=3,height=3) +
  labs(x="Number of VFC's Per County",
       y="Number of Children with CHIP or No Insurance") +
  theme_classic() +
  theme(legend.position = "none")  +
  scale_color_manual(breaks = c("TRUE","FALSE"),
                   values=c("red","#E5E5E5"))

#Replicate ggplot for each county
p <- mget(rep("p", nrow(vfc_per_county)))

#Tag each county in its respective graph
for (i in 1:length(p)){
  p[[i]]$data[i,12]=TRUE
}

p <- lapply(1:length(p), function(i) {
  p[[i]]<-p[[i]] + labs(title=paste(p[[1]]$data[i,1],"County")) + theme( #Add county name as title
    plot.title = element_text(face = "bold", size = 12,hjust = 0.5))  +#Bold
  scale_alpha_ordinal()
})

#Create interactive map of South Carolina
mapview::mapview(shape,zcol="patient_provider_ratio",
        popup=leafpop::popupGraph(p, width = 300, height = 300))

