#This code i'll be using the rmarkdown code to analyse the SECREMP DATA for OCTOCORALS. 

#All data
#All data
Height_data <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/SECREMP_CSV_Files/SECREMP_OCTO_RawData_2012-2022.csv")

#format data 
Height_data$Date <- format(as.Date(Height_data$Date,format = "%m/%d/%Y"))

head(Height_data)
#View(Height_data)

# Summary of colony heights in the different transects while keeping the metadata 

Sum_height <- Height_data %>%
  filter(SPP_Code %in% c("PFLE","GVEN","PAME"))  %>%
  group_by(Year,SiteID,StationID, SPP_Code) %>% 
  mutate(mean_height = mean(Height_cm), sd=sd(Height_cm),se = sd / sqrt(n()), biomass=sum(Height_cm),n= n()) %>% 
  select(-Height_cm) %>% 
  distinct() %>% 
  mutate(sciName = dplyr::recode(sciName,
                                 'Pseudopterogorgia americana' = 'Antillogorgia_americana',
                                 "Eunicea flexuosa" = "Eunicea_flexuosa", 
                                 "Gorgonia ventalina" = "Gorgonia_ventalina","Eunicea calyculata"= "Eunicea calyculata", "Pseudoplexaura porosa"="Pseudoplexaura porosa", "Pseudopterogorgia bipinnata" = 'Antillogorgia_bipinnata')) %>% #have similar name to the density
  ungroup()

head(Sum_height)
view(Sum_height)

#Get the density data
#2013 only
#density_dat <-  read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/Me playing with monitoring data/SECREMP_OCTO_Summaries_2013_Density.csv" ) 

#ALL DATA
density_dat <-  read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/SECREMP_CSV_Files/SECREMP_OCTO_Summaries_2012-2022_Density.csv" ) 
glimpse(density_dat)
#Here I will work with all species I can.

pop.dat <- left_join(Sum_height,density_dat, by = c("Year", "Subregion" ,"Habitat" ,  "SiteID" ,   "Site_name" ,"StationID") )
glimpse(pop.dat)
pop.dat <- pop.dat %>% 
select(-c("Date.y", "Eunicea._calyculata","Gorgonia_ventalina","Antillogorgia_americana","Eunicea_flexuosa","Pseudoplexaura_porosa")) %>% 
rename(oct.density=15)   
view(pop.dat)
#just a check 
#LK2013 <- pop.dat %>% 
 # filter(Year == "2013") %>% 
  #filter(Subregion == "LK")
#View(LK2013)

#Now in pop dat I have the heights, biomass and density and of each species in each transect. 

#biomass will be x = years , (here only 2013). y is mean of biomass / cm2 
# Summarize data
biomass_means <- pop.dat %>% 
  group_by(Year,Subregion,sciName) %>%
  summarize(mean_biomass = mean(biomass), sd = sd(biomass),se = sd / sqrt(n()), n=n() ) %>% 
  ungroup()
#view(biomass_means)

#view(biomass_means)
total_biomass <- pop.dat %>% 
  group_by(Year,Subregion) %>%
  summarize(pooled.taxa_biomass = sum(biomass,na.rm=TRUE), n=n()) %>% 
  ungroup()
view(total_biomass)

#add also pooled data in each subregion. 
# Rename column in total_biomass
total_biomass <- total_biomass %>%
  rename(mean_biomass = pooled.taxa_biomass)
total_biomass$sciName= "pooled taxa" #prepare for binding rows

# Bind rows 
biomass_all <- bind_rows(biomass_means, total_biomass)
# View refined dataframe
view(biomass_all)

# Plot with facet wrap for both sciName and Subregion. Keep plot for pooled taxa seperate because of very different y axis scale. 

#Filter datasets
biomass.pooled <- biomass_all %>% 
  filter(sciName == "pooled taxa")
#%>% mutate(Year= as.character(Year) )
#view(biomass.CREMP.pooled)
biomass.species <- biomass_all %>%
  filter(sciName != "pooled taxa")
view(biomass.SECREMP.species)
# Plot for pooled - 
#this is an estimation as I dont have the heights of all the species in each 
#transect and therefore I can only estimate based on the 5-6 species that have been monitored.
p_biomass.pooled<-  
  ggplot(biomass.pooled, aes(x = Year, y = mean_biomass)) +
  geom_point() +
  geom_path()+
  # geom_errorbar(aes(ymin = mean_biomass - se, 
  #                  ymax = mean_biomass + se)) + #na at thme moment
  scale_x_continuous(breaks=seq(2012,2022,2))+
  facet_wrap(~Subregion) +
  labs(x = "Subregion",
       y = "Biomass proxy",
       title = "Pooled Taxa Biomass of three species by Subregion using SECREMP Data")
p_biomass.pooled

# Plot for each species
#filter(sciName!='Antillogorgia_bipinnata')  # optional to remove species from this analysis
indv_species_biomass<-  ggplot()+
  geom_jitter(data=pop.dat, aes(x=Year, y = biomass), alpha=0.4)+
  geom_line(data=biomass.species, aes(x = Year, y = mean_biomass, color = sciName ), linewidth = 2  )+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  facet_wrap(~sciName + Subregion, nrow=3) +labs(x = "Year",
                                                 y = "Biomass proxy",
                                                 title = "Biomass of octocorals by subregion using SECREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
indv_species_biomass

ggsave(filename = "Biomass_SECREMP_pooled species.jpeg", plot = p_biomass.pooled,  width =30 , height = 25,dpi=300,  units = "cm")

ggsave(filename = "Biomass_SECREMP_indv_species.jpeg", plot = indv_species_biomass,  width =25 , height =35,dpi=300,  units = "cm")

#Another plot i am not using this time

#CREMP.species <-  species %>% 
#filter(sciName!='Antillogorgia_bipinnata') 
#p_CREMP.species <- ggplot(CREMP.species, aes(x = Year, y = mean_biomass, color = sciName)) +
# geom_point() + 
#geom_errorbar(aes(ymin = mean_biomass - se,
#                 ymax = mean_biomass + se)) +
#scale_x_continuous(breaks=seq(2012,2022,2))+
#facet_wrap(~sciName + Subregion,nrow = 5) +
#labs(x = "Subregion",
#    y = "Biomass proxy cm2",
#   title = "Mean Biomass by Species and Subregion")+
#guides(fill = guide_legend()) +
#theme(legend.direction = "horizontal",strip.background = element_blank(),
#     legend.position = "bottom",
#    legend.title = element_blank(),
#   legend.text = element_text(color = "black", size = 10))


### Density 
#How does the population density change? in terms of proxy for all the population and individual species.
#I need to create a dataframe with those two df combined. for that we need to make density dat a long df

#step one: have similar names  done already
#Sum_height <- Sum_height %>% 
# mutate(sciName = dplyr::recode(sciName,
#                        'Pseudopterogorgia americana' = 'Antillogorgia_americana',
#                       "Eunicea flexuosa" = "Eunicea_flexuosa", 
#                      "Gorgonia ventalina" = "Gorgonia_ventalina","Eunicea calyculata"= "Eunicea calyculata", "Pseudoplexaura porosa"="Pseudoplexaura porosa", "Pseudopterogorgia bipinnata" = 'Antillogorgia_bipinnata'))
#view(Sum_height)
#step 2: make long 
density_long_dat <-  density_dat %>% pivot_longer(cols=c('Gorgonia_ventalina', 'Antillogorgia_americana', 'Eunicea_flexuosa' ,'Total_Octocorals'),
                                                  names_to='sciName',
                                                  values_to='density') %>% 
  select(-c("Eunicea._calyculata", "Pseudoplexaura_porosa"))
view(density_long_dat)
#step 3: combine data
combined <- left_join(density_long_dat, Sum_height,by = c("Year", "Subregion" ,"Habitat" ,  "SiteID" ,   "Site_name" ,"StationID","sciName" ) )

#view(combined)

#Sumemrize data
Sum_density <- combined %>%
  #filter(SPP_Code %in% c("PFLE","GVEN","PAME"))  %>%
  #  group_by(SiteID,StationID, SPP_Code) %>% 
  group_by(Year,sciName,Subregion) %>%
  summarize(mean_density = mean(density), sd = sd(density),se = sd / sqrt(n()), n=n() )
view(Sum_density)

#Separate the summary by pooled and unpoold (species only). 
dense_pooled <- Sum_density %>% 
  filter(sciName == "Total_Octocorals", Subregion != "MC")

dense_species <-Sum_density %>% 
  filter(sciName != "Total_Octocorals", Subregion != "MC")
view(dense_pooled)

#seperate the all the dataset with relevant long data and pooled/non pooled
density_species <- combined %>% 
  select(-Date.y) %>% 
  filter(sciName != "Total_Octocorals", Subregion != "MC")

density_pooled <- combined %>% 
  select(-Date.y) %>% 
  filter(sciName == "Total_Octocorals" , Subregion != "MC")
#unique(factor(density_pooled$Subregion))
#plot species density per year in the different regions 

total_species_density <-  ggplot()+
  geom_jitter(data=density_species, aes(x=Year, y = density), alpha=0.4)+
  geom_line(data=dense_species, aes(x = Year, y = mean_density, color = sciName ), linewidth = 2  )+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  facet_wrap(~sciName + Subregion) +labs(x = "Year",
                                         y = "Colonies m2",
                                         title = "Mean density of total octocorals by subregion in SE Florida using SECREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
total_species_density

#Now for pooled data

total_pool_density <-  ggplot()+
  geom_jitter(data=density_pooled, aes(x=Year, y = density), color="orange", alpha=0.4)+
  geom_line(data=dense_pooled, aes(x=Year, y = mean_density), color="skyblue" , linewidth = 2  )+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  facet_wrap(~sciName + Subregion) +labs(x = "Year",
                                         y = "Colonies m2",
                                         title = "Mean density of total octocorals by subregion in SE Florida using SECREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))

total_pool_density

#export
ggsave(filename = "Density_SECREMP_pooled species.jpeg", plot = total_pool_density,  width =30 , height = 25,dpi=300,  units = "cm")

ggsave(filename = "Density_SECREMP_indv_species.jpeg", plot = total_species_density,  width =30 , height = 25,dpi=300,  units = "cm")


#Relative abundances:
density_long_all <-  density_dat %>% pivot_longer(cols=c('Gorgonia_ventalina', 'Antillogorgia_americana', 'Eunicea_flexuosa' ,'Total_Octocorals'),
                                                  names_to='sciName',
                                                  values_to='density')
eco_sum_density <- density_long_all %>%
  #select(-c(Eunicea_calyculata,Date.y)) %>% 
  #filter(SPP_Code %in% c("PFLE","GVEN","PAME"))  %>%
  #  group_by(SiteID,StationID, SPP_Code) %>% 
  group_by(Year,sciName,Subregion,Site_name) %>%
  summarize(mean_density = mean(density), sd = sd(density),se = sd / sqrt(n()), n=n() )
view(eco_sum_density)

unique(density_long_dat$sciName)

eco_sum_species <- eco_sum_density %>% 
  filter(sciName != "Total_Octocorals") 

eco_sum_pooled <- eco_sum_density %>% 
  filter(sciName == "Total_Octocorals") 
eco_sum_pooled <- eco_sum_pooled%>% 
  rename(mean_all = mean_density, 
         sd_all = sd,  
         se_all = se,
         n_all = n)

glimpse(eco_sum_pooled)

join_density <- left_join(eco_sum_density, eco_sum_pooled,
                          by = c("Year", "Subregion" ,"Site_name") )

join_density <- join_density %>%
  filter(Subregion!="MC") %>% 
  mutate(rel_abun = mean_density/mean_all) %>% 
  filter(sciName.x!="Total_Octocorals")


# How much of the total abundance does the CREMP data explain?
rel.explanied <- join_density%>%
  group_by(Year,Subregion,Site_name,) %>%
  summarize(explined_rel = sum(rel_abun)) 
glimpse(rel.explanied)  

#view(rel.explanied)
#rel.explanied.med <- rel.explanied %>%
 # group_by(Year, Subregion) %>%
  #summarise(med = quantile(explined_rel, 0.5,na.rm= T))

p_pooled_rel <- ggplot(rel.explanied, aes(x = Year, y = explined_rel, group= Year)) +
  geom_boxplot() +
  #  geom_text(aes(label = round(med, 2))) +
  facet_wrap(~Subregion) +
  #ggplot(rel.explanied, aes(x = Year, y = explined_rel)) +
  # Violin with median line
  #geom_violin(alpha = 0.2, draw_quantiles = c(0.5)) +
  scale_x_continuous(breaks=seq(2012,2022,2))+
  # Points for each site
  #geom_jitter(aes(fill = Site_name), 
  #position = position_jitter(width = 0.2),
  #alpha = 0.5) +
  # Remove Subregion color mapping
  #guides(color = "none") + 
  facet_wrap(~Subregion) +
  labs(
    title = "Explained Relative Abundance by Subregion in SE-Florida using SECREMP data",
    x = "Year",
    y = "Explined Relative Abundance"
  )

p_pooled_rel
#~ 15-30% of the total abundance is explained by 3 species - pretty bad.

#Relative abundances by species: Look at each species as a proportion of total abundance per year. 
#Analyze how the relative abundances change over time.
# Analyze relative abundance per species
join_density_sum <- join_density%>%
  #filter(sciName.y!="Total_Octocorals") %>% 
  group_by(Year,sciName.x,Subregion) %>%
  summarize(mean_rel = mean(rel_abun, na.rm=T), sd_rel = sd(rel_abun, na.rm=T)) 

p_rel_density <- ggplot(data=join_density_sum, aes(x = Year, y = mean_rel)) + 
  geom_jitter(data = join_density, aes(x = Year, y = rel_abun, color = sciName.x), alpha = 0.5)+
  geom_path(linewidth=1) +
  facet_wrap(~sciName.x+Subregion, ncol=3)+
  scale_x_continuous(breaks=seq(2011,2022,2))+ 
  labs(x = "Year",
       y = "Mean relative abundance",
       title = "Mean relative abundance of octocorals by subregion in SE-Florida using SECREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
p_rel_density

#export *************
ggsave(filename = "Relative Abundance of pooled_taxa_SECREMP.jpg", plot = p_pooled_rel,  width =30 , height = 25,dpi=600,  units = "cm")

ggsave(filename = "Relative Abundance of species_SECREMP.jpg", plot = p_rel_density,width =25 , height = 35,dpi=600,  units = "cm")

# **********

