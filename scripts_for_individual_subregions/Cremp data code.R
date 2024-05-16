#This code i'll be using the rmarkdown code to analyse the CREMP DATA for octocorlas. 


#All data
Height_data <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/CREMP_CSV_Files/CREMP_OCTO_RawData_2011-2022.csv")
glimpse(Height_data)

#format data 
Height_data$Date <- format(as.Date(Height_data$Date,format = "%m/%d/%Y"))

head(Height_data)
#View(Height_data)

# Summary of colony heights in the different transects while keeping the metadata.
# Biomass is calculated for each species in each station by sum of the heights in each station, 
# n is the number of observation of colonies from this species, so basically if a species is more common I expeced highr biomass (doh). 
# I keep the sampling unit by calculating per sttion , unit of sample. 

Sum_height <- Height_data %>%
  #filter(SPP_Code %in% c("PFLE","GVEN","PAME"))  %>%
  filter(sciName!="Eunicea calyculata") %>% 
  group_by(Year,SiteID,StationID, SPP_Code) %>% 
  mutate(mean_height = mean(Height_cm), sd=sd(Height_cm),se = sd / sqrt(n()), biomass=sum(Height_cm),n= n()) %>% 
  select(-Height_cm) %>% 
  distinct() %>% 
  mutate(sciName = dplyr::recode(sciName,
                                 'Pseudopterogorgia americana' = 'Antillogorgia_americana',
                                 "Eunicea flexuosa" = "Eunicea_flexuosa", 
                                 "Gorgonia ventalina" = "Gorgonia_ventalina", "Pseudoplexaura porosa"="Pseudoplexaura porosa", "Pseudopterogorgia bipinnata" = 'Antillogorgia_bipinnata')) %>% #have similar name to the density
  ungroup()

head(Sum_height)
#view(Sum_height)
unique(Sum_height$sciName)
#Get the density data
#2013 only
#density_dat <-  read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/Me playing with monitoring data/SECREMP_OCTO_Summaries_2013_Density.csv" ) 

#ALL DATA
density_dat <-  read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/CREMP_CSV_Files/CREMP_OCTO_Summaries_2011-2022_Density.csv" ) 

glimpse(density_dat)
#view(density_dat)
#Here I will work with all species I can.

pop.dat <- left_join(Sum_height,density_dat, by = c("Year", "Subregion" ,"Habitat" ,  "SiteID" ,   "Site_name" ,"StationID") )
glimpse(pop.dat)
pop.dat <- pop.dat %>%
  select(-c("Date.y", "Eunicea_calyculata","Gorgonia_ventalina","Antillogorgia_americana","Eunicea_flexuosa","Pseudoplexaura_porosa")) %>% 
 # select(-c(15:21)) %>% 
  rename(oct.density=15)
view(pop.dat)
#just a check 
LK2013 <- pop.dat %>% 
  filter(Year == "2013") %>% 
  filter(Subregion == "LK")
#View(LK2013)

#Now in pop dat I have the heights, biomass and density and of each species in each transect. 
#But has the data been collected similarly across years? 
  
#A bit of Data examination 

#Biomass
site_summary <- Height_data %>%
  group_by(Year, Subregion) %>%
  summarise(num_sites = n_distinct(SiteID), num_stations = n_distinct(StationID))

site_wide <- site_summary %>%
  pivot_wider(names_from = Subregion, values_from = c(num_sites,num_stations))
site_wide
view(site_wide)

# It seems that in 2015-2017 another site has been added in LK . this can skew the results. 
#This site was "Red Dun Reef"
#Therefore, I am removing this site from the analysis of biomass and density. 

#It also seems as 2017 had less locations in the MK - possibly becasue of Hurricanes? 
#but this did not effect the data, if any it is a case of more biomass in less sites.. 

#Also, 2011 has less satiton in the UP and thus should be filtered out too
#biomass of each indv. species in each location using x = years , y is mean of biomass / cm2 

# Summarize data
#rm(biomass_means)
biomass_means <- pop.dat %>% 
  filter(Site_name != "Red Dun Reef") %>% 
  filter(!(Subregion == "UK" & Year == 2011)) %>% 
  group_by(Year,Subregion,sciName) %>%
  summarize(mean_biomass = mean(biomass), sd = sd(biomass),se = sd / sqrt(n()), n=n() ) %>% 
  ungroup()
view(biomass_means)

#calculating the total biomass of all the species heights in each location 
# - this is the pooled data using 5 species
#rm(total_biomass)
glimpse(total_biomass)
total_biomass <- pop.dat %>% 
  filter(Site_name != "Red Dun Reef") %>% 
  filter(!(Subregion == "UK" & Year == 2011)) %>% 
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
view(biomass.pooled)
biomass.species <- biomass_all %>%
  filter(sciName != "pooled taxa")

# Plot for pooled - 
#this is an estimation as I dont have the heights of all the species in each 
#transect and therefore I can only estimate based on the species that have been monitored.

p_biomass.pooled<-  
  ggplot(biomass.pooled, aes(x = Year, y = mean_biomass)) +
  geom_point() +
  geom_path()+
 # geom_errorbar(aes(ymin = mean_biomass - se, 
  #                  ymax = mean_biomass + se)) + #na at thme moment
  scale_x_continuous(breaks=seq(2012,2022,2))+
  facet_wrap(~Subregion) +
  labs(x = "Subregion",
       y = "Biomass proxy",title = "Pooled Taxa Biomass (of 5 species) by Subregion in Florida keys using CREMP data")+
  geom_vline(xintercept = 2017, color = "gray75",alpha=0.5, linewidth = 2)  +theme(plot.title = element_text(size = 12))

p_biomass.pooled
 # Plot for each species
#filter(sciName!='Antillogorgia_bipinnata')  # optional to remove species from this analysis
indv_species_biomass<-  ggplot()+
  geom_jitter(data=pop.dat, aes(x=Year, y = biomass), alpha=0.4)+
  geom_line(data=biomass.species, aes(x = Year, y = mean_biomass, color = sciName ), linewidth = 2  )+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  facet_wrap(~sciName + Subregion, nrow=5, scales = "free_y") +labs(x = "Year",
                                         y = "Biomass proxy cm2",
                                         title = "Biomass of octocorals by subregion in Florida KEYS")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10),plot.title = element_text(size = 12))
indv_species_biomass

unique(pop.dat$sciName)

#adding unique y axis
#install.packages("remotes")
#remotes::install_github("teunbrand/ggh4x")
library(ggh4x)

indv_species_biomass <- indv_species_biomass +
  facetted_pos_scales(
    y = list(
      sciName == "Antillogorgia_americana" ~ scale_y_continuous(limits = c(0,4000), breaks = c(0,1000,2500,4000)),
      sciName == "Pseudoplexaura porosa" ~ scale_y_continuous(limits = c(0,1000), breaks = c(0,500,1000)),
      sciName == "Eunicea_flexuosa" ~ scale_y_continuous(limits = c(0,1000), breaks = c(0,500,1000)),
      sciName == "Gorgonia_ventalina" ~ scale_y_continuous(limits = c(0,4000), breaks = c(0,1000,2500,4000)),
      sciName == "Antillogorgia_bipinnata" ~ scale_y_continuous(limits = c(0,3000), breaks = c(0,1000,2000,3000))
      
    )
  )+
  geom_vline(xintercept = 2017, color = "gray75",alpha=0.5, linewidth = 1.5)  
indv_species_biomass


ggsave(filename = "Biomass_CREMP_pooled species.png", plot = p_biomass.pooled,  width =25 , height = 10,dpi=600, bg = "white", units = "cm")

ggsave(filename = "Biomass_CREMP_indv_species.png", plot = indv_species_biomass,  width =25 , height =35,dpi=600, bg = "white", units = "cm")

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
view(density_dat)
glimpse(density_dat)
#step 2: make long 
density_long_dat <-  density_dat %>% pivot_longer(cols=c('Gorgonia_ventalina', 'Antillogorgia_americana', 'Eunicea_flexuosa','Antillogorgia_bipinnata', 'Pseudoplexaura_porosa' ,'Total_Octocorals'),
                                                  names_to='sciName',
                                                  values_to='density')

#step 3: combine data
combined <- left_join(density_long_dat, Sum_height,by = c("Year", "Subregion" ,"Habitat" ,  "SiteID" ,   "Site_name" ,"StationID","sciName" ) )
combined <- combined %>% #filter problematic data points
filter(Site_name != "Red Dun Reef") %>% 
  filter(!(Subregion == "UK" & Year == 2011)) %>% 
  select(-SPP_Code)
view(combined)
glimpse(combined)
#Sumemrize data
Sum_density <- combined %>%
  select(-c(Eunicea_calyculata,Date.y)) %>% 
  #filter(SPP_Code %in% c("PFLE","GVEN","PAME"))  %>%
  #  group_by(SiteID,StationID, SPP_Code) %>% 
  group_by(Year,sciName,Subregion) %>%
  summarize(mean_density = mean(density), sd = sd(density),se = sd / sqrt(n()), n=n() )
view(Sum_density)

#Separate the summary by pooled and unpoold (species only). 
dense_pooled <- Sum_density %>% 
  filter(sciName == "Total_Octocorals")

dense_species <-Sum_density %>% 
  filter(sciName != "Total_Octocorals")
#view(dense_pooled)

#seperate the all the dataset with relevant long data and pooled/non pooled
density_CREMP_species <- combined %>% 
  #select(-c(Date.x, Eunicea_calyculata,Antillogorgia_bipinnata,Pseudoplexaura_porosa, Date.y)) %>% 
  filter(sciName != "Total_Octocorals")

density_CREMP_pooled <- combined %>% 
  #select(-c(Date.x, Eunicea_calyculata,Antillogorgia_bipinnata,Pseudoplexaura_porosa, Date.y)) %>% 
  filter(sciName == "Total_Octocorals")

#plot species density per year in the different regions 

total_species_density <-  ggplot()+
 geom_jitter(data=density_CREMP_species, aes(x=Year, y = density), alpha=0.4)+
  geom_line(data=dense_species, aes(x = Year, y = mean_density, color = sciName ), linewidth = 2  )+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  facet_wrap(~sciName + Subregion, nrow=5,scales = "free_y") +labs(x = "Year",
                                         y = "Colonies m2",
                                         title = "Mean density of octocoral spp. by subregion in Florida KEYS using CREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10),plot.title = element_text(size = 12))
total_species_density

total_species_density <- total_species_density +
  facetted_pos_scales(
    y = list(
      sciName == "Antillogorgia_americana" ~ scale_y_continuous(limits = c(0,20), breaks = c(0,10, 20)),
      sciName == "Pseudoplexaura porosa" ~ scale_y_continuous(limits = c(0,3), breaks = c(0,1.5,3)),
      sciName == "Eunicea_flexuosa" ~ scale_y_continuous(limits = c(0,3), breaks = c(0,1.5,3)),
      sciName == "Gorgonia_ventalina" ~ scale_y_continuous(limits = c(0,25), breaks = c(0,10,25)),
      sciName == "Antillogorgia_bipinnata" ~ scale_y_continuous(limits = c(0,15), breaks = c(0,7.5,15))
      
    )
  )+
  geom_vline(xintercept = 2017, color = "gray75",alpha=0.5, linewidth = 1.5)  

total_species_density

#Pooled data#######
total_pool_density <-  ggplot()+
  geom_jitter(data=density_CREMP_pooled, aes(x=Year, y = density), color="orange", alpha=0.8)+
  geom_line(data=dense_pooled, aes(x=Year, y = mean_density), color="black" , linewidth = 2  )+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  facet_wrap(~sciName + Subregion) +labs(x = "Year",
                                         y = "Colonies m2",
                                         title = "Mean density of total octocorals by subregion in Florida KEYS using CREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10),plot.title = element_text(size=12))+
  geom_vline(xintercept = 2017, color = "gray75",alpha=0.5, linewidth = 1.5)  

total_pool_density

#export *************
ggsave(filename = "Density_CREMP_pooled species.png", plot = total_pool_density,  width =30 , height = 25,dpi=600, bg="white", units = "cm")

ggsave(filename = "Density_CREMP_indv_species.png", plot = total_species_density,  width =30 , height = 25,dpi=600, bg="white", units = "cm")

# **********

#Relative abundances:
eco_sum_density <- density_long_dat %>%
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
  mutate(rel_abun = mean_density/mean_all) %>% 
  filter(sciName.x!="Total_Octocorals")

# How much of the total abundance does the CREMP data explain?
rel.explanied <- join_density%>%
  #filter(sciName.y!="Total_Octocorals") %>% 
  group_by(Year,Subregion,Site_name,) %>%
  summarize(explined_rel = sum(rel_abun)) 
glimpse(rel.explanied)  

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
  scale_x_continuous(breaks=seq(2011,2022,2))+
  # Points for each site
  #geom_jitter(aes(fill = Site_name), 
  #position = position_jitter(width = 0.2),
  #alpha = 0.5) +
  # Remove Subregion color mapping
  #guides(color = "none") + 
  facet_wrap(~Subregion) +
  labs(
    title = "Explained Relative Abundance by Subregion in the KEYS using CREMP data",
    x = "Year",
    y = "Explined Relative Abundance"
  )
p_pooled_rel
#~ 70% of the total abundance is explained by 5 species - not bad, not perfect , and ton of room for improvment 
#Variability in UK is higher. 
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
       title = "Mean relative abundance of octocorals by subregion in Florida KEYS using CREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
p_rel_density

#export *************
ggsave(filename = "Relative Abundance of pooled_taxa_CREMP.jpg", plot = p_pooled_rel,  width =30 , height = 25,dpi=600,  units = "cm")

ggsave(filename = "Relative Abundance of species_CREMP.jpg", plot = p_rel_density,width =25 , height = 35,dpi=600,  units = "cm")

# **********
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Explore the data by site

#Plot density by site Name

total_pool_density_in_site <-  ggplot()+
  geom_jitter(data=density_CREMP_pooled, aes(x=Year, y = density), color="orange")+
  #geom_line(data=dense_pooled, aes(x=Year, y = mean_density), color="skyblue" , linewidth = 2  )+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  facet_wrap(~Site_name) +labs(x = "Year",
                                         y = "Colonies m2",
                                         title = "Mean density of total octocorals by Site in Florida KEYS using CREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
total_pool_density_in_site

#Site name by species
head(density_CREMP_species)

library(RColorBrewer)

colors <- brewer.pal(5, "Set2")

scale_color_discrete <- scale_color_manual(values = colors)

total_species_density_By_site <-  ggplot()+
  geom_jitter(data=density_CREMP_species, aes(x=Year, y = density, color = sciName ))+
  #geom_line(data=dense_species, aes(x = Year, y = mean_density, color = sciName ), linewidth = 2  )+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  scale_color_manual(values = colors)+
  facet_wrap(~Subregion+Site_name) +labs(x = "Year",
                                                 y = "Colonies m2",
                        title = "Mean density of octocoral spp. by Site in 
                               Florida KEYS using CREMP data")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
total_species_density_By_site

ggsave(filename = "exploring_Density_CREMP_indv_by_Site.jpg", plot = total_species_density_By_site,  width =30 , height = 35,dpi=600,  units = "cm")

### not using! ***************************************************

#another option for plotting which use only the summary and the SE
#Filter datasets

p_CREMP.density_species <- ggplot(dense_species, aes(x = Year, y = mean_density, color = sciName)) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean_density - se,
                    ymax = mean_density + se)) +
  scale_x_continuous(breaks=seq(2012,2022,2))+
  facet_wrap(~sciName + Subregion) +
  labs(x = "Year",
       y = "Colonies m2",
       title = "Mean density of octocorals species by Subregion in Florida keys")+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
p_CREMP.density_species
