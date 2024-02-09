Height_data <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/Me playing with monitoring data/SECREMP_OCTO_RawData_2013.csv")
Height_data$Date <- format(as.Date(Height_data$Date,format = "%m/%d/%Y"))


library(data.table)

# Convert data frame to data.table
Height_dat <- as.data.table(Height_data)
glimpse()
# Convert data frame to data.table

# Modify the species names using update() function in data.table
Height_dat[sciName == "Pseudopterogorgia americana", sciName := "Antillogorgia_americana"]
Height_dat[sciName == "Eunicea flexuosa", sciName := "Eunicea_flexuosa"]
Height_dat[sciName == "Gorgonia ventalina", sciName := "Gorgonia_ventalina"]
Height_dat[sciName == "Pseudoplexaura porosa", sciName := "Pseudoplexaura_porosa"]
Height_dat[sciName == "Pseudopterogorgia bipinnata", sciName := "Antillogorgia_bipinnata"]


## Does data for pooled biomass is influenfed by the number of observations? 

# Arrange by n
biomass.pooled %>% 
  arrange(n)

# Can also view summary stats by high/low n 
biomass.pooled %>%
  mutate(n_group = ifelse(n > 100, "high", "low")) %>%
  group_by(n_group) %>%
  summarize(mean_biomass = mean(mean_biomass), 
            sd_biomass = sd(mean_biomass))

#Show correlation - using df = biomass.pooled from the Cremp data code 

glimpse(biomass.pooled)

# Correlation test
cor.test(biomass.pooled$n, biomass.pooled$mean_biomass)

#Ok there is a correlation, but this stems from more/less observation in fixed transect. I dont think there were more transects or sites in different years. 
#Lets examine this

#Biomass
site_summary <- Height_data %>%
  group_by(Year, Subregion) %>%
  summarise(num_sites = n_distinct(SiteID), num_stations = n_distinct(StationID))

site_wide <- site_summary %>%
  pivot_wider(names_from = Subregion, values_from = c(num_sites,num_stations))
site_wide
view(site_wide)
#Yes, there is one additional site in LK in 2015-2017. lets see which one it is 


  arrange(SiteID, Year)

View(LK)
#View(LK2013)



# Identify added site ID
added_site <- setdiff(LK_sites$sites[[5]], LK_sites$sites[[1]])

# it is <- 	"Red Dun Reef"
 #lets try to remove it from the data and see visulaized my data again. 
 
unique(pop.dat$Site_name)


view(total_biomass_nofilter)


#So if this didnt create the 2016 increase in the LK, what has? 

# lets try to look at the data itself and see if we I can track anything? 
LK.specific <- pop.dat %>% 
  filter(Subregion == "LK") %>% 
filter (Year == c("2015","2016","2017")) %>% 
  group_by(Year) %>%
  arrange(Site_name)
view(LK.2016)

ggplot(LK.specific,  aes(x = Site_name, y = biomass)) +
  geom_point()  + facet_wrap(~Year)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
 #Ecology metric
#Can we look at population stability? 

view(Sum_density)
density_long_dat <-  density_dat %>% pivot_longer(cols=c('Gorgonia_ventalina', 'Antillogorgia_americana', 'Eunicea_flexuosa','Antillogorgia_bipinnata', 'Pseudoplexaura_porosa' ,'Total_Octocorals'),
                                                  names_to='sciName',
                                                  values_to='density')
# Filter data to exclude Total_Octocorals
data <- density_long_dat %>% 
  filter(sciName != "Total_Octocorals") %>% 
  select(-Eunicea_calyculata)
glimpse(data)

######## Population synchrony

# Calculate variance of each species by year and subregion
species_vars <- data %>%
  group_by(Year, Subregion, sciName) %>%
  summarize(var_density = var(density, na.rm = TRUE))

# Calculate total variance by year and subregion
total_vars <- data %>% 
  group_by(Year, Subregion) %>%
  summarize(total_var = var(density, na.rm = TRUE)) 

# Join and calculate synchrony 
sync_data <- left_join(species_vars, total_vars)

sync_data <- sync_data %>%
  mutate(synchrony = total_var / sum(var_density))

view(sync_data)

# Make plot
ggplot(sync_data, aes(x = Year, y = synchrony)) +
  geom_jitter() +
  geom_path()+
  scale_x_continuous(breaks=seq(2011,2022,2))+
  #geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~Subregion) +
  labs(
    title = "Population Synchrony by Subregion",
    x = "Year",
    y = "Synchrony",
    color = "Subregion"
  )+
  guides(fill = guide_legend()) +
  theme(legend.direction = "horizontal",strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))

#Relative abundances: Look at each species as a proportion of total abundance per year. 
#Analyze how the relative abundances change over time.
# Group by year, subregion, site, species

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
view(join_density)

# Analyze relative abundance
join_density_sum <- join_density%>%
  #filter(sciName.y!="Total_Octocorals") %>% 
  group_by(Year,sciName.x,Subregion) %>%
  summarize(mean_rel = mean(rel_abun), sd_rel = sd(rel_abun)) 

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
  
  
  ggsave(filename = "Relative Abundance of species_CREMP_.jpeg", plot = p_rel_density,  width =25 , height = 35,dpi=600,  units = "cm")
  
###### Pooled and species density trends ########
  
  glimpse(combined)
  species_data <- combined %>%
    select(Year, Site_name, sciName, density) %>%
    filter(!is.na(density), !is.na(Year), !is.na(sciName), !is.na(Site_name)) 
 glimpse(species_data)
 str(species_data) 
 #To create a linear model for each combination of species and site, you can use the following R code:
 #rm(list = ls())
  
  # Create linear model for each combination of species and site
  model_results <- species_data %>%
    filter(sciName=="Total_Octocorals") %>% 
    group_by(Site_name) %>%
    do(model = lm(density ~ Year, data = .))
  
  model_results
  
  
  models <- species_data %>%
    #filter(sciName=="Total_Octocorals") %>% 
    group_by(Site_name) %>%
    do(model = lm(density ~ sciName, data = .)) %>%
    ungroup() %>% 
    transmute(Site_name, SiteCoef = map(model,tidy)) %>%
    unnest(Site_name)
    
  models
  #####################
  models <- species_data %>%
    filter(sciName!="Total_Octocorals") %>% 
    group_by(Site_name) %>%
    do(mod = glance(lm(density ~ Year, data = .)))%>%
    tidyr::unnest(mod)
  view(models)

#  This is a model for a change in each speceis/ pooled for each year and each site
    ###############################
  
  adjusted_models <- species_data %>%
    filter(sciName=="Total_Octocorals") %>% 
    group_by(Site_name) %>%
    do(mod = tidy(lm(density ~ Year, data = .))) %>%
    unnest(mod)
  adjusted_models

  # This is a model only for the pooled 

  # Filter for term = Year and arrange by slope, I am not intereted in the intercept which is the time 0 density comparison.
    year_term_summary <- adjusted_models %>%
      filter(term == "Year") %>%
      select(Site_name, term, estimate, p.value) %>%
      arrange(estimate)
   year_term_summary
   
    year_term_summary <-  year_term_summary %>%
      mutate(depth = case_when(
        grepl("Deep", Site_name) ~ "deep",
        grepl("Shallow", Site_name) ~ "shallow",
        TRUE ~ NA_character_
      ))
    year_term_summary
    # Binding metadata about the sites from 'combined' to 'year_term_summary' by 'Site_name'
    Site_pooled_octo_summary <- year_term_summary %>%
      left_join(combined %>% distinct(Site_name, Habitat, Subregion), by = "Site_name")
    Site_pooled_octo_summary
  
    #So in total, how many sites increase/ decrease or stayed the same? 
    # Generate a new table showing the count of significant increase, decrease, and no change
    # Adding average and SE to each of the 3 categories in the site table

    
    summary_of_Site_pooled_octo <- Site_pooled_octo_summary %>%
      mutate(change_category = case_when(
        estimate > 0 & p.value < 0.05 ~ "Significant Increase",
        estimate < 0 & p.value < 0.05 ~ "Significant Decrease",
        TRUE ~ "No Change"
      )) %>%
      group_by(Habitat,change_category) %>%
      summarise(
        average_estimate = mean(estimate),
        average_SE = sd(estimate) / sqrt(n()),
        count = n()
      )
     # View the summary of term = Year and the new table showing the count of change categories
    summary_of_Site_pooled_octo # 8/20 have increased! 40% in the Keys. 
    
    #### This is good for a basic linear model
    
    library(nlme)
    library(lme4)
    # Create the adjusted model with covariance and autocorrelation parameters
    
    # Fit a linear mixed-effects model with nested random effects for Year within Site_name
   repeated_model <- lmer(density ~ Year + (1 | Site_name/Year), data = species_data)    
    # View the adjusted_model
    summary(repeated_model)
    
    # Fit the lmer model
    models <- species_data %>%
      filter(sciName == "Total_Octocorals") %>% 
      group_by(Site_name) %>%
      do(mod = lmer(density ~ Year + (1 | Year:Site_name), data = .)) 
    
    # Retrieve model statistics
    models_summary <- summary(models$mod)
    models_summary
    
    # Iterate through the list of lmerMod objects and view the model summaries
    for (i in 1:length(models_summary)) {
      cat("Summary for Site", i, ":\n")
      print(summary(models_summary[[i]]))
    }
    
    #In this model I waant to know the rate of in the total density change in each site from the earliest data point to the most recent 
    recent_year_data <- species_data %>%
      filter(sciName=="Total_Octocorals") %>% 
      filter(!is.na(density) & !is.na(Year)) %>%  # Filter out rows with NA values for 'density' and 'Year'
      group_by(Site_name) %>%
      filter(Year %in% c(min(Year), max(Year)))  # Filter for the rows with the earliest and most recent year for each site
    
    # Calculate the total change in species density using the slope coefficient
    model_early_to_recent <- recent_year_data %>%
      group_by(Site_name) %>%
      do(mod = tidy(lm(density ~ Year, data = .))) %>%
      unnest(mod)
    
    # View the model-level summary statistics
    model_early_to_recent
    
    
  # Optional: Visualizing the trends.
  ggplot(species_data, aes(x = Year, y = density, group = interaction(sciName, Site_name), color = sciName)) +
    #geom_line(aes(linetype = Site_name)) +
    scale_x_continuous(breaks=seq(2011,2022,2))+ 
    geom_smooth(method = "lm", se = FALSE, aes(fill = sciName), alpha = 0.3) +
    facet_wrap(~Site_name, scales = "free_y") +
    labs(color = "Species", linetype = "Site", fill = "Species")
  