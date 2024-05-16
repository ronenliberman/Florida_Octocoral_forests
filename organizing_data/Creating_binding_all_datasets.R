# Using this code to bind similar individual datasets from SE/CREMP projects 

#Heights 
Height_SE <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/SECREMP_CSV_Files/SECREMP_OCTO_RawData_2012-2022.csv")
Height_CREMP <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/CREMP_CSV_Files/CREMP_OCTO_RawData_2011-2022.csv")
Height_DT <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/DTCREMP_CSVs/DTCREMP_OCTO_RawData_2011-2022.csv")

# bind height data 

head(Height_CREMP,10)

Height_all <- rbind(Height_SE, Height_CREMP, Height_DT) 

#remove 2011

Height_all <- filter(Height_all, Year != 2011)
#omit sites that were identified as not resembling octocoralpop.

head(Height_all,10)

# Count the number of unique SPP_Code
unique_spp_count <- Height_all %>% 
  distinct(SPP_Code) 
unique_spp_count
#remove data on E. calyculata
Height_all <- filter(Height_all, SPP_Code != "ECAL")

# Summary of colony heights for each species.  keeping the Transect/station level

Sum_height_all <- Height_all %>%
  #filter(SPP_Code %in% c("PFLE","GVEN","PAME"))  %>%
  group_by(Year,SiteID,StationID, SPP_Code) %>% 
  mutate(mean_height = mean(Height_cm), sd= sd(Height_cm) ,biomass=sum(Height_cm), n= n()) %>% 
  select(-Height_cm) %>% 
  distinct() %>% 
  mutate(sciName = dplyr::recode(sciName,
                                 'Pseudopterogorgia americana' = 'Antillogorgia_americana',
                                 "Eunicea flexuosa" = "Eunicea_flexuosa", 
                                 "Gorgonia ventalina" = "Gorgonia_ventalina", "Pseudoplexaura porosa"="Pseudoplexaura porosa", "Pseudopterogorgia bipinnata" = 'Antillogorgia_bipinnata')) %>% #have similar name to the density
  ungroup()

Sum_height_all[1:10]



# bind density data 
#ALL DATA

density_DT<-  read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/DTCREMP_CSVs/DTCREMP_OCTO_Summaries_2011-2022_Density.csv" ) 

density_SE <-  read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/SECREMP_CSV_Files/SECREMP_OCTO_Summaries_2012-2022_Density.csv" ) 
density_CREMP <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/CREMP_CSV_Files/CREMP_OCTO_Summaries_2011-2022_Density.csv" ) 

# Combine the datasets
combined_dens <- bind_rows(density_DT, density_SE, density_CREMP)
head(combined_dens, 20)

#step 1: make long 
density_long_dat <-  combined_dens %>% 
  select(-c(Eunicea_calyculata,Eunicea._calyculata )) %>% 
pivot_longer(cols=c('Gorgonia_ventalina', 'Antillogorgia_americana', 'Eunicea_flexuosa','Antillogorgia_bipinnata', 'Pseudoplexaura_porosa' ,'Total_Octocorals'),
                                                   names_to='sciName',
                                                   values_to='density') %>% 
  filter(Year != 2011) 

head(density_long_dat)

#ONE DATASET TO RULE THEM ALL 

combined <- left_join(density_long_dat, Sum_height_all,by = c("Year", "Subregion" ,"Habitat" ,  "SiteID" ,   "Site_name" ,"StationID","sciName" ) )
combined <- combined %>% #filter problematic data points
  filter(!(Site_name %in% omit_sites)) %>%  # omiting irrelevant sites !
  filter(!(Subregion == "UK" & Year == 2011))%>% 
  select(-Date.y)
head(combined, 20)

library(dplyr)

# Add SPP_Code based on conditions
combined <- combined %>%
  mutate(
    SPP_Code = case_when(
      sciName == "Antillogorgia_bipinnata" ~ "PBIP",
      sciName == "Pseudoplexaura_porosa" ~ "PPOR",
      sciName == "Total_Octocorals" ~ "TOCT",
      TRUE ~ SPP_Code  # Keep existing SPP_Code for other cases
    )
  )

# Display the updated dataset
head(combined, 20)

# Add station metadata - this is the table I got from NICK. 

transect_meta <- read.csv("~/Postdocing_NSU/Octocoral forests/Manuscript/raw_data/StationMetadata.csv")
head(transect_meta)

meta_dat <- transect_meta %>% 
rename(StationID = stationid, SiteID = siteid)
 head(meta_dat)

 # Join the combined dataset with meta_dat based on SiteID and StationID
 combined_with_metadata <- left_join(combined, meta_dat, by = c("SiteID", "StationID"))
 
 # Display the first few rows of the new dataset
 view(combined_with_metadata)
 head(combined_with_metadata)
 
 # Create a new column to indicate if there is a match between Site_name and sitename (1 if match, 0 if not)
 combined_with_metadata$sitename_match <- ifelse(combined_with_metadata$Site_name == combined_with_metadata$sitename, 1, 0)
 # Summarize the counts of 0s and 1s for sitename matches
 sitename_summary <- table(combined_with_metadata$sitename_match)
 sitename_summary
 
 colnames(combined_with_metadata)
 
 # Creating a list of sites that should be omitted from the datasets 
 omit_sites <- c("Martin County 1" ,"Martin County 2", "Martin County 3", "BCA")
 
 # Create a new dataset "all dat" with the required modifications and excluding the specified sites
 all_dat <- combined_with_metadata %>%
   filter(!(Site_name %in% omit_sites)) %>%
   select(-sitename, -habitatid, -length, -habitat_match, -sitename_match) %>% 
   rename(Depth = offshoreDepth)
 
 view(all_dat)
 # Export the updated dataset to a CSV file
 
 write.csv(all_dat, file = "~/Postdocing_NSU/Octocoral forests/Manuscript/all_density_and_heights_by_stationID.csv", row.names = FALSE)
 
 
 
