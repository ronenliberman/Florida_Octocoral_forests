# Using this code to bind similar individual datasets from SE/CREMP projects 

#Heights 
Height_SE <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/SECREMP_CSV_Files/SECREMP_OCTO_RawData_2012-2022.csv")
Height_CREMP <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/CREMP_CSV_Files/CREMP_OCTO_RawData_2011-2022.csv")
Height_DT <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/DTCREMP_CSVs/DTCREMP_OCTO_RawData_2011-2022.csv")

#Use this for dataset 2011-2023 
rm(c("Height_SE","Height_DT","Height_CREMP"))

Height_SE <- read.table("~/Postdocing_NSU/Octocoral forests/Manuscript/raw_data/SE_OCTO_Raw_Data.txt", header = TRUE, sep = ",", quote = "\"", na.strings = "")
Height_DT<-  read.table("~/Postdocing_NSU/Octocoral forests/Manuscript/raw_data/DT_OCTO_Raw_Data.txt", header = TRUE, sep = ",", quote = "\"", na.strings = "")
Height_CREMP <-  read.table("~/Postdocing_NSU/Octocoral forests/Manuscript/raw_data/FLK_OCTO_Raw_Data.txt", header = TRUE, sep = ",", quote = "\"", na.strings = "")

colnames(Height_SE)[colnames(Height_SE) == "Site_name"] <- "SiteName"
# bind height data 
Height_all <- rbind(Height_SE, Height_CREMP, Height_DT) 

head(Height_all,10)

# Count the number of unique SPP_Code
unique_spp_count <- Height_CREMP %>% 
  distinct(SPP_Code) 
unique_spp_count
#remove data on E. calyculata
Height_all <- filter(Height_all, SPP_Code != "ECAL" & SPP_Code != "PBIP/PKAL")

# Count the number of unique SPP_Code
unique_spp_count <- Height_all %>% 
  distinct(SPP_Code) 
unique_spp_count
# Summary of colony heights for each species.  keeping the Transect/station level
#view(Height_all)

head(filter(Height_all, SPP_Code == "NS-PAME"), 20)

colnames(Height_all)
Sum_height_all <- Height_all %>%
  # filter(Year != 2011) %>% 
  # filter(SPP_Code %in% c("PFLE","GVEN","PAME"))  %>%
  group_by(Year, SiteID, StationID, sciName) %>% 
  mutate(mean_height = mean(Height.cm.), 
         sd = sd(Height.cm.), 
         biomass = sum(Height.cm.), 
         n = n()) %>% 
  distinct() %>% 
  mutate(sciName = dplyr::recode(sciName,
                                 'Pseudopterogorgia americana' = 'Antillogorgia_americana',
                                 "Eunicea flexuosa" = "Eunicea_flexuosa", 
                                 "Gorgonia ventalina" = "Gorgonia_ventalina", 
                                 "Pseudoplexaura porosa" = "Pseudoplexaura porosa", 
                                 "Pseudopterogorgia bipinnata" = 'Antillogorgia_bipinnata', 
                                 "Not Slimy - P. americana" = 'Antillogorgia_americana')) %>% 
  ungroup()



# Identify the indices of the columns to be removed
cols_to_remove <- which(colnames(Sum_height_all) %in% c("Height.cm.", "SPP_Code"))

# Remove the identified columns
Sum_height_all <- Sum_height_all[, -cols_to_remove]

view(Sum_height_all)
Sum_height_all %>% 
  distinct(sciName) 
colnames(Sum_height_all)
# bind density data 
#ALL DATA

density_DT<-  read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/DTCREMP_CSVs/DTCREMP_OCTO_Summaries_2011-2023_Density.csv" ) 

density_SE <-  read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/SECREMP_CSV_Files/SECREMP_OCTO_Summaries_2012-2023_Density.csv" ) 
density_CREMP <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/CREMP_CSV_Files/CREMP_OCTO_Summaries_2011-2023_Density.csv" ) 


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
colnames(Height_all)
colnames(density_long_dat)
colnames(Sum_height_all)[colnames(Sum_height_all) == "SiteName"] <- "Site_name"

combined <- left_join(density_long_dat, Sum_height_all,by = c("Year", "Subregion" ,"Habitat" ,  "SiteID" ,   "Site_name" ,"StationID","sciName" ) )
combined <- combined %>% #filter problematic data points
  filter(!(Site_name %in% omit_sites)) %>%  # omiting irrelevant sites !
  filter(!(Subregion == "UK" & Year == 2011))%>% 
  select(-Date.y)
head(combined, 20)

# Add SPP_Code column based on conditions
combined <- combined %>%
  mutate(
    SPP_Code = case_when(
      sciName == 'Gorgonia_ventalina' ~ "GVEN",
      sciName == "Eunicea_flexuosa" ~ "EFLE",
      sciName == "Antillogorgia_americana" ~ "AAME",
      sciName == "Antillogorgia_bipinnata"  ~ "ABIP",
      sciName == "Pseudoplexaura_porosa"  ~ "PPOR",
      sciName == "Total_Octocorals"  ~ "TOCT"
    )
  )  %>%
  select(Year, Date.x, Subregion, Habitat, SiteID, Site_name, StationID, SPP_Code, sciName, everything())

# Display the first 20 r

# Display the first 20 rows of the updated data frame
head(combined, 20)


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
 #view(combined_with_metadata)
 head(combined_with_metadata)
 
 # Create a new column to indicate if there is a match between Site_name and sitename (1 if match, 0 if not)
 combined_with_metadata$sitename_match <- ifelse(combined_with_metadata$Site_name == combined_with_metadata$sitename, 1, 0)
 # Summarize the counts of 0s and 1s for sitename matches
 sitename_summary <- table(combined_with_metadata$sitename_match)
 sitename_summary
 
 colnames(combined_with_metadata)
 
 # Creating a list of sites that should be omitted from the datasets 
# omit_sites <- c("Martin County 1" ,"Martin County 2", "Martin County 3", "BCA")
 
 # Create a new dataset "all dat" with the required modifications and excluding the specified sites
 all_dat <- combined_with_metadata %>%
  # filter(!(Site_name %in% omit_sites)) %>%
   select(-sitename, -habitatid, -length, -sitename_match) %>% 
   rename(Depth = offshoreDepth)
 
 view(all_dat)
 # Export the updated dataset to a CSV file - Last updated in 10/5/24
 
 write.csv(all_dat, file = "~/Postdocing_NSU/Octocoral forests/Manuscript/all_density_and_heights_by_stationID.csv", row.names = FALSE)
 
# next work is the modelling and/ or visualizations. 
 
 
 