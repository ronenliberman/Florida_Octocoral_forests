# All octocoral dataset 

# Can i do depth ranks? 
#How does the dat disturbed by depth ? 

dat <- read.csv("~/Postdocing_NSU/Octocoral forests/Manuscript/all_density_and_heights_by_stationID.csv")
head(dat)

# Here i test what should be the rangesof the data depth-groups based on number of rows 
# Filter the data for SPP_CODE == "TOCT"
TOCT_data <- dat %>% 
  filter(SPP_Code == "TOCT")

# Convert Depth to numeric (if not already done)
TOCT_data$Depth <- as.numeric(as.character(TOCT_data$Depth))

# Plot the distribution of data by Depth
ggplot(TOCT_data, aes(x = Depth)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Data by Depth for SPP_CODE - TOCT", x = "Depth", y = "Number of Rows") 


# Calculate quartiles for the Depth variable
quartiles <- TOCT_data %>%
  summarise(Q1 = quantile(Depth, 0.25),
            Median = quantile(Depth, 0.5),
            Q3 = quantile(Depth, 0.75))

# Create a frequency table of rows per depth
depth_frequency <- TOCT_data %>%
  group_by(Depth_group = cut(Depth, breaks = c(-Inf, quartiles$Q1, quartiles$Median, quartiles$Q3, Inf))) %>%
  summarise(Frequency = n())

# Rename the Depth_group variable for clarity
depth_frequency$Depth_group <- gsub("\\(|\\]", "", as.character(depth_frequency$Depth_group))

# Combine quartiles and frequency into a summary table
summary_table <- depth_frequency %>%
  select(Quartile = Depth_group, Frequency)

### so it seems that if we make 4 groups we need to have it as <21 , 21-28 , 28-45 and above 45 , all in Feet of course.. 
# lets visualize it 

# Define the depth range groups based on quartiles or custom breaks
dat_3groups <- dat %>% 
  mutate(Depth_group = case_when(
    Depth < 20 ~ "Shallow",
    Depth >= 20 & Depth < 45 ~ "Mid", 
    Depth >= 45 ~ "Deep"
  ))

#dat_3groups %>% filter(SPP_Code == "TOCT") %>%  ggplot(aes(x = Depth_group, fill = Depth_group)) +
 # geom_bar() +
  # labs(title = "Frequency of Observations in Each Depth Group", x = "Depth Group", y = "Frequency") 


dat_4groups <- dat %>% 
  mutate(Depth_group = case_when(
    Depth < 21 ~ "Shallow",
    Depth >= 21 & Depth < 28 ~ "Mid-shallow", Depth >= 28 & Depth < 45  ~ "Mid-deep",
    Depth >= 45 ~ "Deep"
  ))

dat_4groups %>% filter(SPP_Code == "TOCT") %>%  ggplot(aes(x = Depth_group, fill = Depth_group)) +
  geom_bar() +
  labs(title = "Frequency of Observations in Each Depth Group", x = "Depth Group", y = "Frequency") 


# ok , this makes some sense. 

head(dat)

#Looking at density 
Total.octo <- dat_4groups %>% filter(SPP_Code == "TOCT") 
# Count the number of unique ProjectRegion 
Total.octo %>% 
  distinct(ProjectRegion ) 

Total.octo <- Total.octo %>%
  mutate(ProjectRegion = ifelse(ProjectRegion == "NPSDT/FKNMS", "NPSDT", ProjectRegion))

Total.octo %>% 
  distinct(ProjectRegion ) 

# model 

#install.packages("glmmTMB")
library(glmmTMB)
library(lme4)

head(Total.octo)

#look for zero density values
#Looking at density 
Total.zero <- Total.octo %>% filter(density == "0")
view(Total.zero)

#check sites to consider removing 

Total.octo %>% filter(Site_name == "Broward County A")  # dont omit 
Total.octo %>% filter(Site_name == "Palm Beach 1")  # should be omitted I think
Total.octo %>% filter(Site_name == "Davis Rock")  # should be omitted I think
Total.octo %>% filter(Site_name == "Texas Rock")  # should not omitted I think

# Creating a list of sites that should be omitted from the datasets 
omit_sites <- c("Davis Rock","Palm Beach 1") 

 # Remove rows with NA values in density
to.model <- na.omit(Total.octo)
head(to.model)
# Convert SiteID and StationID to factors in the Total.octo dataset
Total.octo <- Total.octo %>%
  filter(!(Site_name %in% omit_sites)) %>% 
  mutate(SiteID = as.factor(SiteID),
         StationID = as.factor(StationID))

head(Total.octo)
glmm_model_depthgroup_interaction <- glmmTMB(density ~ Year * Depth_group + (1|SiteID/StationID),
                      family = poisson, 
                      data = Total.octo)

glmm_model_depthgroup <- glmmTMB(density ~ factor(Year) + Depth_group + (1|SiteID/StationID),
                                 family = poisson, 
                                 data = Total.octo)

glmm_model_depthgroup_year.as.integer <- glmmTMB(density ~ Year + Depth_group + (1|SiteID/StationID),
                                 family = poisson, 
                                 data = Total.octo)
glmm_model_Subregion <- glmmTMB(density ~ factor(Year) + Subregion+(1|SiteID/StationID),
                           family = poisson, 
                           data = Total.octo)
glmm_model_ProjectRegion <- glmmTMB(density ~ factor(Year) + ProjectRegion+(1|SiteID/StationID),
                                family = poisson, 
                                data = Total.octo)

glmm_model_Year <- glmmTMB(density ~ factor(Year) + (1|SiteID/StationID),
                           family = poisson, 
                           data = Total.octo)
glmm_model_Site <- glmmTMB(density ~ factor(Year) + SiteID+(1|StationID),
                           family = poisson, 
                           data = Total.octo)


glmm_model_null <- glmmTMB(density ~  + 1,
                           family = poisson, 
                           data = Total.octo)

AIC(glmm_model_depthgroup,glmm_model_depthgroup_year.as.integer,glmm_model_depthgroup_interaction, glmm_model_Subregion,glmm_model_ProjectRegion,glmm_model_Year,glmm_model_Site,glmm_model_null)

glmm_model_ProjectRegion 
summary(glmm_model_depthgroup_interaction)

view(Total.octo)
