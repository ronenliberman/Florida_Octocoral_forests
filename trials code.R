Height_data <- read.csv("~/Postdocing_NSU/Octocoral forests/Monitoring data/Me playing with monitoring data/SECREMP_OCTO_RawData_2013.csv")
Height_data$Date <- format(as.Date(Height_data$Date,format = "%m/%d/%Y"))

Sum_height <- Height_data %>%
  group_by(SiteID,StationID, SPP_Code) %>%

  summarise(mean_size = mean(Height_cm , na.rm = TRUE),num_obs = sum(!is.na(Height_cm )),.groups = 'keep') 
  

head(Sum_height)
view(Sum_height)



try_s<- Height_data %>% 
  filter(SPP_Code %in% c("PFLE","GVEN","PAME"))  %>%
  group_by(SiteID,StationID, SPP_Code) %>% 
  mutate(mean_height = mean(Height_cm), sd= sd(Height_cm) , n= n()) %>% 
  select(-Height_cm) %>% 
  distinct() %>% 
  ungroup()



view(try_s)
try <-try_s %>%
  select(-Height_cm) %>% 
  distinct()
view(try)
try_1 <- try %>% 
  distinct()

try2 <- Height_data %>% 
  group_by(SiteID,StationID, SPP_Code)
View(try2)

try3 <-   try2 %>%
    reframe(mean_height = mean(Height_cm), across())
View(try3)
