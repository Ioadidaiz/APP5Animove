library(move)
# logging in 
login <- movebankLogin(user= "SvenjaDobe", password = getPass::getPass())


# getting study meta data: 

study.name <- searchMovebankStudies("Kruger African Buffalo, GPS tracking, South Africa", login = login)
study.meta <- getMovebankStudy(study.name[1],login = login)
study.indis <-  getMovebankAnimals(study.name, login= login)

# download study data: 
## all individuals: moveStack
study_data <- getMovebankData(study.name,login = login)

## specific individual: move

study_data_indi <- getMovebankData(study.name,animalName = as.character(study.indis$local_identifier[4]),login = login)


# as spatial objects

study_data_coords <-  coordinates(study_data)
study_data_times <- timestamps(study_data)
study_data_lags <- timeLag(study_data, units="mins")
par(mfrow=c(4,2))
hist(study_data_lags$Cilla) # 60 mins
hist(study_data_lags$Gabs)
hist(study_data_lags$Mvubu)
hist(study_data_lags$Pepper)
hist(study_data_lags$Queen)
hist(study_data_lags$Toni)

study_ids <- idData(study_data)

# or convert to data frame with sf

library(sf)
study_data_df <- as.data.frame(study_data)
study_data_sf <- st_as_sf(study_data_df,coords = c("location_lat", "location_long"), crs= st_crs(study_data))


# plotting data

library(ggplot2)
ggplot(study_data_sf)+
  geom_sf()
  geom_map(data = map_data("world"), map= map_data("world"), aes(long,lat,map_id = region))+
 


library(tidyverse)


# calculate distance and speed 
study_data_speed <- speed(study_data) # m/s
study_data_dist <- distance(study_data)

plot(study_data_speed$Cilla)
plot(study_data_speed$Gabs)
plot(study_data_speed$Mvubu)
plot(study_data_speed$Pepper)
plot(study_data_speed$Queen)
plot(study_data_speed$Toni)

plot(study_data_lags$Cilla,study_data_speed$Cilla)
plot(study_data_lags$Gabs,study_data_speed$Gabs)
plot(study_data_lags$Mvubu,study_data_speed$Mvubu)
plot(study_data_lags$Pepper,study_data_speed$Pepper)
plot(study_data_lags$Queen,study_data_speed$Queen)
plot(study_data_lags$Toni,study_data_speed$Toni)

plot(study_data_dist$Cilla,study_data_speed$Cilla)

  
  # mapview 
  
mapview::mapview(study_data_sf)

# how many positions for which our of the day? 

study_data_times
  
####################################################### BATS####################

study.name2 <- searchMovebankStudies("Parti-colored bat Safi Switzerland", login = login)
study.meta2 <- getMovebankStudy(study.name2[1],login = login)
study.indis2 <-  getMovebankAnimals(study.name2, login= login)

# download study data: 
## all individuals: moveStack
study_data2 <- getMovebankData(study.name2,login = login)

## specific individual: move

study_data_indi2 <- getMovebankData(study.name2,animalName = as.character(study.indis$local_identifier[4]),login = login)


# as spatial objects

study_data_coords2 <-  coordinates(study_data2)
study_data_times2 <- timestamps(study_data2)
study_data_lags2 <- timeLag(study_data2, units="mins")
par(mfrow=c(4,2))
hist(study_data_lags[1]) # 60 mins
hist(study_data_lags[2])
hist(study_data_lags[3])
hist(study_data_lags[4])
hist(study_data_lags[5])
hist(study_data_lags[6])

study_ids <- idData(study_data2)

# or convert to data frame with sf

library(sf)
study_data_d2 <- as.data.frame(study_data2)
study_data_sf2 <- st_as_sf(study_data_df2,coords = c("location_lat", "location_long"), crs= st_crs(study_data2))


# plotting data

library(ggplot2)
ggplot(study_data_sf2)+
  geom_sf()
geom_map(data = map_data("world"), map= map_data("world"), aes(long,lat,map_id = region))+
  
  
  
  library(tidyverse)


# calculate distance and speed 
study_data_speed2 <- speed(study_data2) # m/s
study_data_dis2 <- distance(study_data2)

plot(study_data_speed2)
plot(study_data_speed[2])
plot(study_data_speed[3])
plot(study_data_speed[4])
plot(study_data_speed[5])
plot(study_data_speed[6])

plot(study_data_lags$Cilla,study_data_speed$Cilla)
plot(study_data_lags$Gabs,study_data_speed$Gabs)
plot(study_data_lags$Mvubu,study_data_speed$Mvubu)
plot(study_data_lags$Pepper,study_data_speed$Pepper)
plot(study_data_lags$Queen,study_data_speed$Queen)
plot(study_data_lags$Toni,study_data_speed$Toni)

plot(study_data_dist$Cilla,study_data_speed$Cilla)


# mapview 

mapview::mapview(study_data_sf)

# how many positions for which our of the day? 

study_data_times

