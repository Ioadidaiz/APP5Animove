# white storks stopping sites during migration & analysing their spectral characteristics

# research question: why do storks migrate to spain and not to africa anymorer in the last years?

library(move)
# logging in 
login <- movebankLogin(user= "SvenjaDobe", password = getPass::getPass())


# getting study meta data: 

study.west <- searchMovebankStudies("LifeTrack White Stork SW Germany", login = login)


storks.meta.W <- getMovebankStudy(study.west[1],login = login)
storks.indis.W <-  getMovebankAnimals(study.west[1], login= login)

# download study data: 
## all individuals: moveStack
#storks.data <- getMovebankData(study.west[1],login = login)
#storks.data.ul <- unlist(storks.data)
#plot(study.data.ul)


## specific individual: yolo

yolo <- getMovebankData(study.west[1],animalName = c(storks.indis.W[storks.indis.W$tag_local_identifier == 2666, "local_identifier"],storks.indis.W[storks.indis.W$tag_local_identifier == 2561, "local_identifier"], storks.indis.W[storks.indis.W$tag_local_identifier == 3031, "local_identifier"], (storks.indis.W[storks.indis.W$tag_local_identifier == 4001, "local_identifier"][4])),login = login, removeDuplicatedTimestamps=TRUE)

plot(yolo)

# convert to data frame with sf

library(sf)
yolo.df <- as.data.frame(yolo)
yolo.sf <- st_as_sf(yolo.df,coords = c("location_lat", "location_long"), crs= st_crs(yolo))

library(ggplot2)
ggplot(yolo.sf)+
  geom_sf()

#mapview::mapview(yolo.sf)


# coords, timestamps and lags:
yolo.coords <-  coordinates(yolo)
yolo.times <- timestamps(yolo)
yolo.lags <- timeLag(yolo, units="mins")

#

library(circular)
yolo.speed <- speed(yolo) # m/s
yolo.speed.na <- append(NA,yolo.speed)

yolo.heading <- angle(yolo)
yolo.heading.na <- append(NA,yolo.heading)

yolo.angle <- turnAngleGc(yolo)
yolo.angle.na <- append(NA,yolo.angle)
yolo.angle.na <- append(yolo.angle.na,NA)

yolo.azimuth <- as.circular(yolo)
yolo.azimuth.na <- append(NA,yolo.azimuth)

plot(yolo.speed,yolo.heading)
windrose(yolo.speed,yolo.heading)
plot(yolo.times,yolo.heading.na)

plot(yolo.speed.na,yolo.angle.na)
windrose(yolo.angle.na,yolo.speed.na)

plot(yolo.speed.na,yolo.azimuth)

### 1. fancy moveVIS plot:

library(moveVis)
library(raster)



move_data <- align_move(yolo, res = 1, unit = "days")
plot(move_data)

frames <- frames_spatial(move_data, path_colours = c("red","green","blue","orange"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5)

length(frames) # number of frames
frames[[100]] 




#You may recognize that the map seems to be distorted which becomes mainly visible when looking at the map labels. By default, moveVis calculates an equidistant map extent (squared), with y and x axis measuring equal surface distances. In addition, moveVis reprojects the default base maps to the projection of your movement data. The default base maps are originally projected as WGS 84 / Pseudo Mercator (epsg:3857), also referred to as Web Mercator, and thus are squeezed or stretched onto the projection grid of the movement data (in this case WGS 84 / LatLon).

#To represent the base map in its original projection, just reproject your movement data to the WGS 84 / Pseudo Mercator projection and disable the calculation of an equidistant extent:
  
  move_data <- sp::spTransform(move_data, crs("+init=epsg:3857"))
frames <- frames_spatial(move_data, path_colours = c("red","green","blue","orange"),
                         map_service = "osm", map_type = "streets", map_res = 0.8, equidistant = F)
frames[[100]] # display one of the frames

#Finally, animate the newly created frames:
  
animate_frames(frames, out_file = "example_1b.gif", width = 700, height = 500, res = 80)


