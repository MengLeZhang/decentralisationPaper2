##  Querying openstreet map to find city and town centres as well as landmarks
##  Implementation requires the osmdata package to query osm
#options(repos = c(CRAN = "https://cran.revolutionanalytics.com")) # code to use different repository than default

pkgs <- c('osmdata', 'tmap', 'sf', 'tidyverse', 'RANN')
lapply(pkgs, require, character.only = T)
ukgrid = "+init=epsg:27700" ## always remember the CRS


# Section 1: Querying osm and saving the data (can skip if query done) -----------------------------
eng_bb <- getbb('england, uk') # Extent of England
wales_bb <- getbb('wales') # Extent of Wales
eng_bb; wales_bb ## actually england has the max and min
bb <- eng_bb

city_sf <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'place', value = 'city') %>%
  osmdata_sf
city_sf$osm_points$type <- 'city'

town_sf <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'place', value = 'town') %>%
  osmdata_sf
town_sf$osm_points$type <- 'town'

townhall_sf <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'amenity', value = 'townhall') %>%
  osmdata_sf
townhall_sf$osm_points$type <- 'townhall'

# station_sf <- opq(bbox = bb) %>% 
#   add_osm_feature(key = 'public_transport', value = 'station') %>%
#   osmdata_sf
# station_sf$osm_points$type <- 'station'

landmarks_sf <- c(city_sf, 
                  town_sf, 
                  #station_sf, 
                  townhall_sf)
##  Okay.. I reckon we only need to get  points

landmarks_sf <- landmarks_sf$osm_points %>% st_transform(ukgrid)
landmarks_sf %>% object.size ##almost 250mb

landmarks_sf %>% saveRDS('Working analysis files/Large files to ignore/Saved Centre landmarks from osmdata.RDS')

# Section 2: Filtering and finding locality centre points -----------------
landmarks_sf <- readRDS('Working analysis files/Large files to ignore/Saved Centre landmarks from osmdata.RDS')
landmarks_sf <- landmarks_sf %>% filter(!is.na(name) | type == 'townhall') #filtering to verified with name or is a townhall

##  We have to basically get all the bua and bua subdivisions going
all.weighted <-
  read.csv('Working analysis files/bua and buasd11 pop weighted centroids and other data.csv')


# 3) Loading in the bua and buasd bondary files ---------------------------

bua.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA 2011', sep = ''),
  layer = 'Builtup_Areas_December_2011_Boundaries_V2') %>%
  st_transform(crs = st_crs(ukgrid))

bua.sf <- bua.sf %>% 
  filter(has_sd == 'N' & urban_bua == 'Yes') %>%
  mutate(bua_name = bua11nm, 
         bua_type = 'bua') %>%
  dplyr::select(bua_name, bua_type, geometry) #keep those without subdivision and is urban
###
buasd.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA subdivision 2011', sep = ''),
  layer = 'Builtup_Area_Sub_Divisions_December_2011_Boundaries') %>%
  st_transform(crs = st_crs(ukgrid))

buasd.sf <- buasd.sf %>% 
  mutate(bua_name = buasd11nm, 
         bua_type = 'buasd') %>%
  dplyr::select(bua_name, bua_type, geometry) #keep those without subdivision and is urban

all_bua.sf <- bua.sf %>% rbind(buasd.sf)


# 4) Running the for loop to get the centres ------------------------------

##  So let me reiterate how this all work: 
##  1) we restrict the landmarks to those within the bua/ buasd of interest
##  2) we use the pop weighted centroid as a starting point (referred to as origin) and select the nearest landmark as it's centre
##  3) Landmarks are prioritised as city > town > townhall
##  4) if no landmarks exist we simply use the pop weighted centroid
##  5) (diag) we produce dist error stats

##  First work by joining landmarks to their appropriate bua/buas
landmarks_csv <- landmarks_sf %>% st_join(all_bua.sf)
landmarks_csv <- landmarks_csv %>% cbind(st_coordinates(landmarks_sf))
st_geometry(landmarks_csv) <- NULL


centres.imputed <-  all.weighted %>%
  mutate(imputed_easting = NA, imputed_northing = NA, imputed_type = NA, imputed_error = NA,
         imputed_osmnm = NA)

# Start of for loop -------------------------------------------------------

for (i in 1:nrow(centres.imputed)){

  
  ##  Subtset the data to locality
  ## The code or name
  temp.nm <- centres.imputed$name[i]
  temp.nm %>% print
  
  temp.lnd <- landmarks_csv %>% filter(bua_name == temp.nm)
  temp.origin <- all.weighted[i, ]
  
  ##  Here we start with asking whether we have found any landmarks and if so 
  ##  which one
  ##  Default to none -- under which.landmark -- then we search until one is found
  
  type.vec <- temp.lnd$type
  which.landmark <- 'None'
  if ('city' %in% type.vec){
    which.landmark <- 'city'
  } else if('town' %in% type.vec){
    which.landmark <- 'town'
  } else if('townhall' %in% type.vec){
    which.landmark <- 'townhall'
  } 
  
  
  # (for loop) calculation of distance from origin ----------------------
  ## say what we are imputing
  print('Landmarks based on' %>% paste(which.landmark))
  
  ## If no landmarks then fill in data from origin and next loop
  if(which.landmark == 'None'){
    centres.imputed$imputed_easting[i] = centres.imputed$pweight_x[i]
    centres.imputed$imputed_northing[i] = centres.imputed$pweight_y[i]
    centres.imputed$imputed_type[i] = 'Origin'
    centres.imputed$imputed_error[i] <- NA
    next
  } 
  
  ##  If landmarks then create the data
  filtered.lnd <- temp.lnd %>% 
    filter(type == which.landmark)
  
  ##  First calculate the distance
  origin <- all.weighted[i, ] %>% 
    dplyr::select(pweight_x, pweight_y) %>%
    as.matrix

  dest <- filtered.lnd %>% 
    dplyr::select(X, Y) %>% 
    as.matrix
  
  # Distance matrix -- we keep the first col which tell us from the dist from origin
  
  origin_to_dest <- nn2(data = dest, query = origin, k = 1)
  min_id <- origin_to_dest$nn.idx %>% c 
  
  centres.imputed$imputed_easting[i] = filtered.lnd$X[min_id]
  centres.imputed$imputed_northing[i] = filtered.lnd$Y[min_id]
  centres.imputed$imputed_type[i] = which.landmark
  centres.imputed$imputed_error[i] <- origin_to_dest$nn.dists %>% c
  centres.imputed$imputed_osmm[i] <- filtered.lnd$name
  ##  And done
}

##  Now we run and wait

# Final step: Check things and save results -------------------------------
##  Lots of origins
centres.imputed <- centres.imputed %>% filter(!is.na(name))  ## there are na for name non included
centres.imputed$imputed_type %>% table
centres.imputed %>% summary # less than 1km error most of the time but.. wow

centres.imputed %>% 
  group_by(imputed_type) %>%
  summarise(min(pop11),
            max(pop11),
            median(pop11))
##  right so origins only occur for very small places really mostly
centres.imputed %>% write.csv('Working analysis files/Imputed centres based on osmdata.csv')


### Checkign summary stats
centres.imputed <-
  read.csv('Working analysis files/Imputed centres based on osmdata.csv')

centres.imputed %>% 
  mutate(osm_type = as.factor(imputed_type)) %>%
  filter(pop11 > 10000) %>%
  summary
