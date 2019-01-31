##  Query OSM in England and scotland
##  Querying openstreet map to find city and town centres as well as landmarks
##  Implementation requires the osmdata package to query osm
#options(repos = c(CRAN = "https://cran.revolutionanalytics.com")) # code to use different repository than default
##  We have imprved the old SLS and rci paper routine -- should be better

pkgs <- c('osmdata', 'tmap', 'sf', 'tidyverse', 'RANN')
lapply(pkgs, require, character.only = T)
ukgrid = "+init=epsg:27700" ## always remember the CRS


# Section 1: Querying osm and saving the data (can skip if query done) -----------------------------
##  Need England and Scotland at least
##  We will get an overpass error if we search and dl too much too soon so 
##  piece meal dl for us

##  England is too big so solution is to cut the bound box in half
bb_eng <- 
  getbb('england', featuretype = 'country') ## works just search for country; this is full extent checked
half_eng <- (bb_eng[, 2] - bb_eng[, 1]) / 2
bb_eng_1 <- bb_eng + matrix(cbind(c(0, 0), - half_eng), nrow = 2)
bb_eng_2 <- bb_eng + matrix(cbind(half_eng, c(0, 0)), nrow = 2)

bb_scot <- 
  getbb('scotland', featuretype = 'country') ## works just search for country; this is full extent checked

bb_list <- list(bb_eng_1, bb_eng_2, bb_scot) # into one big list

##  Since the routine from previous attempts is so well routine we have made
##  it into a function
## Sometimes we get overpass errors; assuming that we are using latest version of
##  osmdata (check because MRAN has older pkgs) then this is caused by server failure
##  esp. if we are restricted due to our large data request

##  Let's just go and do this as a function; default we use value to name
extract.sf.osm <- function(bbox, value, key = 'place', crs = ukgrid){
  out <- opq(bbox = bbox) %>% 
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf

  out <- out$osm_points %>% 
    mutate(type = value) %>%
    select.sf(osm_id, name, type) %>%
    st_transform(crs = crs)
  
  print('Waiting 10 seconds before returning')
  Sys.sleep(10) # R waits for 5 min before returning results -- reason is to not clog 
  # up the overpass server
  
  return(out)
}

#test <- extract.sf.osm(bb_eng_1, value = 'city')

##  Now to run the code for list
cities_sf <- lapply(bb_list, extract.sf.osm, value = 'city')
towns_sf <- lapply(bb_list, extract.sf.osm, value = 'town')
townhalls_sf <- lapply(bb_list, extract.sf.osm, value = 'townhall', key = 'amenity')


##  For some reason do.call doesn't like osmdata in list so.. we do it the long way

landmarks_sf <- do.call(rbind, c(cities_sf, towns_sf, townhalls_sf)) ## we combine lists with c()
landmarks_sf %>% object.size #17mb # sma;; bc we saved just few details
landmarks_sf %>% str
##  save but temp
landmarks_sf %>% saveRDS('Saved generated data/Saved Centre landmarks from osmdata.RDS')

# Section 2: Filtering and finding locality centre points -----------------
landmarks_sf <- 
  'Saved generated data/Saved Centre landmarks from osmdata.RDS' %>% 
  readRDS
landmarks_sf <- landmarks_sf %>% filter(!is.na(name) | type == 'townhall') #filtering to verified with name or is a townhall

##  We have to basically get all the bua and bua subdivisions going
all.weighted_bua <-
  read.csv('Saved generated data/bua and buasd11 pop weighted centroids and other data.csv')

loc.weighted <-
  read.csv('Saved generated data/locality pop weighted centroids and other data.csv')
loc.weighted %>% head

all.weighted <- bind_rows(all.weighted_bua, loc.weighted) ## binds even when diff cols
all.weighted %>% head

# 3) Loading in the bua, buasd and localities bondary files ---------------------------

bua.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA 2011', sep = ''),
  layer = 'Builtup_Areas_December_2011_Boundaries_V2') %>%
  st_transform(crs = st_crs(ukgrid))

bua.sf <- bua.sf %>% 
  filter(has_sd == 'N' & urban_bua == 'Yes') %>%
  mutate(bua_name = bua11nm, 
         bua_type = 'bua') %>%
  select.sf(bua_name, bua_type) #keep those without subdivision and is urban
###
buasd.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA subdivision 2011', sep = ''),
  layer = 'Builtup_Area_Sub_Divisions_December_2011_Boundaries') %>%
  st_transform(crs = st_crs(ukgrid))

buasd.sf <- buasd.sf %>% 
  mutate(bua_name = buasd11nm, 
         bua_type = 'buasd') %>%
  select.sf(bua_name, bua_type) #keep those without subdivision and is urban

##
loc.sf <- st_read(dsn = google.drive %>% 
                    paste('/Spatial lookup files/UK/Scottish settlements', sep = ''), 
                  layer = 'Localities2012') %>%
  st_transform(crs = st_crs(ukgrid))

loc.sf <- loc.sf %>%
  mutate(bua_name = L12NAME,
         bua_type = 'locality') %>%
  select.sf(bua_name, bua_type)

all_bua.sf <- bua.sf %>% rbind(buasd.sf) %>% rbind(loc.sf)


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
landmarks_csv


centres.imputed %>% tail
centres.imputed <-  all.weighted %>%
  mutate(imputed_easting = NA, imputed_northing = NA, imputed_type = NA, imputed_error = NA,
         imputed_osm_nm = NA)

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
  centres.imputed$imputed_osm_nm[i] <- filtered.lnd$name
  ##  And done
}

##  Now we run and wait
warnings()# right warnings due to missing osm name -- which can occur in scotland since no restrict
centres.imputed %>% tail


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
centres.imputed %>% write.csv('Saved generated data/Imputed centres based on osmdata for England and Scotland.csv')


