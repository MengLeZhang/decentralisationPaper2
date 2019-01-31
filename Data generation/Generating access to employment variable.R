##  Generating access to employment variable
##  So this is the same method for England as our rci paper
##  Except this time we are not really constrained to formatting everything to 
##  lsoa01 and we have made things more routine
##  So that is the big difference between our two attempts.

source('UI paper 1 source.R') 
ukgrid = "+init=epsg:27700" ## always remember the CRS
library(RANN) ## additional 

##  Step 1: Since it is so reptitive I've made a function to calc the access measure ----

##  So the generic code is to take in the data set we want the measure at
##  df.x (i.e. lsoa) and the work data (df.x). Find all centres within a distance
##  default (15km). Then use a function to use the distance and another variable
##  to work out the computed employment measure. Let's function this up
##  Both df.x and df.y must be formatted as to have 
##  3 variables:: 1) zone, 2) x, 3) y, 
##  df.y must have a variable for wk.pop as well; then we output our desired zone
##  and data


## It measures the total jobs within 15km by default and decay function which is
##  governed by a distance weighted decay parameter; see paper and hansen
access.measure <- function(df.x, df.y, bounds = 15000, decay = 1){
  
  ##  Function Step 1: Get the coords matrix for df.x ----
  ##  The coordinates
  x.coords <- df.x %>% dplyr::select(x, y) %>% as.matrix
  y.coords <- df.y %>% dplyr::select(x, y) %>% as.matrix
  
  ##  We can use search a little bit at a time?; nearest 4k neighbours ought to be enough
  
  'Finding distances' %>% print
  nn.list <- nn2(query = x.coords,
                 data = y.coords,
                 k = 10000,
                 searchtype = 'radius',
                 radius = bounds)
  #huge file

  str(nn.list)
  ##  So now we have a huge matrix of nearest neighbours and indicies; each row contains the nearest 10k data for
  ##  each lsoa
  
  ##  Function step 2: This is how we use the nearest neighbour matrix to calc access ----
  access.out <- df.x %>% dplyr::select(zone) %>%
    mutate(accessA = NA, accessB = NA)
  ##  Add sum of 15km and inverse distance weighting (0.1 + dist)

  'Distances done; calculating access' %>% print
  ##  For loop for calcualte the stats
  for (i in 1:nrow(access.out)){
    dist_vec <- nn.list$nn.dists[i, ] / 1000
    dist_vec <- dist_vec[dist_vec <= 15] 
    
    count <- df.y$wk.pop[nn.list$nn.idx[i, ]]

    access.out$accessA[i] <- count %>% sum

    access.out$accessB[i] <- t(1 / (exp(decay * (0.1 + dist_vec)))) %*% count
  }
  
  ##  Now it's done retrn the data
  return(access.out)
}

##  Step 2: Now to calculate the access for each year etc that we are interested ----

##  Scotland 2001 -----------
##  A) datazone (df.x)
dz01.sf <- 
  google.drive.spatial %>% paste0('/Scottish datazones/2001') %>%
  read_sf(layer = 'SG_DataZone_Cent_2001')

st_geometry(dz01.sf) <- NULL 

dz01.sf <- dz01.sf %>%
  mutate(zone = DZ_CODE,
         x = EASTING,
         y = NORTHING)

##  B) work (df.y)
##  Centroids first
oa.sf <- 
  read_sf(dsn = google.drive.spatial %>% paste0('/OA 2001/Scotland'),
          layer = 'OutputArea2001_HWC')

st_geometry(oa.sf) <- NULL

oa.sf <- oa.sf %>%
  mutate(zone = NRSoldOutp,
         x = Easting,
         y = Northing) ## the old oa name

##  Attach work data
wkp.df <- google.drive.spatial %>% 
  paste0('/OA 2001/Scotland/scot workplace oa.csv') %>%
  read.csv(na.string = '-')

wkp.df <- 
  wkp.df %>%
  mutate(wk.pop = Total %>% replace_na(0),
         zone = oa)

##  merge
oa.sf <- 
  oa.sf %>%
  merge(wkp.df)

##  C) Now to calculate and save
dz01.access <- access.measure(df.x = dz01.sf, df.y = oa.sf)
dz01.access %>% write.csv('Saved generated data/emp access dz01 lkp.csv')



##  Scotland 2011 -----------
##  A) datazone (df.x)
dz11.sf <- 
  google.drive.spatial %>% paste0('/Scottish datazones/2011') %>%
  read_sf(layer = 'SG_DataZone_Cent_2011')

st_geometry(dz11.sf) <- NULL 

dz11.sf <- dz11.sf %>%
  mutate(zone = DataZone,
         x = Easting,
         y = Northing)

##  B) work (df.y)
##  Centroids first
oa.sf <- 
  read_sf(dsn = google.drive.spatial %>% paste0('/OA 2011/Scotland'),
          layer = 'OutputArea2011_PWC')

st_geometry(oa.sf) <- NULL
oa.sf %>% head

oa.sf <- oa.sf %>%
  mutate(zone = code,
         x = easting,
         y = northing) ## the old oa name

##  Attach work data
wkp.df <- google.drive.spatial %>% 
  paste0('/OA 2011/Scotland/workplace pop oa 2011 scotland.csv') %>%
  read.csv(na.string = '-')

wkp.df <- 
  wkp.df %>%
  mutate(wk.pop = wkplace.pop.16.to.74 %>% replace_na(0),
         zone = OA)

##  merge and some might not be there but hmm
oa.sf <- 
  oa.sf %>%
  merge(wkp.df)

##  C) Now to calculate and save
dz11.access %>% summary
dz11.access <- access.measure(df.x = dz11.sf, df.y = oa.sf)
dz11.access %>% write.csv('Saved generated data/emp access dz11 lkp.csv')

##  England 2001 -----------
##  A) datazone (df.x)
lsoa01.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/LSOA 2001', sep = ''),
  layer = 'Lower_Layer_Super_Output_Areas_December_2001_Population_Weighted_Centroids') %>%
  st_transform(crs = st_crs(ukgrid))

lsoa01.sf <- cbind(lsoa01.sf %>% st_coordinates,
                   lsoa01.sf)
st_geometry(lsoa01.sf) <- NULL 

lsoa01.sf <- 
  lsoa01.sf %>%
  mutate(zone = lsoa01cd,
         x = X,
         y = Y)

##  B) work (df.y)
##  Centroids first
oa.sf <- 
  read_sf(dsn = google.drive.spatial %>% paste0('/OA 2001'),
          layer = 'Output_Areas_December_2001_Full_Clipped_Boundaries_in_England_and_Wales') %>%
  st_transform(ukgrid)

oa.sf <- cbind(oa.sf %>% st_centroid %>% st_coordinates, 
               oa.sf) #turn to centroids and get coords
st_geometry(oa.sf) <- NULL

oa.sf <- oa.sf %>%
  mutate(zone = oa01cd,
         x = X,
         y = Y) 

##  Attach work data
oa.wplace01 <- 
  google.drive.spatial %>%
  paste('/OA 2001/workplace oa 2001.csv', sep = '') %>%
  read.csv

wkp.df <- 
  oa.wplace01 %>%
  mutate(zone = substr(Area, 8 , 17),
         wk.pop = X2001 %>% as.numeric)

head(wkp.df) ; wkp.df[1000, ] ## okay so some codes are useless

##  merge
oa.sf <- 
  oa.sf %>%
  merge(wkp.df)

##  C) Now to calculate and save (it's huge so we have to break it down)
nrow(lsoa01.sf) ## so in 10k slices

lsoa01.access.pt1 <- access.measure(df.x = lsoa01.sf[1:10000, ], df.y = oa.sf)
lsoa01.access.pt2 <- access.measure(df.x = lsoa01.sf[10001:20000, ], df.y = oa.sf)
lsoa01.access.pt3 <- access.measure(df.x = lsoa01.sf[20001:nrow(lsoa01.sf), ], df.y = oa.sf)

lsoa01.access <- 
  rbind(lsoa01.access.pt1, lsoa01.access.pt2, lsoa01.access.pt3)

lsoa01.access %>% write.csv('Saved generated data/emp access lsoa01 lkp.csv')



##  England 2011  -----------
##  A) datazone (df.x)
lsoa11.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/LSOA 2011', sep = ''),
  layer = 'Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids') %>%
  st_transform(crs = st_crs(ukgrid))

lsoa11.sf <- cbind(lsoa11.sf %>% st_coordinates,
                   lsoa11.sf)
st_geometry(lsoa11.sf) <- NULL 

lsoa11.sf <- 
  lsoa11.sf %>%
  mutate(zone = lsoa11cd,
         x = X,
         y = Y)

##  B) work (df.y)
##  Centroids first
oa.sf <- 
  read_sf(dsn = google.drive.spatial %>% paste0('/OA 2011'),
          layer = 'Output_Area_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales') %>%
  st_transform(ukgrid)

oa.sf <- cbind(oa.sf %>% st_centroid %>% st_coordinates, 
               oa.sf) #turn to centroids and get coords
st_geometry(oa.sf) <- NULL

oa.sf <- oa.sf %>%
  mutate(zone = oa11cd,
         x = X,
         y = Y) 

##  Attach work data
oa.wplace11 <- 
  google.drive.spatial %>%
  paste('/OA 2011/workplace oa 2011.csv', sep = '') %>%
  read.csv

wkp.df <- 
  oa.wplace11 %>%
  mutate(zone = X2011.output.area,
         wk.pop = X2011 %>% as.numeric)

##  merge
oa.sf <- 
  oa.sf %>%
  merge(wkp.df)

##  C) Now to calculate and save (it's huge so we have to break it down)
nrow(lsoa11.sf) ## so in 10k slices

lsoa11.access.pt1 <- access.measure(df.x = lsoa11.sf[1:10000, ], df.y = oa.sf)
lsoa11.access.pt2 <- access.measure(df.x = lsoa11.sf[10001:20000, ], df.y = oa.sf)
lsoa11.access.pt3 <- access.measure(df.x = lsoa11.sf[20001:nrow(lsoa11.sf), ], df.y = oa.sf)

lsoa11.access <- 
  rbind(lsoa11.access.pt1, lsoa11.access.pt2, lsoa11.access.pt3)

lsoa11.access %>% write.csv('Saved generated data/emp access lsoa11 lkp.csv')

##  End script

