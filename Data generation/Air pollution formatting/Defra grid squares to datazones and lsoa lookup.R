##  Defra air pollution: We are going to repeat what Gavin has done but instead ----
##  using just pm 2.5 for the latest years.
##  The problem is that they have 1 by 1km grid data but not lookup. We must
##  sort this out. So first we simply have to load in one of the data files
##  Start: 11/07/2018
##  End product is a lookup table with 1) grid name, 
##  2) lsoa/dz its within (or nearest) and 3) zone type (dz11, lsoa01 etc)


##  Step 1: Read in one of the data files ----
source('UI paper 1 source.R')
library(nngeo) ## this is for later
ukgrid = "+init=epsg:27700" ## always remember the CRS for northing easting

grids <-
  google.drive.spatial %>% paste0('/Defra air pollution/mappm252004g.csv') %>%
  read.csv
str(grids) #So readings with x and y coordinates of the centroids in northing easting

##  Convert dataframe to sf

grids.sf <-
  st_as_sf(x = grids,
           coords = c('x', 'y'),
           crs = ukgrid)

##  Step 2: Define the routine to output the lookupfiles -----
##  Zones must match 
grid2zone <-
  function(grids.sf, zones.sf, zone.nm, zone.type) {
    ## Set same CRS for zone and grid (the defra file has no crs) and restrict to
    ##  grids within the whole range of zones
    
    if(!(st_crs(grids.sf) == st_crs(zones.sf))){
      print('CRS does not match')
      return(NULL)
    }
    
    ##  Now we need to find which grid centroids are within which polygons
    ##  This makes up the macthed results
    system.time(grid.match <-
                  st_within(x = grids.sf, y = zones.sf)) %>% print
    ## Just curious about time for st_within
    
    ## Create a vector which shows which grid squares are unmatched
    unmatched.id <-
      lapply(grid.match, function(x)
        length(x) == 0L) %>% unlist
    #grid.match[unmatched.id] <- NA #change elements with 0 lenght (i.e. no match) to NA # superfluous but useful to know
    
    
    matched_grid <-
      grids.sf$ukgridcode[!unmatched.id]
    matched_zone <- zones.sf[[zone.nm]]
    matched_zone <-
      matched_zone[grid.match %>% unlist]
    
    ## Next step. Get the unmatched zones using unmatched.id and find which grid
    ##  is closest to their centroid
    ##  This is the unmatched results
    
    
    missing.id <-
      !(zones.sf[[zone.nm]] %in% unique(matched_zone))
    missing.zone <- zones.sf[missing.id,]
    
    nearest.grid <-
      st_nn(missing.zone %>% st_centroid,
            grids.sf,
            k = 1,
            returnDist = F)
    nn.idx <- nearest.grid %>% unlist
    
    unmatched_grid <- grids.sf$ukgridcode[nn.idx]
    unmatched_zone <- missing.zone[[zone.nm]]
    
    ##  Final output
    lkp <- data.frame(
      ukgridcode = c(matched_grid, unmatched_grid),
      zone = c(matched_zone, unmatched_zone),
      type = zone.type
    )
    
    return(lkp)
  }

##  Step 3: Change the CRS of our grid file to continue ----
##  Output folder

##  A: Run for our data from lsoa 2001
lsoa2001.sf <-
  read_sf(dsn = google.drive.spatial %>% paste0('/LSOA 2001'),
          layer = 'Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales') %>%
  st_transform(crs = ukgrid)

grid2lsoa01 <- 
  grid2zone(grids.sf,
            lsoa2001.sf,
            'lsoa01cd',
            'lsoa01')

grid2lsoa01 %>% head
grid2lsoa01$zone %in% lsoa2001.sf$lsoa01cd %>% table #all good
rm(lsoa2001.sf) # get rid of the lsoa

##  B: Run for lsoa 2011
lsoa2011.sf <-
  read_sf(dsn = google.drive.spatial %>% paste0('/LSOA 2011'),
          layer = 'Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales')
lsoa2011.sf %>% head

grid2lsoa11 <- 
  grid2zone(grids.sf,
            lsoa2011.sf,
            'lsoa11cd',
            'lsoa11')

grid2lsoa11 %>% head
grid2lsoa11$zone %in% lsoa2011.sf$lsoa11cd %>% table #all good
rm(lsoa2011.sf)

## c: Run for datazones 2001
dz2001.sf <-
  read_sf(dsn = google.drive.spatial %>% paste0('/Scottish datazones/2001'),
          layer = 'SG_DataZone_Bdry_2001')
dz2001.sf

grid2dz01 <- 
  grid2zone(grids.sf,
            dz2001.sf,
            'DZ_CODE',
            'dz01')

grid2dz01 %>% head
grid2dz01$zone %in% dz2001.sf$DZ_CODE %>% table #all good
rm(dz2001.sf)

##  D: Run for datazones 2011
dz2011.sf <-
  read_sf(dsn = google.drive.spatial %>% paste0('/Scottish datazones/2011'),
          layer = 'SG_SIMD_2016')

grid2dz11 <- 
  grid2zone(grids.sf,
            dz2011.sf,
            'DataZone',
            'dz11')

## Note: This takes ages...

grid2dz11 %>% head
grid2dz11$zone %in% dz2011.sf$DataZone %>% table #all good

rm(dz2001.sf)

##  Step 4: Append all the result tables and save ----
grid2all <- rbind(grid2lsoa01,
                  grid2lsoa11,
                  grid2dz01,
                  grid2dz11)
grid2all %>% head
grid2all %>% write.csv('./Saved generated data/Defra grids to dz and lsoa lkp.csv',
                       row.names = F)

##  End
