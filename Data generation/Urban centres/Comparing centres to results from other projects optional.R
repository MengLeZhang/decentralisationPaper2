##  Let's perform on comparing the data that we used to our SLS work
##  This is to make sure thing are results consistent as the code changes
##  yet the overall method is the same
##  1/2/2019

##  Load in the data from 1) this current project and 2) the version we sent to
##  SLS
## 
ukgrid = "+init=epsg:27700" ## always remember the CRS

centres_sls_path <- 
  'C:/Users/Meng Le Zheng/Documents/SLS-postcode-data-linkage/Saved files for linkage/Locations of centres based landmark searching algo.csv'

centres_sls <- 
  read.csv(centres_sls_path)
centres_ui1 <- 
  read.csv('Saved generated data/Imputed centres based on osmdata for England and Scotland.csv')

centres_sls %>% head #both use landmarks but default to either pop weighted or local aci if none found
##  method 1 is local aci, method 2 is pop weighted
centres_ui1 %>% head

##  Test it
centres_merged <- 
  centres_sls %>%
  rename(name = Locality.Name) %>%
  left_join(centres_ui1)

## okay works
centres_merged %>% summary
centres_merged %>% head
centres_merged <- 
  centres_merged %>%
  mutate(distance.error = sqrt( 
    (easting_method2 - imputed_easting)^2 + (northing_method2 - imputed_northing)^2
    )
  )

centres_merged %>% summary # hmm minimal error except 1km out
centres_merged %>% 
  filter(distance.error > 0) %>% 
  arrange(pop11) %>%
  dplyr::select(name, distance.error, type_method2, imputed_type, pop11)
## Right so most are not over 1km-- oddly enough we end up with the same
##  type of landmark for Edinburgh and Glasgow but it's different points?
##  Could this be due to an issue with code or maybe osm changed the point of
##  centre

##  Nonetheless done
##  Note when this happened.

##  Let's tmap this
merged.sf <-
  centres_merged %>%
  st_as_sf(coords = c('imputed_easting', 'imputed_northing'),
           crs = ukgrid)

tmap_mode('view')
merged.sf %>% 
  filter(pop11 > 10000) %>%
  qtm
## Looks fine -- as expected since names are centred right