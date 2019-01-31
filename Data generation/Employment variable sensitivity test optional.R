##  Sensitivity test Optional
##  The whole purpose of this is not for the paper but to maintain internal
##  consistency; our Urban study paper measures lsoa 2001 data using employment
##  but we aggregated across LSOA to get employment counts instead of using raw oa
##  Just to test that the results are similar

library(tidyverse)
library(lme4)

##  Load in data
where.is.other.file <- 'file:///C:/Users/Meng Le Zheng/Documents/SMI-paper-1-RCI-/Working analysis files/Access to employment computed lkp.csv' 
# Location of the other emp file in another repo

access.LSOA <- where.is.other.file %>%
  read.csv
access.OA <- read.csv('Saved generated data/emp access lsoa01 lkp.csv') ## 

## Same rows so good start
access.LSOA %>% head
access.OA %>% head

##  Merge and test the data
access.OA %>% summary
test.df <-
  access.OA %>%
  left_join(access.LSOA %>% 
              rename(zone = lsoa01)) %>%
  dplyr::select(zone, accessA, accessB, access01, access01_exp)
summary(test.df) ## So basically we are comparing
## hmm very similar but the exp variab;e is not quite right

cor(test.df[, -1]) # corr is like 0.993 though so we should be satisfied
cor(test.df[, -1], method = 'spearman') # high rank cor
