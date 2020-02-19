#############################################
#############################################
#Merging Candidates Speech Data
#############################################
#############################################

library(tidyverse)

#read in all seperate speech files (trump_dat, sanders_dat, etc.)


yang_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/yang_dat.rds"))
warren_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/warren_dat.rds"))
sanders_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/sanders_dat.rds"))
klobuchar_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/klobuchar_dat.rds"))
buttigieg_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/buttigieg_dat.rds"))
bloomberg_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/bloomberg_dat.rds"))
biden_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/biden_dat.rds"))
trump_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/trump_dat.rds"))
mcconnell_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/mcconnell_dat.rds"))
mccarthy_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/mccarthy_dat.rds"))
romney_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/romney_dat.rds"))
thune_dat <- readRDS(url("https://github.com/codymg/lab_work/master/data/thune_dat.rds"))

dat <- trump_dat %>%
  bind_rows(mcconnell_dat, mccarthy_dat, romney_dat, thune_dat, sanders_dat, warren_dat, 
            biden_dat, buttigieg_dat, klobuchar_dat, yang_dat, bloomberg_dat)
