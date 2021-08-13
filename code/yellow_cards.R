# ---------------------------------------------------------
# Prelims
# ---------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(magrittr)
library(janitor)
library(rio)

setwd("/Users/jeppeviero/Dropbox/03 Football/HistoricalFootballData")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
games <- rio::import("data/ClubData.Rdata")

# ---------------------------------------------------------
# -----
# ---------------------------------------------------------
games %>% 
  filter(home_team == "Celtic") %>% 
  select(div) %>% 
  head(10)

scotland <- games %>% 
  filter(div == "SC0") %>% 
  filter(season %in% c("2021", "2122"))

table(scotland$season)

