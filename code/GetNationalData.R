# ---------------------------------------------------------
# Prelims
# ---------------------------------------------------------
rm(list=ls())

library(rio)
library(tidyverse)
library(knitr)
library(haven)
library(lubridate)
library(zoo)
library(hrbrthemes)
library(gganimate)
library(openxlsx)

setwd("/Users/jeppeviero/Dropbox/03 Football/HistoricalFootballData")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
games <- rio::import("data/results.csv")

games <- games %>% 
  mutate(
    year = as.numeric(substr(date, start = 1, stop = 4))
  )


# ---------------------------------------------------------
# Expand data structure
# ---------------------------------------------------------

# ----- Create home team data
temp1 <- games %>% 
  mutate(
    cntry = home_team,
    opponent = away_team
  ) %>% 
  mutate(
    home = ifelse(cntry == country,
                  1,
                  0)
  ) %>% 
  mutate(
    result = case_when(
      home_score > away_score ~ "win",
      home_score == away_score ~ "draw",
      home_score < away_score ~ "defeat",
    )
  ) %>% 
  mutate(
    victory = ifelse(
      home_score > away_score,
      1,
      0
    ),
    draw = ifelse(
      home_score == away_score,
      1,
      0
    ),
    defeat = ifelse(
      home_score < away_score,
      1,
      0
    )
  ) %>% 
  rename(goals_for = home_score,
         goals_against = away_score) %>% 
  dplyr::select(date,
                year,
                cntry,
                opponent,
                goals_for,
                goals_against,
                result,
                tournament,
                home,
                victory,
                draw,
                defeat,
                city,
                country) %>% 
  mutate(match_id = row_number())

# ----- Create away team data
temp2 <- games %>% 
  mutate(
    cntry = away_team,
    opponent = home_team
  ) %>% 
  mutate(
    home = ifelse(cntry == country,
                  1,
                  0)
  ) %>% 
  mutate(
    result = case_when(
      home_score < away_score ~ "win",
      home_score == away_score ~ "draw",
      home_score > away_score ~ "defeat",
    )
  ) %>% 
  mutate(
    victory = ifelse(
      home_score < away_score,
      1,
      0
    ),
    draw = ifelse(
      home_score == away_score,
      1,
      0
    ),
    defeat = ifelse(
      home_score > away_score,
      1,
      0
    )
  ) %>% 
  rename(goals_for = away_score,
         goals_against = home_score) %>% 
  dplyr::select(date,
                year,
                cntry,
                opponent,
                goals_for,
                goals_against,
                result,
                tournament,
                home,
                victory,
                draw,
                defeat,
                city,
                country) %>% 
  mutate(match_id = row_number())

# ----- Gather in one dataset
games <- rbind(temp1,
               temp2) %>% 
  arrange(date, match_id)

rm(temp1, temp2)

# ----- Create date variable
games <- games %>% 
  rename(olddate = date)

games <- games %>% 
  mutate(date = ymd(olddate))

# ----- Clean data
games <- games %>% 
  dplyr::select(-c(olddate))

str(games)

games <- games %>% 
  dplyr::select(c(match_id,
                  year,
                  date,
                  cntry,
                  opponent,
                  tournament,
                  result,
                  goals_for,
                  goals_against,
                  victory,
                  draw,
                  defeat,
                  city,
                  country,
                  home))

games <- games %>% 
  rename(stadium_city = city,
         stadium_country = country)

# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
# ----- R
save(games,
     file = "data/IntMatches1872_2021.Rdata")

# ----- Excel
write.xlsx(games,
           file = "data/IntMatches1872_2021.xlsx")




