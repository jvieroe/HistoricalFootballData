# ---------------------------------------------------------
# Prelims

# https://stackoverflow.com/questions/48006958/download-all-files-on-a-webpage-with-r
# https://stackoverflow.com/questions/33790052/download-all-files-from-a-folder-on-a-website
# https://stackoverflow.com/questions/39246739/download-multiple-files-using-download-file-function

# ---------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(magrittr)
library(janitor)
library(RCurl)
library(openxlsx)

setwd("/Users/jeppeviero/Dropbox/03 Football/HistoricalFootballData")

# ---------------------------------------------------------
# Get data
# ---------------------------------------------------------
# ----- Define seasons
# ----- consistent data from the 2005/2006 season
year_seq1 <- seq(5, 21, 1)
year_seq2 <- year_seq1 + 1

year_seq1 <- str_pad(year_seq1, 2, pad = "0")
year_seq2 <- str_pad(year_seq2, 2, pad = "0")

year_seq <- paste(year_seq1, year_seq2,
                  sep = "")

rm(year_seq1, year_seq2)

year_seq

year_seq <- c("9394",
              "9495",
              "9596",
              "9697",
              "9798",
              "9899",
              "9900",
              "0001",
              "0102",
              "0203",
              "0304",
              "0405",
              "0506",
              "0607",
              "0708",
              "0809",
              "0910",
              "1011",
              "1112",
              "1213",
              "1314",
              "1415",
              "1516",
              "1617",
              "1718",
              "1819",
              "1920",
              "2021",
              "2122")

year_seq

# ----- Define leagues
league_seq <- c('E0', # English Premier League
                'SC0', # Scottish Premier League
                'D1', # German Bundesliga
                'I1', # Italian Serie A
                'SP1', # Spanish La Liga
                'N1', # Dutch Eresdivisie
                'B1', # Belgian Jupiler League
                'P1', # Portuguese La Liga
                'T1' # Turkish Superliga
                #'G1' # Greek Ethniki Katigoria
)

# league_seq <- c('E0', # English Premier League
#                 'SC0' # Scottish Premier League
# )

league_seq

# ----- Download data
global_list <- list()

# outer loop: across leagues
for (i in seq_along(league_seq)) { 
  
  print(league_seq[i])
  
  ref_url <- league_seq[i]
  
  betting_list <- list()
  
  # inner loop: across seasons
  for (j in seq_along(year_seq)) {
    
    print(year_seq[j])
    
    url <- paste("https://www.football-data.co.uk/mmz4281/",
                 year_seq[j],
                 "/",
                 ref_url,
                 ".csv",
                 sep = "")
    
    url_exists <- RCurl::url.exists(url)
    
    if (url_exists == TRUE) {
      
      csv <- read_csv(url) %>% 
        as.data.frame() %>% 
        mutate_all(na_if,"") %>% 
        filter(!is.na(HomeTeam) & !is.na(AwayTeam))
      
      # ----- (see codebook: https://www.football-data.co.uk/notes.txt)
      csv <- csv %>% 
        select_if(names(.) %in% c(# ----- Game data -----
                                  'Div', 'Date', 'HomeTeam', 'AwayTeam',
                                  # Full-time scores
                                  'FTHG', 'FTAG',
                                  # Half-time scores
                                  'HTHG', 'HTAG',
                                  # Shots
                                  'HS', 'AS',
                                  # Shots on target
                                  'HST', 'AST',
                                  # Shots on woodwork
                                  'HHW', 'AHW',
                                  # Corners
                                  'HC', 'AC',
                                  # Fouls commited
                                  'HF', 'AF',
                                  'HFKC', 'AFKC',
                                  # Offsides
                                  'HO', 'AO',
                                  # Yellow cards
                                  'HY', 'AY',
                                  # Red cards
                                  'HR', 'AR',
                                  # Booking points
                                  'HBP', 'ABP',
                                  # ----- Betting data -----
                                  # number of BetBrain bookmakers used to calculate avg.
                                  'Bb1X2',
                                  # Betbrain maximum home win odds
                                  'BbMxH', 
                                  # Betbrain average home win odds
                                  'BbAvH',
                                  # Betbrain maximum draw odds
                                  'BbMxD',
                                  # Betbrain average draw win odds
                                  'BbAvD',
                                  # Betbrain maximum away win odds
                                  'BbMxA',
                                  # Betbrain average away win odds
                                  'BbAvA',
                                  # Market maximum home win odds,
                                  'MaxH',
                                  # Market maximum draw win odds
                                  'MaxD',
                                  # Market maximum away win odds
                                  'MaxA',
                                  # Market average home win odds
                                  'AvgH',
                                  # Market average draw win odds
                                  'AvgD',
                                  # Market average away win odds
                                  'AvgA'))
      
      csv <- csv %>% 
        mutate(season = year_seq[j])
      
      betting_list[[j]] <- csv
      
    }  else {
      
      #csv <- NA
      print("Season not available")
      
    }
    
    
    df <- bind_rows(betting_list)
    
  }
  
  global_list[[i]] <- df
  
}

warnings()

global_df <- bind_rows(global_list) %>% 
  janitor::clean_names()

any(is.na(global_df$season))
any(is.na(global_df$home_team))

global_df <- global_df %>% 
  mutate(result = case_when(fthg > ftag ~ "home",
                            fthg == ftag ~ "draw",
                            fthg < ftag ~ "away"))

global_df <- global_df %>% 
  mutate(home_win = ifelse(result == "home",
                           1,
                           0))

tabyl(global_df$home_win)


# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
# ----- R
save(global_df,
     file = "data/ClubData.Rdata")

# ----- Excel
write.xlsx(global_df,
           file = "data/ClubData.xlsx")


# q()

