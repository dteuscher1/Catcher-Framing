# Date of most recent change: 19.12.21
# Author: David Teuscher
# The script pulls in the home plate umpires for each game in the 2021 season
#################################

# Load packages
library(baseballr)
library(tidyverse)

# Pull the umpires for each game and filter them for the 2021 regular season
# and for only the home plate umpire
umpires <- get_umpire_ids_petti() %>% 
    filter(game_date >= "2021-04-01", position == "HP")

# Write the data to a .csv file if desired
#write_csv(umpires, "plate_umpires_2021.csv")
