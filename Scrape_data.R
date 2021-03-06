# Date of most recent changes: 19.12.21
# Author: David Teuscher
# The script uses the baseballr package to scrape all Statcast pitch data
# for the 2021 season
####################################

# Load packages
library(baseballr)
library(tidyverse)

# Function used to pull in the data, loading the data 1 week at a time
# Code comes from a blog post by Bill Petti who developed the baseballr package
# https://billpetti.github.io/2021-04-02-build-statcast-database-rstats-version-3.0/

annual_statcast_query <- function(season) {
    
    dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                      as.Date(paste0(season, '-12-01')), by = 'week')
    
    date_grid <- tibble(start_date = dates, 
                        end_date = dates + 6)
    
    safe_savant <- safely(scrape_statcast_savant)
    
    payload <- map(.x = seq_along(date_grid$start_date), 
                   ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                       
                       payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                              end_date = date_grid$end_date[.x], type = 'pitcher')
                       
                       return(payload)
                   })
    
    payload_df <- map(payload, 'result')
    
    number_rows <- map_df(.x = seq_along(payload_df), 
                          ~{number_rows <- tibble(week = .x, 
                                                  number_rows = length(payload_df[[.x]]$game_date))}) %>%
        filter(number_rows > 0) %>%
        pull(week)
    
    payload_df_reduced <- payload_df[number_rows]
    
    combined <- payload_df_reduced %>%
        bind_rows()
    
    return(combined)
    
}

# Function to format some of the Statcast data
format_append_statcast <- function(df) {
    
    # function for appending new variables to the data set
    
    additional_info <- function(df) {
        
        # apply additional coding for custom variables
        
        df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                       ifelse(type == "X" & events == "double", 2,
                                              ifelse(type == "X" & events == "triple", 3, 
                                                     ifelse(type == "X" & events == "home_run", 4, NA)))))
        
        df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                                  ifelse(type == "X" & events == "double", 1,
                                         ifelse(type == "X" & events == "triple", 1, 
                                                ifelse(type == "X" & events == "home_run", 1, NA)))))
        
        df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
        
        df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
        
        df <- df %>%
            mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
        df <- df %>%
            mutate(spray_angle = round(
                (atan(
                    (hc_x-125.42)/(198.27-hc_y)
                )*180/pi*.75)
                ,1)
            )
        
        df <- df %>%
            filter(!is.na(game_year))
        
        return(df)
    }
    
    df <- df %>%
        additional_info()
    
    df$game_date <- as.character(df$game_date)
    
    df <- df %>%
        arrange(game_date)
    
    df <- df %>%
        filter(!is.na(game_date))
    
    df <- df %>%
        ungroup()
    
    df <- df %>%
        select(setdiff(names(.), c("error")))
    cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                           "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                           "fielder_8", "fielder_9")
    
    df <- df %>%
        mutate_at(.vars = cols_to_transform, as.numeric) %>%
        mutate_at(.vars = cols_to_transform, function(x) {
            ifelse(is.na(x), 999999999, x)
        })
    
    data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
    
    character_columns <- data_base_column_types %>%
        filter(class == "character") %>%
        pull(variable)
    
    numeric_columns <- data_base_column_types %>%
        filter(class == "numeric") %>%
        pull(variable)
    
    integer_columns <- data_base_column_types %>%
        filter(class == "integer") %>%
        pull(variable)
    
    df <- df %>%
        mutate_if(names(df) %in% character_columns, as.character) %>%
        mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
        mutate_if(names(df) %in% integer_columns, as.integer)
    
    return(df)
}

# Load the data
payload_statcast <- annual_statcast_query(2021)

# Format the data
df <- format_append_statcast(df = payload_statcast)    

# Write the data to .csv file, if desired
#write.csv(df, file = "statcast_2021.csv")