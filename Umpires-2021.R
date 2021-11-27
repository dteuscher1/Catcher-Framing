library(baseballr)

umpires <- get_umpire_ids_petti() %>% filter(game_date >= "2021-04-01", position == "HP")
write_csv(umpires, "plate_umpires_2021.csv")
