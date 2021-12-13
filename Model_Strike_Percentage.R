library(tidyverse)
data_2021 <- read_csv("statcast_2021.csv")
umpires <- read_csv("plate_umpires_2021.csv")
catchers <- read_csv("catcher_ids.csv") 
caught_pitches <- data_2021 %>% 
    filter(description %in% c("ball", "called_strike")) %>% 
    filter(fielder_2 != 1000000000) %>%
    inner_join(umpires, by = 'game_pk') %>%
    inner_join(catchers, by = c("fielder_2" = 'key_mlbam')) %>%
    mutate(fielder_2 = factor(fielder_2))
    
plate_width <- 17 + 2 * (9/pi)
strike_zone <- ggplot(NULL, aes(x = plate_x, y = plate_z)) + 
    geom_rect(xmin = -(plate_width/2)/12,
              xmax = (plate_width/2)/12,
              ymin = 1.5,
              ymax = 3.5, color = "black", alpha = 0) + 
    coord_equal() + 
    scale_x_continuous("Horizontal pitch location (ft.)",
                       limits = c(-2, 2)) +
    scale_y_continuous("Vertical pitch location (ft.)",
                       limits = c(0, 5)) +
    theme_light() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          axis.line = element_line(color = 'black'))

strike_zone %+% sample_n(caught_pitches, 7000) + 
    aes(color = type) + 
    geom_point(alpha = .5) + 
    scale_color_manual("Pitch Result", 
                       values = c("darkgoldenrod", "royalblue4"), 
                       labels = c("Ball", "Strike"))

ggsave("pitches.jpg")



library(mgcv)
model_data <- sample_n(caught_pitches, 100000)
model <- gam(type == 'S' ~ s(plate_x, plate_z) + stand + p_throws, family = binomial, data = model_data)
model2 <- gam(type == 'S' ~ s(plate_x, plate_z), family = binomial, data = model_data)
summary(model)
anova(model2, model)
library(broom)
library(modelr)
hats <- model %>%
    augment(newdata = caught_pitches, type.predict = "response")

grid <- caught_pitches %>%
    data_grid(plate_x = seq(-2, 2, length.out = 25),
              plate_z = seq(0, 5, length.out = 25),
              p_throws, stand)
grid_hat <- model %>%
    augment(newdata = grid, type.predict = "response")

strike_zone %+% grid_hat +
    geom_tile(aes(fill = .fitted), alpha = 0.4) + 
    scale_fill_gradient("Strike Probability", low = "gray", high = "deepskyblue4")
ggsave("strike_prob.jpg")

strike_zone %+% grid_hat +
    geom_tile(aes(fill= .se.fit), alpha = 0.4) + 
    scale_fill_gradient("Standard Errors", low = "gray", high = "deepskyblue4")
ggsave("se_prob.jpg")


library(lme4)

mixed_1 <- glmer(type == "S" ~ .fitted  + (1|fielder_2) + (1|pitcher) + (1|name),
               data = hats, family = binomial, nAGQ = 0)

summary(mixed_1)

catchers <- read.csv("catcher_ids.csv") %>%
    mutate(key_mlbam = factor(key_mlbam))

umpires_total <- umpires %>%
    mutate(name = factor(name)) %>% 
    select(name) %>% unique()
random_effects <- random.effects(mixed_1) %>% data.frame()

catcher_large <- random_effects %>% filter(grpvar == "fielder_2") %>%
    inner_join(catchers, by = c("grp" = 'key_mlbam')) %>% arrange(desc(condval)) %>%
    head(10)

catcher_small <- random_effects %>% filter(grpvar == "fielder_2") %>%
    inner_join(catchers, by = c("grp" = 'key_mlbam')) %>% arrange(condval) %>%
    head(10)

umpire_large <- random_effects %>% filter(grpvar == "name") %>%
    inner_join(umpires_total, by = c("grp" = 'name')) %>% arrange(desc(condval)) %>%
    head(10)

umpire_small <- random_effects %>% filter(grpvar == "name") %>%
    inner_join(umpires_total, by = c("grp" = 'name')) %>% arrange(condval) %>%
    head(10)

umpire_results <- umpire_large %>% bind_rows(umpire_small) %>%
    select(grp, condval) %>%
    rename(Umpire = grp)

names(umpire_results) <- c("Umpire", "Random Effect")
knitr::kable(umpire_results)

catcher_results <- catcher_large %>% bind_rows(catcher_small) %>%
    select(grp, condval) %>%
    rename(Catcher = grp)

names(catcher_results) <- c("Catcher", "Random Effect")
knitr::kable(catcher_results)

    
