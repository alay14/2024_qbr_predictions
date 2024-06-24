library(espnscrapeR)
library(gt)
library(gtExtras)
library(tidyverse)
library(cfbfastR)
Sys.setenv(CFBD_API_KEY = "") ######## ENTER YOUR OWN cfbfastR API KEY HERE #############

# Getting the data

qbr <- 2004:2023 %>% map(possibly(function(x) espnscrapeR::get_college_qbr(season = x, type = "season"))) %>% bind_rows()

rec <- 2004:2023 %>% map(possibly(function(x) cfbfastR::cfbd_recruiting_player(year = x))) %>% bind_rows()

teams <- cfbfastR::cfbd_team_info()

ratings <- rec %>% 
  filter(position %in% c("PRO","DUAL","QB")) %>%
  distinct(athlete_id,name,rating) %>%
  filter(name != "Michael Penix")

# 2023 starters determined by number of max snaps per team
starters23 <- qbr %>% filter(season == 2023) %>%   
  group_by(season,team_short_name) %>%
  filter(qb_plays == max(qb_plays)) %>% ungroup() %>%
  mutate(team_short_name = case_match(team_short_name,
                                      "OU" ~ "OKLA",
                                      "NCSU" ~ "NCST",
                                      "IU" ~ "IND",
                                      "NU" ~ "NW",
                                      # KANSAS AND PITT
                                      "MIA" ~ "MIAMI",
                                      .default = team_short_name
                      ))



proj24starters <- read.delim("24projected.txt",header = FALSE) %>%
  as_tibble() %>%
  mutate(team = str_split_i(V1,":",1),
         player = str_split_i(V1,": ",2),
         player = case_match(
           player,
           "Cam Ward" ~ "Cameron Ward",
           .default = player
         )) %>%
  select(team,player) 

proj24full <- starters23 %>% left_join(teams %>% select(school,abbreviation), by = c("team_short_name" = "abbreviation")) %>%
  left_join(proj24starters, by = c("school" = "team")) %>%
  filter(!is.na(player)) %>%
  mutate(season = 2024) %>%
  select(season,school,team_short_name,player) %>%
  left_join(ratings, by = c("player" = "name")) %>%
  mutate(rating = case_when(
    player == "Quinn Ewers" ~ 0.9999,
    player == "Cam Ward" ~ 0.72,
    player == "Dylan Raiola" ~ 0.9926,
    player == "Diego Pavia" ~ 0.76,
    player == "Cameron Ward" ~ 0.76,
    player == "Thomas Castellanos" ~ 0.8815,
    TRUE ~ rating
  )) %>%
  rename(name_display = player) %>%
  bind_rows(qbr %>% group_by(season,team_short_name) %>%
              filter(qb_plays == max(qb_plays)) %>%
              ungroup() %>%
              left_join(ratings %>% select(-athlete_id), by = c("name_display" = "name")) %>%
              left_join(ratings %>% select(-name), by = c("player_id" = "athlete_id")) %>%
              mutate(rating = ifelse(is.na(rating.x), rating.y,rating.x),
                     team_short_name = case_match(team_short_name,
                                                  "OU" ~ "OKLA",
                                                  "NCSU" ~ "NCST",
                                                  "IU" ~ "IND",
                                                  "NU" ~ "NW",
                                                  # KANSAS AND PITT
                                                  "MIA" ~ "MIAMI",
                                                  .default = team_short_name
                     )) %>%
              select(-rating.x,-rating.y)
  ) %>%
  arrange(season) %>%
  dplyr::group_by(name_display) %>%
  mutate(exp = as.factor(0:(n()-1)),
         player_lag1_qbr_total = lag(qbr_total,1),
         player_lag2_qbr_total = lag(qbr_total,2),
         player_lag3_qbr_total = lag(qbr_total,3),
         player_lag4_qbr_total = lag(qbr_total,4),
         player_lag_max_qbr_total = pmax(player_lag1_qbr_total,player_lag2_qbr_total,
                                         player_lag3_qbr_total,player_lag4_qbr_total,
                                         na.rm = T)) %>%
  group_by(team_short_name) %>%
  mutate(team_lag_qbr_total = lag(qbr_total),
         team_lag2_qbr_total = lag(qbr_total,2)) %>%
  ungroup() %>%
  mutate(avg_qbr = mean(qbr_total,na.rm = T),
         player_lag1_qbr_total = ifelse(is.na(player_lag1_qbr_total),
                                           (avg_qbr/2 + team_lag_qbr_total/2),
                                           player_lag1_qbr_total),
         ret_st = ifelse(team_lag_qbr_total == player_lag1_qbr_total,1,0))

# See who is missing
proj24full %>% filter(season > 2004,season < 2023) %>%
  filter(if_any(c(rating,team_lag_qbr_total), function(x) is.na(x))) %>% view()

summary(lm(qbr_total ~ team_lag_qbr_total + rating,
           data = (proj24full %>% filter(season < 2024, season > 2014,exp == 0))))

# Splitting data for random forest 
trn <- proj24full %>% filter(season < 2023) %>%
  select(qbr_total,team_lag_qbr_total,rating,exp,player_lag1_qbr_total,ret_st) %>%
  filter(across(c(rating,team_lag_qbr_total), function(x) !is.na(x)))

tst <- proj24full %>% filter(season == 2023) %>%
  select(season,team_short_name,name_display,qbr_total,team_lag_qbr_total,rating,exp,player_lag1_qbr_total,ret_st) %>%
  na.omit()

# Model for first year starters, experience = 0
yr0_lm <- lm(qbr_total ~ team_lag_qbr_total + rating,
             data = (trn %>% filter(exp == 0) %>% select(-exp)))

set.seed(44)

# Model for multi year starters, experience > 0
rf <- ranger::ranger(formula = qbr_total ~ team_lag_qbr_total + rating + player_lag1_qbr_total + ret_st,
               data = trn %>% filter(exp != 0) %>% na.omit(),
               num.trees = 2000,
               min.node.size = 15,
               sample.fraction = 0.8,
               importance = "impurity")

ranger::importance(rf)

rf

# Comparing results to null model of last year to predict this year

MLmetrics::RMSE(ranger::predictions(predict(rf,tst)),
                tst$qbr_total)

MLmetrics::RMSE(tst$team_lag_qbr_total,
               tst$qbr_total)


tst %>%
  mutate(exp_qbr = ranger::predictions(predict(rf,tst))) %>%
  ggplot(aes(exp_qbr, y = qbr_total)) +
  geom_abline() +
  geom_text(aes(label = name_display))


new <- proj24full %>% filter(season == 2024) %>%
  select(season,team_short_name,name_display,team_lag_qbr_total,rating,exp,player_lag1_qbr_total,ret_st) %>%
  na.omit() %>%
  mutate(exp_qbr = ranger::predictions(predict(rf,.)), # predictions from rf
         y0_exp_qbr = predict(yr0_lm,.), # predictions for 1st year starters
         fin_exp_qbr = ifelse(exp == 0,y0_exp_qbr,exp_qbr)) %>%
  left_join(teams %>% select(abbreviation,logo), by = c("team_short_name" = "abbreviation")) %>%
  arrange(desc(fin_exp_qbr)) %>%
  mutate(rank = 1:n())


new %>%
  slice_head(n = 13) %>%
  select(rank,logo,name_display,exp,fin_exp_qbr) %>%
  bind_cols(
    new %>%
      slice(14:26) %>%
      select(rank,logo,name_display,exp,fin_exp_qbr) %>%
      rename_with(~paste0(.x,"_2"))
  ) %>%
  gt() %>%
  tab_header("2024 CFB P4 QBR Projections",
             subtitle = "@alexlaycock4 | Data: espnscrapeR, cfbfastR | Insp: @statsowar") %>%
  gt_add_divider(columns = "fin_exp_qbr",
                 color = "black",
                 weight = px(5)) %>%
  fmt_image(
    columns = contains("logo")
  ) %>%
  data_color(
    columns = contains("qbr"),
    domain = c(25,97),
    palette = "PiYG",
  ) %>%
  cols_label(
    contains("rank") ~ "Rank",
    contains("logo") ~ "Team",
    contains("name") ~ "Player",
    contains("exp") ~ "Years as Starter",
    contains("qbr") ~ "Proj. QBR"
  ) %>%
  fmt_number(
    columns = contains("qbr"),
    decimals = 2
  ) %>%
  gt_theme_538() %>%
  cols_align(align = "center") %>%
  opt_align_table_header(align = "center") %>%
  gtsave_extra(filename = "qbr_projections_2024.png")
  

