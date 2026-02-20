library(gt)
library(dplyr)
library(gtExtras)

df <- read.csv('../results/xozpt.csv', check.names=FALSE)

teams <- read.csv('../results/teams.csv', check.names=FALSE)
players <- read.csv('../results/players.csv', check.names=FALSE)
players <- players[, c('player_id', 'player_name', 'position_group', 'primary_position')]


game_xozpt <- df %>%
  group_by(team_id, game_id) %>%
  summarise(xozpt = sum(xOZPT), ozptax = sum(OZPT_residual))

season_xozpt <- game_xozpt %>%
  group_by(team_id) %>%
  summarise(xozpt = mean(xozpt), ozptax = round(mean(ozptax),2))

game_xozptagainst <- df %>%
  group_by(opp_team_id, game_id) %>%
  summarise(xozpt = sum(xOZPT), ozptaax = sum(OZPT_residual))

season_xozptagainst <- game_xozptagainst %>%
  group_by(opp_team_id) %>%
  summarise(xozpt = mean(xozpt), ozptaax = round(mean(ozptaax),2)) %>%
  rename(team_id = opp_team_id)

player_game_xozpt <- df %>%
  group_by(player_id, game_id) %>%
  summarise(xozpt = sum(xOZPT), ozptax = sum(OZPT_residual))

player_season_xozpt <- player_game_xozpt %>%
  group_by(player_id) %>%
  summarise(gp=length(xozpt), xozpt = round(mean(xozpt),2), ozptax = round(mean(ozptax),2)) %>%
  filter(gp >= 10)

season_xozpt <- left_join(teams, season_xozpt, by='team_id')
season_xozptagainst <- left_join(teams, season_xozptagainst, by='team_id')
player_season_xozpt <- inner_join(players, player_season_xozpt, by='player_id')

top5_season_xozpt <- season_xozpt %>%
  arrange(desc(ozptax)) %>%
  select(c(-team_id, -xozpt)) %>%
  head(5) %>%
  gt() %>%
  data_color(
    columns = ozptax,
    colors = scales::col_numeric(
      palette = c("white", "royalblue4"), 
      domain = NULL)
  ) %>%
  gt_theme_espn()

bottom5_season_xozpt <- season_xozpt %>%
  arrange(ozptax) %>%
  select(c(-team_id, -xozpt)) %>%
  head(5) %>%
  arrange(desc(ozptax)) %>%
  gt() %>%
  data_color(
    columns = ozptax,
    colors = scales::col_numeric(
      palette = c("red4", "white"), 
      domain = NULL)
  ) %>%
  gt_theme_espn()
  
gt_two_column_layout(list(top5_season_xozpt, bottom5_season_xozpt))

top5_season_xozpta <- season_xozptagainst %>%
  arrange(ozptaax) %>%
  select(c(-team_id, -xozpt)) %>%
  head(5) %>%
  gt() %>%
  data_color(
    columns = ozptaax,
    colors = scales::col_numeric(
      palette = c("royalblue4", "white"), 
      domain = NULL)
  ) %>%
  gt_theme_espn()

bottom5_season_xozpta <- season_xozptagainst %>%
  arrange(desc(ozptaax)) %>%
  select(c(-team_id, -xozpt)) %>%
  head(5) %>%
  arrange(ozptaax) %>%
  gt() %>%
  data_color(
    columns = ozptaax,
    colors = scales::col_numeric(
      palette = c("white", "red4"), 
      domain = NULL)
  ) %>%
  gt_theme_espn()

gt_two_column_layout(list(top5_season_xozpta, bottom5_season_xozpta))


top5_fwd_season_xozpt <- player_season_xozpt %>%
  arrange(desc(xozpt)) %>%
  filter(position_group=='F') %>%
  select(c(-player_id, -ozptax, -position_group)) %>%
  head(10) %>%
  gt() %>%
  data_color(
    columns = xozpt,
    colors = scales::col_numeric(
      palette = c("white", "royalblue4"), 
      domain = NULL)
  ) %>%
  gt_theme_espn()

bottom5_fwd_season_xozpt <- player_season_xozpt %>%
  arrange(xozpt) %>%
  filter(position_group=='F') %>%
  select(c(-player_id, -ozptax, -position_group)) %>%
  head(10) %>%
  arrange(desc(xozpt)) %>%
  gt() %>%
  data_color(
    columns = xozpt,
    colors = scales::col_numeric(
      palette = c("red4", "white"), 
      domain = NULL)
  ) %>%
  gt_theme_espn()

gt_two_column_layout(list(top5_fwd_season_xozpt, bottom5_fwd_season_xozpt))

top5_d_season_xozpt <- player_season_xozpt %>%
  arrange(desc(xozpt)) %>%
  filter(position_group=='D') %>%
  select(c(-player_id, -ozptax, -position_group)) %>%
  head(10) %>%
  gt() %>%
  data_color(
    columns = xozpt,
    colors = scales::col_numeric(
      palette = c("white", "royalblue4"), 
      domain = NULL)
  ) %>%
  gt_theme_espn()

bottom5_d_season_xozpt <- player_season_xozpt %>%
  arrange(xozpt) %>%
  filter(position_group=='D') %>%
  select(c(-player_id, -ozptax, -position_group)) %>%
  head(10) %>%
  arrange(desc(xozpt)) %>%
  gt() %>%
  data_color(
    columns = xozpt,
    colors = scales::col_numeric(
      palette = c("red4", "white"), 
      domain = NULL)
  ) %>%
  gt_theme_espn()

gt_two_column_layout(list(top5_d_season_xozpt, bottom5_d_season_xozpt))
