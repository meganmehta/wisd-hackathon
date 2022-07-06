library (StatsBombR)
library(tidyverse)
library(ggplot2)
library(grid)


Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
Matches = Matches %>% filter(competition.competition_name=="UEFA Euro")
data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)

events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)
events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)
events = events %>% left_join(data360, by = c("id" = "id"))

events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)

ffs = events %>%
  group_by(team.name) %>%
  filter(type.name=="Pass") %>%
  select(id, match_id, team.name, OpposingTeam, player.name, type.name, minute, second, location.x, location.y, pass.end_location.x, pass.end_location.y, pass.type.name, pass.cross, freeze_frame)

ffs = ffs %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))

crosses = ffs %>%
  filter(pass.end_location.x>102 & pass.end_location.y>18 & pass.end_location.y<62) %>%
  filter(is.na(pass.type.name) | pass.type.name=="Recovery" | pass.type.name=="Interception")%>%
  filter(pass.cross==TRUE) %>%
  filter(keeper==FALSE) %>%
  group_by(team.name, OpposingTeam, id) %>%
  summarise(attackers = sum(teammate==TRUE & ff_location.x>102 & ff_location.y>18 & ff_location.y<62, na.rm = TRUE),
            defenders = sum(teammate==FALSE & ff_location.x>102 & ff_location.y>18 & ff_location.y<62, na.rm = TRUE),
            att_n_def = attackers+defenders,
            att_v_def = attackers-defenders) %>%
  ungroup() %>%
  arrange(desc(att_n_def)) %>%
  head(n = 15)


chart = ffs %>%
  filter(id=="2b5bd40d-e4a5-41b3-9074-ca9e3fe4b646") %>%
  mutate(Player_Type_Key = case_when(actor==TRUE & teammate==TRUE ~ "Actor",
                                     teammate==TRUE ~ "Teammate",
                                     teammate==FALSE & keeper==FALSE ~ "Opponent",
                                     keeper==TRUE & teammate==FALSE ~ "Goalkeeper"))


library(ggplot2)
library(grid)
