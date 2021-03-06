library(tidyverse)
library(janitor)

path_db <- "https://raw.githubusercontent.com/lutz1402/MBSL/main/MBSL_Datenbanktest.csv"
path_fpts <- "https://raw.githubusercontent.com/christianlohr9/MBSL/main/preparation/fpts18-20.csv"
path_contract <- "https://raw.githubusercontent.com/christianlohr9/MBSL/main/preparation/Vertragszuordnung.csv"

players_raw <- read.csv2(path_db)
players_raw$BasisGehalt <- as.numeric(players_raw$BasisGehalt)
players_raw$Garantien <- as.numeric(players_raw$Garantien)  

players <- players_raw %>% 
  filter(Woche==0) %>% 
  group_by(PlayerID) %>% 
  mutate(contract=max(Jahr)-min(Jahr),extension=case_when(
    contract<=2 ~ Player, ####### hier die ID f�r die Extension rein. --> dann switch der NA_integer
    T ~ NA_character_
  )) %>% 
  select(Player,PlayerID,Jahr,GM,BasisGehalt,Garantien,Spot,contract,extension)

all_fpts <- read.csv2(path_fpts) %>% 
  select(-X,-player_id) %>% 
  mutate_at("fantasy_pts",as.numeric) %>% 
  pivot_wider(names_from = year, values_from = fantasy_pts, names_prefix = "fpts_")

players_fpts <- players %>%
  left_join(all_fpts, by = c("PlayerID" = "sleeper_id")) %>%
  mutate(base = BasisGehalt / 1000000,
         guarantee = Garantien / 1000000) %>%
  select(-BasisGehalt, -Garantien) %>%
  # filter(Jahr > 2020) %>%
  pivot_wider(
    names_from = Jahr,
    values_from = c(base, guarantee),
    names_repair = "check_unique"
  ) %>%
ungroup() %>%
  mutate(
    ppm = fpts_2020 / (base_2021*10),
    temp = case_when(
      is.na(fpts_2018) & is.na(fpts_2019) & is.na(fpts_2020) ~ NA_real_,
      is.na(fpts_2018) & is.na(fpts_2019) ~ fpts_2020,
      is.na(fpts_2018) & is.na(fpts_2020) ~ fpts_2019,
      is.na(fpts_2019) & is.na(fpts_2020) ~ fpts_2018,
      is.na(fpts_2018) ~ (fpts_2019 * 0.35) + (fpts_2020 * 0.65),
      is.na(fpts_2019) ~ (fpts_2018 * 0.25) + (fpts_2020 * 0.75),
      is.na(fpts_2020) ~ (fpts_2018 * 0.40) + (fpts_2019 * 0.60),
      T ~ (fpts_2018 * 0.10) + (fpts_2019 * 0.30) + (fpts_2020 * 0.60)
    ),
    fpts_contract = ceiling(round(ceiling(temp) / 10, digits = 1)) * 10
  )

contracts <- read.csv2(path_contract) %>% 
  mutate(extension_value = BasisGehalt/1000000) %>% 
  select(-Anteil.vom.Cap,-BasisGehalt)

players_fpts <- players_fpts %>% 
  left_join(contracts, by = c("fpts_contract"="unter.Fantasy.Punkte"))

# saveRDS(players_fpts,"Z:/GitHub/MBSL/data/players_shiny.rds") #private
saveRDS(players_fpts,"H:/GitHub/MBSL/data/players_shiny.rds") #work
